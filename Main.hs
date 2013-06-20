{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main
( main
)where

-- External dependencies imports

import ATerm.AbstractSyntax
import ATerm.Generics as G
import ATerm.ReadWrite
import ATerm.Unshared
import CATerms
import Codec.Archive.Zip
import Control.Applicative
import Control.Exception.Base
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.String
import Data.Tree
import Data.Tree.ATerm
import Data.Tree.AntiUnification
import Data.Tree.Types
import Data.Tree.Weaver
import Data.Tree.Yang
import Filesystem hiding (writeFile, readFile, withFile, openFile)
import Filesystem.Path hiding (concat, (<.>))
import JavaATerms
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax
import Prelude hiding (FilePath)
import Shelly hiding (FilePath, (</>),get,put)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (FilePath)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import qualified Filesystem as FS

-- Recommended by Shelly
default (LT.Text)

-----------------------------------------------------------------------
-- * Option Parsing
data Options = Options
  { optVerbose :: Bool
  , optSandbox :: FilePath
  , optMode    :: Mode
  }

data Mode
  = GenerateATerms
  | AntiUnifyATerms
  | Graphviz
  | Unparse
  | Weave
  deriving (Read, Show, Eq, Ord)

defaultOptions :: Options
defaultOptions = Options
  { optVerbose = False
  , optSandbox = "/tmp/dagit/version-patterns"
  , optMode    = GenerateATerms
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "s" ["sandbox"]
    (ReqArg
      (\arg opt -> return opt { optSandbox = fromString arg })
      "DIRECTORY")
    "Directory for storing or reading aterms"
  , Option "v" ["verbose"]
    (NoArg
      (\opt -> return opt { optVerbose = True }))
    "Enable verbose output"
  , Option "m" ["mode"]
    (ReqArg
      (\arg opt -> do
        let mode = fromMaybe (error "Unrecognized mode") (parseMode arg)
        return opt { optMode = mode })
      "MODE")
    "Mode of operation, MODE is one of: generate-aterms, antiunify-aterms, graphviz, unparse, weave"
  , Option "h" ["help"]
    (NoArg
      (\_ -> do
        prg <- getProgName
        hPutStrLn stderr (usageInfo prg options)
        exitWith ExitSuccess))
    "Show help"
  ]

parseMode :: String -> Maybe Mode
parseMode "generate-aterms"  = Just GenerateATerms
parseMode "antiunify-aterms" = Just AntiUnifyATerms
parseMode "graphviz"         = Just Graphviz
parseMode "unparse"          = Just Unparse
parseMode "weave"            = Just Weave
parseMode _                  = Nothing
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- * Git Diff
-- Ignore permissons on files:
-- drivers/char/mmtimer.c /tmp/IV5Cnd_mmtimer.c 58eddfdd3110a3a7168f2b8bdbfabefb9691016a 100644 /tmp/9W7Cnd_mmtimer.c 12006182f575a4f3cd09827bcaaea6523077e7b3 100644
data GitDiffArgs = GitDiffArgs
  { gdaFilePath       :: LT.Text
  , gdaBeforeCommit   :: LT.Text
  , gdaAfterCommit    :: LT.Text
  }
  deriving (Show, Read, Eq, Ord)

-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- * Main program
main :: IO ()
main = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let verbosity = if optVerbose opts then verbosely else silently
  shelly $ verbosity $ do
    case optMode opts of
      GenerateATerms  -> generateTerms (optSandbox opts)
      AntiUnifyATerms -> processTerms  (optSandbox opts)
      Graphviz        -> generateGraphs (optSandbox opts)
      Unparse         -> unparseTerms (optSandbox opts)
      Weave           -> weaveTerms (optSandbox opts)
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- * Heavy lifting

-- | generateTerms Assumes that the current working directory is a git repository.
-- It builds a list of pairs from the history of the repository. Visits each of those
-- commits and requests the arguments that git-diff would use between those versions.
-- Then it parses the source code that changed between each revision in the context it was
-- commited in. The parsed representations are saved as ATerms in a zip archive.
generateTerms :: FilePath -> Sh ()
generateTerms sandbox = do
  -- We add "master" so that if the repo is currently at a weird revision or branch, we get
  -- reasonable view of the history of master.
  cs <- LT.lines <$> run "git" ["log", "master", "--pretty=format:%H", "--reverse"]
  -- TODO: use the full history
  let pairs = {- take 10 -} (zip cs (drop 1 cs))
  -- Log the version pairs for later processing
  mkdir_p sandbox
  let initArchive    = versionPairs `addEntryToArchive` emptyArchive
      versionPairsFP = "version-pairs.hs" :: String
      versionPairs   = toEntry versionPairsFP 0 (BL.pack (show pairs))
      archiveFP      = LT.unpack (toTextIgnore (sandbox </> "version-patterns.zip"))
  writeFileInChunks archiveFP (fromArchive initArchive)

  flipFoldM_ pairs initArchive $ \archive' (first,second) -> do
    setenv "GIT_EXTERNAL_DIFF" "echo"
    diffLines <- LT.lines <$> run "git" ["diff", first, second]
    -- Git should always return the same information, eg., if this fails something has gone wrong
    -- TODO: what about multiple files? would that generate multiple lines?
    let parseDiffArgs :: [LT.Text] -> GitDiffArgs
        parseDiffArgs [fp,_,_,_,_,_,_] = GitDiffArgs { gdaFilePath     = fp
                                                     , gdaBeforeCommit = first
                                                     , gdaAfterCommit  = second }
        parseDiffArgs l | otherwise    = error ("unexpected output from git diff: " ++ LT.unpack (LT.concat l))

        diffArgs :: [GitDiffArgs]
        diffArgs = map parseDiffArgs (map LT.words diffLines)

        saveATerms :: Archive -> LT.Text -> [GitDiffArgs] -> Sh Archive
        saveATerms a commit gdas = do
          -- To properly parse files we need the context that the repository was in
          -- when the commit was made. So we do a checkout. We may also need to
          -- run configure or other commands. We have no support for that yet.
          void (run "git" ["checkout", commit])
          -------------------------------
          -- Hacks for configuring/building the linux kernel enough to make it
          -- parsable by ROSE
          -- just take the default config
          -- escaping False (run "yes \"\" | make oldconfig" [])
          -- setup the asm symlink
          -- run "make" ["include/asm"]
          -- End Hacks for the kernel
          -------------------------------
          -------------------------------
          -- Hacks for configuring/building freetype2 enough to make it
          -- parsable by ROSE
          -- void (run "./configure" []) `catchany_sh` (const (return ()))
          -- End Hacks for the kernel
          -------------------------------
          flipFoldM gdas a $ \archive gda -> do
            let commitDir   = fromText commit
                gdasFP      = LT.unpack (toTextIgnore (commitDir </> "gdas.hs"))
                gdasEntry   = toEntry gdasFP 0 (BL.pack (show gdas))
                gdasArchive = gdasEntry `addEntryToArchive` archive
            -- parse (gdaFilePath gda) using src2trm and save the result in destDir
            -- Only handle C source for now
            if fileFilter (gdaFilePath gda)
              then do
                liftIO (putStrLn ("running src2term for " ++ (LT.unpack (gdaFilePath gda))))
                let from    = fromText (gdaFilePath gda)
                    to      = commitDir </> (replaceExtension from "trm")
                catchany_sh
                  (do trm <- B.pack <$> writeSharedATerm <$> src2term from
                      let trmArchive = toEntry (LT.unpack (toTextIgnore to)) 0 (BL.fromChunks [trm]) `addEntryToArchive` gdasArchive
                      writeFileInChunks archiveFP (fromArchive trmArchive)
                      liftIO (putStrLn ("Wrote: " ++ LT.unpack (toTextIgnore to) ++ " in archive."))
                      liftIO (toArchive <$> (BL.readFile archiveFP)))
                  (\e -> do
                    liftIO (putStr "Error running src2term: ")
                    liftIO (putStrLn (show e))
                    return gdasArchive)
              else
                return gdasArchive

    -- Finally, save off the term files
    archive'' <- saveATerms archive' first  diffArgs
    saveATerms archive'' second diffArgs

-- | src2term runs the ROSE program by the same name over the input file,
-- saves the output to a temp file, reads it back in, deletes the temp file
-- and returns the string version.
-- NOTE: We can't just read it from stdout because ROSE dumps warnings there
-- and later we may not be able to parse it.
{-
src2term :: FilePath -> FilePath -> Sh (B.ByteString)
src2term from to = do
  -- TODO: remove this hardcode path
  -- TODO: take the include list and some of these other paths via command line
  -- or other configuration file
  -- For whatever reason, this gcc command gives us an extra newline that is
  -- troublesome, ergo this hack:
  let destFile = "/tmp" </> filename to
  void (run "/home/dagit/ftt/rose/compileTree/bin/src2term"
                [ "--aterm", toTextIgnore from, "-o", toTextIgnore destFile
                , "-Iinclude"
                , "-Iinclude/freetype"
                , "-Iinclude/freetype/config"
                , "-Iinclude/freetype/internal"
                , "-Isrc/base"
                , "-Isrc/shared"
                , "-Isrc/shared/otlayout"
                , "-Isrc/sfnt"
                , "-Isrc/psnames"
                , "-Isrc/type1z"
                , "-Isrc/otlayout"
                , "-Isrc/truetype"
                , "-Iconfig/unix"
                ])
  -- read the file in so we can add it to the archive
  bs <- liftIO (B.readFile (LT.unpack (toTextIgnore destFile)))
  rm destFile
  return bs
-}
src2term :: FilePath -> Sh ATermTable
src2term from = do
  cs <- liftIO (readFile (LT.unpack (toTextIgnore from)))
  case parser compilationUnit cs of
    Right prog -> return (toATermTable (toATerm prog))
    Left  e    -> fail (show e)

-- | processTerms looks in the version-patterns.zip archive
-- for version-pairs.hs. When it finds it, then it starts
-- looking for GitDiffArgs in gdas.hs in each commit directory.
-- When it reads GitDiffArgs it loads the two term representations
-- mentioned in the GitDiffArgs and computes the antiunification
-- of the two and adds that to the zip archive.
processTerms :: FilePath -> Sh ()
processTerms dir = do
  let archiveFP = LT.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(LT.Text,LT.Text)]
      initArchive = toArchive initArchiveBS
  case mb_ds of
    Just ds -> do
      liftIO (putStrLn ("length ds = " ++ show (length ds)))
      -- process each diff pair
      flipFoldM_ ds initArchive $ \archive' (commitBefore,commitAfter) -> do
        liftIO (putStrLn ("processing " ++ show (commitBefore,commitAfter)))
        let commitDir = fromText commitBefore
        liftIO (putStrLn ("Looking in " ++ show commitDir))
        -- Look for the GitDiffArgs stored in the current commit directory of the archive
        let gdasFilePath = commitDir </> "gdas.hs"
            mb_gdas      = readFromArchive archive' gdasFilePath :: Maybe [GitDiffArgs]
        case mb_gdas of
          Just gdas -> do
            liftIO (putStrLn ("Found gdas.hs"))
            -- For each GitDiffArg we will antiunify them separately
            flipFoldM gdas archive' $ \archive gda -> do
              catchany_sh
                -- Make sure we can process this file
                (if fileFilter (gdaFilePath gda)
                  then do
                    liftIO (putStrLn ("Antiunify using " ++ show gda))
                    let diffDir             = fromText (commitBefore `LT.append` ".." `LT.append` commitAfter)
                        antiunifiedFilePath = diffDir </> (replaceExtension (fromText (gdaFilePath gda)) "hs")
                        antiTerms           = antiUnifySh archive gda
                    case antiTerms of
                      -- Something went wrong
                      Left e          -> liftIO (putStrLn e) >> return archive
                      Right (t,s1,s2) -> do
                        let entry      = toEntry (LT.unpack (toTextIgnore antiunifiedFilePath)) 0 (BL.pack (show (t,s1,s2)))
                            newArchive = entry `addEntryToArchive` archive
                        liftIO (putStrLn ("Wrote antiunification to: " ++ (LT.unpack (toTextIgnore antiunifiedFilePath))))
                        writeFileInChunks archiveFP (fromArchive newArchive)
                        liftIO (putStrLn "Done writing archive.")
                        liftIO (toArchive <$> (BL.readFile archiveFP))
                  else return archive)
                -- Log the error and move on
                (\e -> do
                  liftIO (putStr "Error processingTerms: ")
                  liftIO (putStrLn (show e))
                  return archive)
          Nothing -> return archive'
    Nothing -> return ()

-- | antiUnifySh Looks at the GitDiffArgs and pulls two terms out of the archive
-- computes the antiunfication and returns the results
antiUnifySh :: Archive -> GitDiffArgs -> Either String (Term, Subs, Subs)
antiUnifySh archive gda = do
  let termBeforeFilePath = fromText (gdaBeforeCommit gda) </> replaceExtension (fromText (gdaFilePath gda)) "trm"
      termAfterFilePath  = fromText (gdaAfterCommit gda)  </> replaceExtension (fromText (gdaFilePath gda)) "trm"
      mb_tb              = findEntryByPath (LT.unpack (toTextIgnore termBeforeFilePath)) archive
      mb_ta              = findEntryByPath (LT.unpack (toTextIgnore termAfterFilePath))  archive
  case (mb_tb, mb_ta) of
    (Just tb, Just ta) ->
      let termToTree t = atermToTree (getATerm t) t
          termBefore   = replaceFileInfos (termToTree (readATerm (BL.unpack (fromEntry tb))))
          termAfter    = replaceFileInfos (termToTree (readATerm (BL.unpack (fromEntry ta))))
      in case (termBefore,termAfter) of
         (term1,term2) -> Right (term1 `antiunify` term2)
         _             -> Left "Filter return Nothing"
    _ -> Left "Failed to load terms"

generateGraphs :: FilePath -> Sh ()
generateGraphs dir = do
  let archiveFP = LT.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(LT.Text,LT.Text)]
      initArchive = toArchive initArchiveBS
      index       = filesInArchive initArchive
  case mb_ds of
    Nothing -> return ()
    Just ds -> do
      liftIO (putStrLn "just ds")
      forM_ ds $ \(commitBefore,commitAfter) -> do
        let diffDir = LT.unpack (commitBefore `LT.append` ".." `LT.append` commitAfter `LT.append` "/")
            inDir   = filter (diffDir `isPrefixOf`) index
            hs      = filter (".hs" `isSuffixOf`) inDir
        forM_ hs $ \h -> do
          liftIO (putStrLn h)
          let term = readFromArchive initArchive (fromText (LT.pack h)) :: Maybe (Term,Subs,Subs)
          case term of
            Nothing        -> return ()
            Just (t,s1,s2) -> do
              let destPath = "/tmp/dagit" </> directory (fromString h)
              mkdir_p destPath
              makeGraphFromSubs (destPath </> filename (fromString h) <.> "s1") s1
              makeGraphFromSubs (destPath </> filename (fromString h) <.> "s2") s2

makeGraphFromSubs :: FilePath -> Subs -> Sh ()
makeGraphFromSubs fp subs = do
  let ps  = M.assocs (extractMap subs)
      gs  = map (\(k,v) -> (extractName k, treeToGraphviz v)) ps
      cs  = map (\(k,v) -> (k,unlines v)) gs
      o k = (LT.unpack (toTextIgnore fp)) ++ "-" ++ k ++ ".gv"
  forM_ cs (\(k,v) -> liftIO (writeFile (o k) (concat ["digraph {\n",v,"}"])))
  where extractName (Node (LBLString n) _) = n
        extractMap (Subs t) = t

unparseTerms :: FilePath -> Sh ()
unparseTerms dir = do
  let archiveFP = LT.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(LT.Text,LT.Text)]
      initArchive = toArchive initArchiveBS
      index       = filesInArchive initArchive
  case mb_ds of
    Nothing -> return ()
    Just ds -> do
      liftIO (putStrLn "just ds")
      forM_ ds $ \(commitBefore,commitAfter) -> do
        let diffDir = LT.unpack (commitBefore `LT.append` ".." `LT.append` commitAfter `LT.append` "/")
            inDir   = filter (diffDir `isPrefixOf`) index
            hs      = filter (".hs" `isSuffixOf`) inDir
        forM_ hs $ \h -> do
          liftIO (putStrLn h)
          let term = readFromArchive initArchive (fromText (LT.pack h)) :: Maybe (Term,Subs,Subs)
          case term of
            Nothing        -> return ()
            Just (t,s1,s2) -> do
              let destPath = "/tmp/dagit" </> directory (fromString h)
              mkdir_p destPath
              term2src (destPath </> filename (fromString h) <.> "s1") s1 >>= liftIO . putStrLn 
              term2src (destPath </> filename (fromString h) <.> "s2") s2 >>= liftIO . putStrLn 

term2src :: FilePath -> Subs -> Sh String
term2src fp subs = do
  let ps = M.assocs (extractMap subs)
      gs = map (\(k,v) -> (extractName k, treeToATerm v)) ps
      cs = map (\(k,v) -> (k, getATermFull v)) gs
  return (show cs)
  where extractName (Node (LBLString n) _) = n
        extractMap (Subs t) = t

weaveTerms :: FilePath -> Sh ()
weaveTerms dir = do
  let archiveFP = LT.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds   = readFromArchive archive "version-pairs.hs" :: Maybe [(LT.Text,LT.Text)]
      archive = toArchive initArchiveBS
  case mb_ds of
    Just ds -> do
      liftIO (putStrLn ("length ds = " ++ show (length ds)))
      -- process each diff pair
      forM_ ds $ \(commitBefore,commitAfter) -> do
        liftIO (putStrLn ("processing " ++ show (commitBefore,commitAfter)))
        let commitDir = fromText commitBefore
        liftIO (putStrLn ("Looking in " ++ show commitDir))
        -- Look for the GitDiffArgs stored in the current commit directory of the archive
        let gdasFilePath = commitDir </> "gdas.hs"
            mb_gdas      = readFromArchive archive gdasFilePath :: Maybe [GitDiffArgs]
        case mb_gdas of
          Just gdas -> do
            liftIO (putStrLn ("Found gdas.hs"))
            -- For each GitDiffArg we will weave them separately
            forM_ gdas $ \gda -> do
              catchany_sh
                -- Make sure we can process this file
                (if fileFilter (gdaFilePath gda)
                  then do
                    liftIO (putStrLn ("weave using " ++ show gda))
                    let diffDir             = fromText (commitBefore `LT.append` ".." `LT.append` commitAfter)
                        weaveFilePath       = diffDir </> (replaceExtension (fromText (gdaFilePath gda)) "hs")
                        weave               = weaveSh archive gda
                    case weave of
                      -- Something went wrong
                      Left e  -> liftIO (putStrLn e)
                      Right w -> do
                         let entry      = toEntry (LT.unpack (toTextIgnore weaveFilePath)) 0 (BL.pack (show w))
                             destPath   = "/tmp/dagit" </> directory weaveFilePath
                             outfp      = LT.unpack (toTextIgnore (destPath </> (filename (replaceExtension weaveFilePath "gv"))))
                             gv         = concat ["digraph {\n",unlines (wtreeToGraphviz w),"}"]
                             newArchive = entry `addEntryToArchive` archive
                         mkdir_p destPath
                         liftIO (putStrLn ("Writing: " ++ outfp))
                         liftIO (writeFile outfp gv)
                         liftIO (putStrLn ("Wrote weave to: " ++ outfp))
                  else return ())
                -- Log the error and move on
                (\e -> do
                  liftIO (putStr "Error processingTerms: ")
                  liftIO (putStrLn (show e))
                  return ())
          Nothing -> return ()
    Nothing -> return ()

weaveSh archive gda = do
  let termBeforeFilePath = fromText (gdaBeforeCommit gda) </> replaceExtension (fromText (gdaFilePath gda)) "trm"
      termAfterFilePath  = fromText (gdaAfterCommit gda)  </> replaceExtension (fromText (gdaFilePath gda)) "trm"
      mb_tb              = findEntryByPath (LT.unpack (toTextIgnore termBeforeFilePath)) archive
      mb_ta              = findEntryByPath (LT.unpack (toTextIgnore termAfterFilePath))  archive
  case (mb_tb, mb_ta) of
    (Just tb, Just ta) ->
      let termToTree t = atermToTree (getATerm t) t
          termBefore   = termToTree (readATerm (BL.unpack (fromEntry tb)))
          termAfter    = termToTree (readATerm (BL.unpack (fromEntry ta)))
          (y1,y2)      = treediff termBefore termAfter (==)
          w            = weave y1 y2 False
      in Right w
    _ -> Left "Failed to load terms"

-----------------------------------------------------------------------


-----------------------------------------------------------------------
-- * Visualize Differences

wtreeToGraphviz :: WeaveTree a -- ^ Tree to print
                -> [String]    -- ^ DOT-file lines
wtreeToGraphviz t = snd $ evalIDGen t wToGV

wpLabel :: WeavePoint a -> String
wpLabel (Match _)      = "MATCH"
wpLabel (Mismatch _ _) = "MISMATCH"
wpLabel (RightHole _)  = "RHOLE"
wpLabel (LeftHole _)   = "LHOLE"

wpToGV :: WeavePoint a -> IDGen (Int, [String])
wpToGV wp = do
  myID <- genID
  let self = makeNode myID [cGreen] (wpLabel wp)
  case wp of
    Match t -> do (kidID, kidStrings) <- wToGV t
                  let kEdge = makeEdge myID kidID
                  return (myID, self:kEdge:kidStrings)
    Mismatch a b ->  do (kidID1, kidStrings1) <- wToGV a
                        (kidID2, kidStrings2) <- wToGV b
                        let kEdge1 = makeEdge myID kidID1
                            kEdge2 = makeEdge myID kidID2
                        return (myID, self:kEdge1:kEdge2:(kidStrings1++kidStrings2))
    LeftHole t -> do (kidID, kidStrings) <- wToGV t
                     let kEdge = makeEdge myID kidID
                     return (myID, self:kEdge:kidStrings)
    RightHole t -> do (kidID, kidStrings) <- wToGV t
                      let kEdge = makeEdge myID kidID
                      return (myID, self:kEdge:kidStrings)

wToGV :: WeaveTree a -> IDGen (Int, [String])
wToGV (WLeaf t) = do
  myID <- genID
  let self = makeNode myID [cGreen] "WLeaf"
  (kidID, kidStrings) <- tToGV t
  let kidEdge = makeEdge myID kidID
  return (myID, self:kidEdge:kidStrings)
wToGV (WNode lbl _ wps) = do
  myID <- genID
  let self = makeNode myID [cGreen] ("WNode:"++(gvShowLabel lbl))
  processed <- mapM wpToGV wps
  let (kIDs, kSs) = unzip processed
      kidEdges = map (makeEdge myID) kIDs
  return (myID, self:(kidEdges++(concat kSs)))

-- | This code is taken from the compose-hpc rulegen:
{-|
  Take a LabeledTree and return a list of lines for the
  corresponding graphviz DOT file.
-}
treeToGraphviz :: LabeledTree -- ^ Tree to print
               -> [String]    -- ^ DOT-file lines
treeToGraphviz t = snd $ evalIDGen t tToGV
--
-- node attributes for different node types
--
tToGV :: LabeledTree -> IDGen (Int, [String])
tToGV (Node label kids) = do
  myID <- genID
  let self = makeNode myID [cRed] (gvShowLabel label)
  processedKids <- mapM tToGV kids
  let (kidIDs, kidStrings) = unzip processedKids
      kidEdges = map (makeEdge myID) kidIDs
  return (myID, self:(kidEdges++(concat kidStrings)))

gvShowLabel :: Label -> String
gvShowLabel (LBLString s) = s
gvShowLabel (LBLList)     = "LIST"
gvShowLabel (LBLInt i)    = show i

-- use state monad for generating unique identifiers
type IDGen = State Int

-- generate sequence of unique ID numbers
genID :: IDGen Int
genID = do
  i <- get
  put (i+1)
  return i

-- generate variables with a fixed prefix and a unique suffix.  For
-- example, with prefix "x", a sequence of invocations of this
-- function will yield the names "x0", "x1", "x2", and so on.
genName :: String -> IDGen String
genName n = do
  i <- genID
  return $ n ++ (show i)

evalIDGen :: a -> (a -> IDGen b) -> b
evalIDGen x f = evalState (f x) 0

-- helper for common case with no attribute : avoid having to write Nothing
-- all over the place
makeEdge :: Int -> Int -> String
makeEdge i j = makeAttrEdge i j Nothing

-- node maker
makeNode :: Int -> [String] -> String -> String
makeNode i attrs lbl =
  "NODE"++(show i)++" ["++a++"];"
  where a = intercalate "," (("label=\""++(cleanlabel lbl)++"\""):attrs)

cGreen :: String
cGreen = "color=green"

cRed :: String
cRed   = "color=red"

cBlue :: String
cBlue  = "color=blue"

cBlack :: String
cBlack = "color=black"

aBold :: String
aBold  = "style=bold"
--
-- edge makers
--
makeAttrEdge :: Int -> Int -> Maybe [String] -> String
makeAttrEdge i j Nothing = "NODE"++(show i)++" -> NODE"++(show j)++";"
makeAttrEdge i j (Just as) = "NODE"++(show i)++" -> NODE"++(show j)++" ["++a++"];"
  where a = intercalate "," as

cleanlabel :: String -> String
cleanlabel lbl = filter (\c -> c /= '\\' && c /= '\'' && c /= '\"') lbl
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- * Utility Functions

-- | Lazily pulls a Readable value out of the requested file in the archive.
readFromArchive :: Read a => Archive -> FilePath -> Maybe a
readFromArchive archive fp =
  (read . BL.unpack . fromEntry) <$> findEntryByPath (LT.unpack (toTextIgnore fp)) archive

-- | Like the normal withFile except that internally it uses
-- 'shelly' and 'liftIO' to make the type Sh instead of IO.
withFileSh :: String -> IOMode -> (Handle -> Sh r) -> Sh r
withFileSh fp mode f =
  liftIO (bracket (openFile fp mode) hClose (\h -> shelly (f h)))

-- | We make an attempt to be somewhat atomic:
-- We throw ".tmp" onto the file extension, write to it in chunks
-- and when the chunks are written we move the file over top of
-- the requested file name.
writeFileInChunks :: String -> BL.ByteString -> Sh ()
writeFileInChunks fp bl = do
  withFileSh (fp++".tmp") WriteMode $ \h -> do
    forM_ (BL.toChunks bl) $ \b -> do
      liftIO (B.hPut h b)
  mv (fromText (LT.pack (fp++".tmp")))
     (fromText (LT.pack fp))

-- | We're mostly interested in C at the moment, so that means
-- .c and .h files.
fileFilter :: LT.Text -> Bool
fileFilter fp = (".java" `LT.isSuffixOf` fp)

flipFoldM_ :: Monad m => [b] -> a -> (a -> b -> m a) -> m ()
flipFoldM_ xs a f = foldM_ f a xs

flipFoldM :: Monad m => [b] -> a -> (a -> b -> m a) -> m a
flipFoldM xs a f = foldM f a xs

-- | Remove file_infos from the tree
filterFileInfos :: LabeledTree -> Maybe LabeledTree
filterFileInfos tree = removeSubtrees (LBLString "file_info") tree

-- | Homogenize the file_infos in the tree
replaceFileInfos :: LabeledTree -> LabeledTree
replaceFileInfos t = replaceSubtrees (LBLString "file_info")
                                     (Node (LBLString "file_info")
                                           [Node (LBLString "\"compilerGenerated\"") []
                                           ,Node (LBLInt    0)                       []
                                           ,Node (LBLInt    0)                       []])
                                     t
-----------------------------------------------------------------------
