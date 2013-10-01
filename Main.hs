{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ExtendedDefaultRules     #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

-- External dependencies imports

-- import CATerms
-- import Filesystem hiding (writeFile, readFile, withFile, openFile)
-- import Language.Java.Pretty
-- import Language.Java.Syntax
-- import qualified Filesystem as FS
import ATerm.AbstractSyntax
import ATerm.Generics as G
import ATerm.ReadWrite
import ATerm.Unshared
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
import Data.Vector (Vector)
import Filesystem.Path hiding (concat, (<.>), null)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import JavaATerms ()
import Language.Java.Parser
import Prelude hiding (FilePath)
import Shelly hiding (FilePath, (</>),get,put)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (FilePath)
import Text.CSV
import WeaveUtils
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Recommended by Shelly
default (T.Text)

-----------------------------------------------------------------------
-- * Option Parsing
data Options = Options
  { optVerbose   :: Bool
  , optSandbox   :: FilePath
  , optMode      :: Mode
  , optThreshold :: Double
  , optNumChanges:: Int 
  }

data Mode
  = GenerateATerms
  | AntiUnifyATerms
  | AntiUnifyGroup
  | Graphviz
  | Unparse
  | Weave
  | Similarity
  deriving (Read, Show, Eq, Ord)

defaultOptions :: Options
defaultOptions = Options
  { optVerbose    = False
  , optSandbox    = "/tmp/version-patterns"
  , optMode       = GenerateATerms
  , optThreshold  = 0
  , optNumChanges = 100
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "s" ["sandbox"]
    (ReqArg
      (\arg o -> return o { optSandbox = fromString arg })
      "DIRECTORY")
    "Directory for storing or reading aterms"
  , Option "v" ["verbose"]
    (NoArg
      (\o -> return o { optVerbose = True }))
    "Enable verbose output"
  , Option "m" ["mode"]
    (ReqArg
      (\arg o -> do
        let mode = fromMaybe (error "Unrecognized mode") (parseMode arg)
        return o { optMode = mode })
      "MODE")
    "Mode of operation, MODE is one of: generate-aterms, antiunify-aterms, antiunify-group, graphviz, similarity, unparse, weave"
  , Option "t" ["threshold"]
    (ReqArg
      (\arg o -> do
        let threshold = read arg :: Double
        threshold `seq` return o { optThreshold = threshold })
      "THRESHOLD")
    "Threshold for similarity, as a floating point value, in the range [0..1]"
  , Option "n" ["num"]
    (ReqArg
      (\arg o -> do
        let num = read arg :: Int
        num `seq` return o { optNumChanges = num })
      "NUM")
    "The number of changes to consume"
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
parseMode "antiunify-group"  = Just AntiUnifyGroup
parseMode "graphviz"         = Just Graphviz
parseMode "similarity"       = Just Similarity
parseMode "unparse"          = Just Unparse
parseMode "weave"            = Just Weave
parseMode _                  = Nothing
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- * Git Diff
-- Ignore permissons on files:
-- drivers/char/mmtimer.c /tmp/IV5Cnd_mmtimer.c 58eddfdd3110a3a7168f2b8bdbfabefb9691016a 100644 /tmp/9W7Cnd_mmtimer.c 12006182f575a4f3cd09827bcaaea6523077e7b3 100644
data GitDiffArgs = GitDiffArgs
  { gdaFilePath       :: T.Text
  , gdaBeforeCommit   :: T.Text
  , gdaAfterCommit    :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- * Main program
main :: IO ()
main = do
  args <- getArgs
  let (actions, rest, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let verbosity = if optVerbose opts then verbosely else silently
  shelly $ verbosity $ do
    case optMode opts of
      GenerateATerms  -> generateTerms (optSandbox opts)
      AntiUnifyATerms -> processTerms  (optSandbox opts)
      AntiUnifyGroup  -> antiUnifyTerms (optSandbox opts) "antiunify.gv" (map read rest)
      Graphviz        -> generateGraphs (optSandbox opts)
      Similarity      -> similarTrees (optSandbox opts) (optThreshold opts)
      Unparse         -> unparseTerms (optSandbox opts)
      Weave           -> weaveTerms (optSandbox opts) (optNumChanges opts)
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
  cs <- T.lines <$> run "git" ["log", "master", "--pretty=format:%H", "--reverse"]
  -- TODO: use the full history
  let pairs = {- take 10 -} (zip cs (drop 1 cs))
  -- Log the version pairs for later processing
  mkdir_p sandbox
  let initArchive    = versionPairs `addEntryToArchive` emptyArchive
      versionPairsFP = "version-pairs.hs" :: String
      versionPairs   = toEntry versionPairsFP 0 (BL.pack (show pairs))
      archiveFP      = T.unpack (toTextIgnore (sandbox </> "version-patterns.zip"))
  writeFileInChunks archiveFP (fromArchive initArchive)

  flipFoldM_ pairs initArchive $ \archive' (first,second) -> do
    setenv "GIT_EXTERNAL_DIFF" "echo"
    diffLines <- T.lines <$> run "git" ["diff", first, second]
    -- Git should always return the same information, eg., if this fails something has gone wrong
    -- TODO: what about multiple files? would that generate multiple lines?
    let parseDiffArgs :: [T.Text] -> GitDiffArgs
        parseDiffArgs [fp,_,_,_,_,_,_] = GitDiffArgs { gdaFilePath     = fp
                                                     , gdaBeforeCommit = first
                                                     , gdaAfterCommit  = second }
        parseDiffArgs l | otherwise    = error ("unexpected output from git diff: " ++ T.unpack (T.concat l))

        diffArgs :: [GitDiffArgs]
        diffArgs = map parseDiffArgs (map T.words diffLines)

        saveATerms :: Archive -> T.Text -> [GitDiffArgs] -> Sh Archive
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
                gdasFP      = T.unpack (toTextIgnore (commitDir </> "gdas.hs"))
                gdasEntry   = toEntry gdasFP 0 (BL.pack (show gdas))
                gdasArchive = gdasEntry `addEntryToArchive` archive
            -- parse (gdaFilePath gda) using src2trm and save the result in destDir
            -- Only handle C source for now
            if fileFilter (gdaFilePath gda)
              then do
                liftIO (putStrLn ("running src2term for " ++ (T.unpack (gdaFilePath gda))))
                let from    = fromText (gdaFilePath gda)
                    to      = commitDir </> (replaceExtension from "trm")
                catchany_sh
                  (do trm <- B.pack <$> writeSharedATerm <$> src2term from
                      let trmArchive = toEntry (T.unpack (toTextIgnore to)) 0 (BL.fromChunks [trm]) `addEntryToArchive` gdasArchive
                      writeFileInChunks archiveFP (fromArchive trmArchive)
                      liftIO (putStrLn ("Wrote: " ++ T.unpack (toTextIgnore to) ++ " in archive."))
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

src2term :: FilePath -> Sh ATermTable
src2term from = do
  cs <- liftIO (readFile (T.unpack (toTextIgnore from)))
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
  let archiveFP = T.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(T.Text,T.Text)]
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
                    let diffDir             = fromText (commitBefore `T.append` ".." `T.append` commitAfter)
                        antiunifiedFilePath = diffDir </> (replaceExtension (fromText (gdaFilePath gda)) "hs")
                        antiTerms           = antiUnifySh archive gda
                    case antiTerms of
                      -- Something went wrong
                      Left e          -> liftIO (putStrLn e) >> return archive
                      Right (t,s1,s2) -> do
                        let entry      = toEntry (T.unpack (toTextIgnore antiunifiedFilePath)) 0 (BL.pack (show (t,s1,s2)))
                            newArchive = entry `addEntryToArchive` archive
                        liftIO (putStrLn ("Wrote antiunification to: " ++ (T.unpack (toTextIgnore antiunifiedFilePath))))
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
  let termBeforeFilePath = fromText (gdaBeforeCommit gda) </>
                             replaceExtension (fromText (gdaFilePath gda)) "trm"
      termAfterFilePath  = fromText (gdaAfterCommit gda)  </>
                             replaceExtension (fromText (gdaFilePath gda)) "trm"
      mb_tb              = findEntryByPath (T.unpack (toTextIgnore termBeforeFilePath)) archive
      mb_ta              = findEntryByPath (T.unpack (toTextIgnore termAfterFilePath))  archive
  case (mb_tb, mb_ta) of
    (Just tb, Just ta) ->
      let termToTree t = atermToTree (getATerm t) t
          termBefore   = replaceFileInfos (termToTree (readATerm (BL.unpack (fromEntry tb))))
          termAfter    = replaceFileInfos (termToTree (readATerm (BL.unpack (fromEntry ta))))
      in Right (termBefore `antiunify` termAfter)
    _ -> Left "Failed to load terms"

-- |Find all terms whose filename ends in one of the ids passed in
-- eg., find . -name '*-id.trm', where id is one of the passed in ints
antiUnifyTerms :: FilePath -> String -> [Int] -> Sh ()
antiUnifyTerms dir fname termIds = do
  liftIO (putStrLn ("antiUnifyTerms: " ++ unwords (map  show termIds)))
  -- findWhen :: (FilePath -> Sh Bool) -> FilePath -> Sh [FilePath]
  fs <- findWhen match (dir </> fromString "weaves")
  ts <- loadTerms fs
  if length ts < 1 then error "not enough terms to antiunify" else return ()
  let (t,_)  = fromJust (antiunifyList ts)
      gv     = (concat ["digraph {\n",unlines (treeToGraphviz t),"}"])
  liftIO (writeFile fname gv)
  where
  match :: FilePath -> Sh Bool
  match fp = return (or [("-" ++ show i ++ ".trm") `isSuffixOf` T.unpack (toTextIgnore fp) | i <- termIds])

  loadTerms :: [FilePath] -> Sh [Term]
  loadTerms = mapM loadTerm

  loadTerm :: FilePath -> Sh Term
  loadTerm fp = do
    cs <- liftIO (readFile (T.unpack (toTextIgnore fp)))
    let t = readATerm cs
    length cs `seq` return (atermToTree (getATerm t) t)

generateGraphs :: FilePath -> Sh ()
generateGraphs dir = do
  let archiveFP = T.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(T.Text,T.Text)]
      initArchive = toArchive initArchiveBS
      index       = filesInArchive initArchive
  case mb_ds of
    Nothing -> return ()
    Just ds -> do
      liftIO (putStrLn "just ds")
      forM_ ds $ \(commitBefore,commitAfter) -> do
        let diffDir = T.unpack (commitBefore `T.append` ".." `T.append` commitAfter `T.append` "/")
            inDir   = filter (diffDir `isPrefixOf`) index
            hs      = filter (".hs" `isSuffixOf`) inDir
        forM_ hs $ \h -> do
          liftIO (putStrLn h)
          let term = readFromArchive initArchive (fromText (T.pack h)) :: Maybe (Term,Subs,Subs)
          case term of
            Nothing        -> return ()
            Just (_,s1,s2) -> do
              let destPath = "/tmp/dagit" </> directory (fromString h)
              mkdir_p destPath
              makeGraphFromSubs (destPath </> filename (fromString h) <.> "s1") s1
              makeGraphFromSubs (destPath </> filename (fromString h) <.> "s2") s2

makeGraphFromSubs :: FilePath -> Subs -> Sh ()
makeGraphFromSubs fp subs = do
  let ps  = M.assocs (extractMap subs)
      gs  = map (\(k,v) -> (extractName k, treeToGraphviz v)) ps
      cs  = map (\(k,v) -> (k,unlines v)) gs
      o k = (T.unpack (toTextIgnore fp)) ++ "-" ++ k ++ ".gv"
  forM_ cs (\(k,v) -> liftIO (writeFile (o k) (concat ["digraph {\n",v,"}"])))
  where extractName (Node (LBLString n) _) = n
        extractName _                      = ""
        extractMap (Subs t) = t

unparseTerms :: FilePath -> Sh ()
unparseTerms dir = do
  let archiveFP = T.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(T.Text,T.Text)]
      initArchive = toArchive initArchiveBS
      index       = filesInArchive initArchive
  case mb_ds of
    Nothing -> return ()
    Just ds -> do
      liftIO (putStrLn "just ds")
      forM_ ds $ \(commitBefore,commitAfter) -> do
        let diffDir = T.unpack (commitBefore `T.append` ".." `T.append` commitAfter `T.append` "/")
            inDir   = filter (diffDir `isPrefixOf`) index
            hs      = filter (".hs" `isSuffixOf`) inDir
        forM_ hs $ \h -> do
          liftIO (putStrLn h)
          let term = readFromArchive initArchive (fromText (T.pack h)) :: Maybe (Term,Subs,Subs)
          case term of
            Nothing        -> return ()
            Just (_,s1,s2) -> do
              let destPath = "/tmp/dagit" </> directory (fromString h)
              mkdir_p destPath
              term2src s1 >>= liftIO . putStrLn 
              term2src s2 >>= liftIO . putStrLn 

term2src :: Subs -> Sh String
term2src subs = do
  let ps = M.assocs (extractMap subs)
      gs = map (\(k,v) -> (extractName k, treeToATerm v)) ps
      cs = map (\(k,v) -> (k, getATermFull v)) gs
  return (show cs)
  where extractName (Node (LBLString n) _) = n
        extractName _                      = ""
        extractMap (Subs t) = t

weaveTerms :: FilePath -> Int -> Sh ()
weaveTerms dir num = do
  let archiveFP = T.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  initArchiveBS <- liftIO (BL.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds   = readFromArchive archive "version-pairs.hs" :: Maybe [(T.Text,T.Text)]
      archive = toArchive initArchiveBS
  case mb_ds of
    Just ds -> do
      liftIO (putStrLn ("length ds = " ++ show (length ds)))
      -- process each diff pair
      (_,ps,ts) <- flipFoldM (take num ds) (0,[],[]) $ \(count,ps,ts) (commitBefore,commitAfter) -> do
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
            flipFoldM gdas (count,ps,ts) $ \(prevCount,prevPs,prevTs) gda -> do
              catchany_sh
                -- Make sure we can process this file
                (if fileFilter (gdaFilePath gda)
                  then do
                    liftIO (putStrLn ("weave using " ++ show gda))
                    let diffDir             = fromText (commitBefore `T.append` ".." `T.append` commitAfter)
                        weaveFilePath       = diffDir </> (replaceExtension (fromText (gdaFilePath gda)) "hs")
                        woven               = weaveSh archive gda
                    case woven of
                      -- Something went wrong
                      Left  e -> liftIO (putStrLn e) >> return (prevCount,prevPs,prevTs)
                      Right w -> do
                         let destPath  = "weaves" </> directory weaveFilePath
                             outfp     = destPath </> (filename (replaceExtension weaveFilePath "gv"))
                             outgvfps  = [directory outfp </>
                                           (fromText (toTextIgnore (basename outfp) `T.append`
                                           "-" `T.append` T.pack (show x))) <.> "gv"
                                         | x <- [(prevCount::Int)..]]
                             outtrmfps = [directory outfp </>
                                           (fromText (toTextIgnore (basename outfp) `T.append`
                                           "-" `T.append` T.pack (show x))) <.> "trm"
                                         | x <- [(prevCount::Int)..]]
                             mkGV l    = concat ["digraph {\n", unlines l,"}"]
                             gvs       = map (mkGV . eTreeToGraphviz) ws'
                             ws'       = extract2 w
                             terms     = map treeToATerm (extract w) 
                             ts'       = map treeType ws'
                             ps'       = let ws = extract w
                                         in zip ws (map (size . toSizedTree) ws)
                         if length ps' /= length ts' then error "ps' /= ts'" else return ()
                         mkdir_p destPath
                         liftIO (forM_ (zip (map (T.unpack . toTextIgnore) outgvfps) gvs)
                                       (\(fp,gv) -> do
                                         putStrLn ("Writing: " ++ fp)
                                         writeFile fp gv
                                         putStrLn ("Wrote graphviz of weave to: " ++ fp)))
                         liftIO (forM_ (zip (map (T.unpack . toTextIgnore) outtrmfps) terms)
                                       (\(fp,term) -> do
                                         putStrLn ("Writing: " ++ fp)
                                         writeFile fp (writeSharedATerm term)
                                         putStrLn ("Wrote aterm of weave to: " ++ fp)))
                         return (prevCount+length ps',prevPs ++ ps',prevTs++ts')
                  else return (prevCount,prevPs,prevTs))
                -- Log the error and move on
                (\e -> do
                  liftIO (putStr "Error processingTerms: ")
                  liftIO (putStrLn (show e))
                  return (prevCount,prevPs,prevTs))
          Nothing -> return (0,[],[])
      if length ps /= length ts then error "ps /= ts" else return ()
      let dists :: MismatchType -> [((LabeledTree,Int),Maybe MismatchType)] -> [Double]
          dists ty xs = [ let -- score = fromIntegral (treedist t1 t2 (==))/fromIntegral(max s1 s2)
                              -- score = fromIntegral (treedist t1 t2 (==))/fromIntegral s1
                              scorel = fromIntegral (treedist t1 t2 (==))
                              scorer = fromIntegral (treedist t2 t1 (==))
                              -- score  = min (scorel / fromIntegral s1)
                              --              (scorer / fromIntegral s2)
                              score = min scorel scorer / fromIntegral (max s1 s2)
                          in if ty1 == ty2 && ty1 == Just ty
                               then score
                               else 0
                       | ((t1,s1),ty1) <- xs, ((t2,s2),ty2) <- xs ]
          csvShow xs  = unlines (map csvShow' xs)
            where
            csvShow' ys = intercalate "," (map show ys)
          pairs     = zip ps ts
          chunkSize = length pairs
          forestBefore = dists MismatchTypeLeft  pairs
          forestAfter  = dists MismatchTypeRight pairs
          forestDelete = dists RightHoleType     pairs
          forestAdd    = dists LeftHoleType      pairs

      liftIO (writeFile "treetypes.csv"      (intercalate "," (map (show.fromEnum') ts)))
      liftIO (writeFile "treesimilarity-before.csv" (csvShow (chunk chunkSize forestBefore)))
      liftIO (writeFile "treesimilarity-after.csv"  (csvShow (chunk chunkSize forestAfter)))
      liftIO (writeFile "treesimilarity-delete.csv" (csvShow (chunk chunkSize forestDelete)))
      liftIO (writeFile "treesimilarity-add.csv"    (csvShow (chunk chunkSize forestAdd)))
      -- liftIO (writeFile "treesimilarity-all.csv"    (csvShow (chunk (length ps) (dists ps))))
    Nothing -> return ()

weaveSh :: IsString a => Archive -> GitDiffArgs -> Either a (WeaveTree Bool)
weaveSh archive gda = do
  let termBeforeFilePath = fromText (gdaBeforeCommit gda) </> replaceExtension (fromText (gdaFilePath gda)) "trm"
      termAfterFilePath  = fromText (gdaAfterCommit gda)  </> replaceExtension (fromText (gdaFilePath gda)) "trm"
      mb_tb              = findEntryByPath (T.unpack (toTextIgnore termBeforeFilePath)) archive
      mb_ta              = findEntryByPath (T.unpack (toTextIgnore termAfterFilePath))  archive
  case (mb_tb, mb_ta) of
    (Just tb, Just ta) ->
      let termToTree t = atermToTree (getATerm t) t
          termBefore   = termToTree (readATerm (BL.unpack (fromEntry tb)))
          termAfter    = termToTree (readATerm (BL.unpack (fromEntry ta)))
          (y1,y2)      = treediff termBefore termAfter (==)
          w            = weave y1 y2 False
      in Right w
    _ -> Left "Failed to load terms"


similarTrees :: FilePath -> Double -> Sh ()
similarTrees dir thres = do
  let fps  = map (\nm -> T.unpack (toTextIgnore (dir </> fromString ("treesimilarity-" ++ nm ++ ".csv")))) alts
      alts = ["before","after","delete","add"]
  -- d3 <- liftIO (modexp d2 sz sz)

  liftIO (putStrLn ("Similarity threshold: " ++ show thres))

  similars <- zip alts <$> mapM load fps
  mapM_ dumpGV similars

  where
  dumpGV (prefix,m) = forM_ (M.toAscList m) $ \(k,is) -> do
    let set = S.toAscList is
    antiUnifyTerms dir ("antiunify-" ++ prefix ++ "-" ++ show k ++ ".gv") set
  -- horribly inefficient, but for some reason the csv parser
  -- treats the final newline in a file as a record. So,
  -- we strip it out.
  dropTrailingNewline [] = []
  dropTrailingNewline xs | last xs == '\n' = init xs
                         | otherwise       = xs

  load fp = do
    cs <- liftIO (readFile fp)
    let csvDat = case parseCSV fp (dropTrailingNewline cs) of
                 Right c -> map read <$> c :: [[Double]]
                 Left  e -> error (show e)
        d      = V.fromList (concat csvDat)
        d2     = (>= thres) <$> d
        sz     = length csvDat
    return (similarityMatrix d2 sz)

  similarityMatrix m sz = similar
    where
    sm = chunk sz (V.toList m)
    filterSimilar :: M.Map Int (S.Set Int) -> M.Map Int (S.Set Int)
    filterSimilar m = M.fromList $ case mapAccumL go S.empty (M.toList m) of
                                   (_, ys) -> catMaybes ys
      where
      go :: S.Set Int -> (Int, S.Set Int) -> (S.Set Int, Maybe (Int,S.Set Int))
      go seen (i,rs) = (seen', irs)
        where
        seen' = S.singleton i `S.union` seen `S.union` rs
        irs   = if i `S.notMember` seen then Just (i,rs) else Nothing

    similar :: M.Map Int (S.Set Int)
    similar = filterSimilar $ M.fromList $ do
      (i,r) <- zip [0..] sm
      let js = elemIndices True r
      guard (not (null js))
      return (i,S.fromList js)
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
  (kidID, kidStrings) <- tToGV gvShowLabel t
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
treeToGraphviz t = snd $ evalIDGen t (tToGV gvShowLabel)
--
-- node attributes for different node types
--
tToGV :: (a -> String) -> Tree a -> IDGen (Int, [String])
tToGV showIt (Node label kids) = do
  myID <- genID
  --let self = makeNode myID [cRed] (gvShowLabel label)
  let self = makeNode myID [cRed] (showIt label)
  processedKids <- mapM (tToGV showIt) kids
  let (kidIDs, kidStrings) = unzip processedKids
      kidEdges = map (makeEdge myID) kidIDs
  return (myID, self:(kidEdges++(concat kidStrings)))

eTreeToGraphviz :: Tree (Label,Maybe MismatchType) -> [String]
eTreeToGraphviz t = snd $ evalIDGen t (tToGV gvShowLabelMismatch)

gvShowLabel :: Label -> String
gvShowLabel (LBLString s) = s
gvShowLabel (LBLList    ) = "LIST"
gvShowLabel (LBLInt i   ) = show i

gvShowLabelMismatch :: (Label,Maybe MismatchType) -> String
gvShowLabelMismatch (l, Just m)  = gvShowLabel l ++ ": " ++ show m
gvShowLabelMismatch (l, Nothing) = gvShowLabel l

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
  (read . BL.unpack . fromEntry) <$> findEntryByPath (T.unpack (toTextIgnore fp)) archive

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
  mv (fromText (T.pack (fp++".tmp")))
     (fromText (T.pack fp))

-- | We're mostly interested in C at the moment, so that means
-- .c and .h files.
fileFilter :: T.Text -> Bool
fileFilter fp = (".java" `T.isSuffixOf` fp)

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
size :: SizedTree a -> Int
size (Node (_,s) _) = s

treeType :: Tree (a,Maybe b) -> Maybe b
treeType (Node (_,Just b)  _)    = Just b
treeType (Node (_,Nothing) kids) = foldl' (<|>) Nothing (map treeType kids)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

fromEnum' :: Enum a => Maybe a -> Int
fromEnum' (Just a) = fromEnum a
fromEnum' Nothing  = -1

-- Haskell doesn't know C99's bool type, so we use CChar
foreign import ccall "mult" c_mult :: Ptr CChar -> Ptr CChar -> Ptr CChar -> CInt -> IO ()

true :: Int -> Int -> Vector Bool
true nrs ncs = V.fromList (replicate (nrs*ncs) True)

-- | Computes x ^ y for an sz by sz matrix where
-- we use Bool as the field with 2 elements
modexp :: Vector Bool -> Int -> Int -> IO (Vector Bool)
modexp _ sz 0 = return (true sz sz)
modexp x sz y
  | even y = do
    z <- xx
    mult z z sz
  | otherwise = do
    z  <- xx
    z' <- mult z z sz
    mult x z' sz
  where xx = mult x x sz

-- | Calculates an exponentiation of the matrix in the field with two elements.
-- Take a square matrix as a vector, the size of the matrix in one dimension,
-- the power to raise the matrix to and returns the result.
mult :: Vector Bool -> Vector Bool -> Int -> IO (Vector Bool)
mult x y sz = do
  let len = V.length x
      x'  = toChar <$> x
      y'  = toChar <$> y
  a <- mallocForeignPtrBytes len
  b <- mallocForeignPtrBytes len
  c <- mallocForeignPtrBytes len
  withForeignPtr a $ \ptrX -> do
    pokeArray ptrX (V.toList x')
    withForeignPtr b $ \ptrY -> do
      pokeArray ptrY (V.toList y')
      withForeignPtr c $ \ptrC -> do
        c_mult ptrX ptrY ptrC (fromIntegral sz)
        V.map toBool <$> V.fromList <$> peekArray len ptrC
  where
  toBool :: CChar -> Bool
  toBool = toEnum . fromEnum
  toChar :: Bool -> CChar
  toChar = toEnum . fromEnum
-----------------------------------------------------------------------
