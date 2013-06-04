{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main
( main
)where

-- External dependencies imports
import ATerm.AbstractSyntax
import ATerm.ReadWrite
import Codec.Archive.Zip
import Control.Applicative
import Control.Exception.Base
import Control.Monad
import Data.List
import Data.Maybe
import Data.String
import Data.Tree.ATerm
import Data.Tree.AntiUnification
import Filesystem hiding (writeFile, readFile, withFile, openFile)
import Filesystem.Path
import Prelude hiding (FilePath)
import Shelly hiding (FilePath, (</>))
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (FilePath)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as LT

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
    "Mode of operation, MODE is either generate-aterms or antiunify-aterms"
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

  forM_ pairs $ \(first,second) -> do
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

        saveATerms :: LT.Text -> [GitDiffArgs] -> Sh ()
        saveATerms commit gdas = do
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
          void (run "./configure" []) `catchany_sh` (const (return ()))
          -- End Hacks for the kernel
          -------------------------------
          forM_ gdas $ \gda -> do
            -- We make a point to read/write the achrive each iteration to avoid
            -- losing data from having the run terminate in the middle
            archiveBS <- liftIO (B.readFile archiveFP)
            let commitDir   = fromText commit
                archive     = toArchive archiveBL
                gdasFP      = LT.unpack (toTextIgnore (commitDir </> "gdas.hs"))
                gdasEntry   = toEntry gdasFP 0 (BL.pack (show gdas))
                gdasArchive = gdasEntry `addEntryToArchive` archive
                archiveBL   = BL.fromChunks [archiveBS]
            -- parse (gdaFilePath gda) using src2trm and save the result in destDir
            -- Only handle C source for now
            when (fileFilter (gdaFilePath gda)) $ do
              liftIO (putStrLn ("running src2term for " ++ (LT.unpack (gdaFilePath gda))))
              let from    = fromText (gdaFilePath gda)
                  to      = commitDir </> (replaceExtension from "trm")
              catchany_sh
                (do trm <- src2term from to
                    let trmArchive = toEntry (LT.unpack (toTextIgnore to)) 0 (BL.fromChunks [trm]) `addEntryToArchive` gdasArchive
                    writeFileInChunks archiveFP (fromArchive trmArchive)
                    liftIO (putStrLn ("Wrote: " ++ LT.unpack (toTextIgnore to) ++ " in archive.")))
                (\e -> do
                  liftIO (putStr "Error running src2term: ")
                  liftIO (putStrLn (show e))
                  return ())


    -- Finally, save off the term files
    saveATerms first  diffArgs
    saveATerms second diffArgs

-- | src2term runs the ROSE program by the same name over the input file,
-- saves the output to a temp file, reads it back in, deletes the temp file
-- and returns the string version.
-- NOTE: We can't just read it from stdout because ROSE dumps warnings there
-- and later we may not be able to parse it.
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

-- | processTerms looks in the version-patterns.zip archive
-- for version-pairs.hs. When it finds it, then it starts
-- looking for GitDiffArgs in gdas.hs in each commit directory.
-- When it reads GitDiffArgs it loads the two term representations
-- mentioned in the GitDiffArgs and computes the antiunification
-- of the two and adds that to the zip archive.
processTerms :: FilePath -> Sh ()
processTerms dir = do
  let archiveFP = LT.unpack (toTextIgnore (dir </> "version-patterns.zip"))
  -- Read the archive strictly so that we know the handle is freed as soon as we are done
  initArchiveBS <- liftIO (B.readFile archiveFP)

  -- find version-pairs.hs in the archive
  let mb_ds       = readFromArchive initArchive "version-pairs.hs" :: Maybe [(LT.Text,LT.Text)]
      initArchive = toArchive (BL.fromChunks [initArchiveBS])
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
            mb_gdas      = readFromArchive initArchive gdasFilePath :: Maybe [GitDiffArgs]
        case mb_gdas of
          Just gdas -> do
            liftIO (putStrLn ("Found gdas.hs"))
            -- For each GitDiffArg we will antiunify them separately
            forM_ gdas $ \gda -> do
              catchany_sh
                -- Make sure we can process this file
                (when (fileFilter (gdaFilePath gda)) $ do
                  liftIO (putStrLn ("Antiunify using " ++ show gda))
                  let diffDir             = fromText (commitBefore `LT.append` ".." `LT.append` commitAfter)
                      antiunifiedFilePath = diffDir </> (replaceExtension (fromText (gdaFilePath gda)) "hs")
                      antiTerms           = antiUnifySh initArchive gda
                  case antiTerms of
                    -- Something went wrong
                    Left e          -> liftIO (putStrLn e)
                    Right (t,s1,s2) -> do
                      -- Reload the archive before we add anything to it
                      curArchiveBS <- liftIO (B.readFile archiveFP)
                      let entry      = toEntry (LT.unpack (toTextIgnore antiunifiedFilePath)) 0 (BL.pack (show (t,s1,s2)))
                          curArchive = toArchive (BL.fromChunks [curArchiveBS])
                          newArchive = entry `addEntryToArchive` curArchive
                      liftIO (putStrLn ("Wrote antiunification to: " ++ (LT.unpack (toTextIgnore antiunifiedFilePath))))
                      writeFileInChunks archiveFP (fromArchive newArchive)
                      liftIO (putStrLn "Done writing archive."))
                -- Log the error and move on
                (\e -> do
                  liftIO (putStr "Error processingTerms: ")
                  liftIO (putStrLn (show e)))
          Nothing -> return ()
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
    (Just tb, Just ta) -> Right $
      let termToTree t = atermToTree (getATerm t) t
          termBefore   = termToTree (readATerm (BL.unpack (fromEntry tb)))
          termAfter    = termToTree (readATerm (BL.unpack (fromEntry ta)))
      in (termBefore `antiunify` termAfter)
    (_, _) -> Left "Failed to load terms"
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
fileFilter fp = (".c" `LT.isSuffixOf` fp)
              ||(".h" `LT.isSuffixOf` fp)
-----------------------------------------------------------------------
