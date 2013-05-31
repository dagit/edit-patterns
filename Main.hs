{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main
( main
)where

import Control.Applicative
import Control.Monad
import Filesystem.Path

-- These lines are recommended by Shelly:
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Shelly hiding (FilePath, (</>))
import Prelude hiding (FilePath)
default (LT.Text)
-- end Shelly

-- Ignore permissons on files:
-- drivers/char/mmtimer.c /tmp/IV5Cnd_mmtimer.c 58eddfdd3110a3a7168f2b8bdbfabefb9691016a 100644 /tmp/9W7Cnd_mmtimer.c 12006182f575a4f3cd09827bcaaea6523077e7b3 100644
data GitDiffArgs = GitDiffArgs
  { gdaFilePath       :: LT.Text
  , gdaBeforeCommit   :: LT.Text
  , gdaAfterCommit    :: LT.Text
  }
  deriving (Show, Read, Eq, Ord)

voidSh :: Sh a -> Sh ()
voidSh sh = sh >> return ()


-- This is where we'll drop off the parsed files
sandbox = "/tmp/dagit/version-patterns"

main :: IO ()
main = shelly $ silently $ do
  -- We add "master" so that if the repo is currently at a weird revision or branch, we get 
  -- reasonable view of the history of master.
  cs <- LT.lines <$> run "git" ["log", "master", "--pretty=format:%H", "--reverse"]
  -- TODO: use the full history
  let pairs = take 10 (zip cs (drop 1 cs))
  -- Log the version pairs for later processing
  mkdir_p sandbox
  liftIO (writeFile (LT.unpack (toTextIgnore (sandbox </> "version-pairs.hs"))) (show pairs))
  ds <- forM pairs $ \(first,second) -> do
    setenv "GIT_EXTERNAL_DIFF" "echo"
    diffLines <- LT.lines <$> run "git" ["diff", first, second]
    -- Git should always return the same information, eg., if this fails something has gone wrong
    -- TODO: what about multiple files? would that generate multiple lines?
    let parseDiffArgs :: [LT.Text] -> GitDiffArgs
        parseDiffArgs l | length l < 7          = error ("unexpected output from git diff: " ++ LT.unpack (LT.concat l))
        parseDiffArgs [fp,before,_,_,after,_,_] = GitDiffArgs { gdaFilePath     = fp
                                                              , gdaBeforeCommit = first
                                                              , gdaAfterCommit  = second }

        diffArgs :: [GitDiffArgs]
        diffArgs = map parseDiffArgs (map LT.words diffLines)

        pathToFile :: LT.Text -> FilePath -> FilePath
        pathToFile commit fp = sandbox </> fromText commit </> fp

        saveATerms :: LT.Text -> [GitDiffArgs] -> Sh ()
        saveATerms commit gdas = do
          -- To properly parse files we need the context that the repository was in
          -- when the commit was made. So we do a checkout. We may also need to
          -- run configure or other commands. We have no support for that yet.
          run "git" ["checkout", commit]
          -------------------------------
          -- Hacks for configuring/building the linux kernel enough to make it
          -- parsable by ROSE
          -- just take the default config
          escaping False (run "yes \"\" | make oldconfig" [])
          -- setup the asm symlink
          run "make" ["include/asm"] 
          -- End Hacks for the kernel
          -------------------------------
          forM_ gdas $ \gda -> do
            let destDir = pathToFile commit (directory (fromText (gdaFilePath gda)))
            mkdir_p destDir
            liftIO (writeFile (LT.unpack (toTextIgnore (pathToFile commit "gdas.hs"))) (show gdas))
            -- parse (gdaFilePath gda) using src2trm and save the result in destDir
            -- Only handle C source for now
            when (".c" `LT.isSuffixOf` (gdaFilePath gda)) $ do
              let from = fromText (gdaFilePath gda)
                  to   = pathToFile commit (replaceExtension from "trm")
              src2trm from to

    saveATerms first  diffArgs
    saveATerms second diffArgs
    return diffArgs
  liftIO (mapM_ print ds)

src2trm :: FilePath -> FilePath -> Sh ()
src2trm from to = do
  -- TODO: remove this hardcode path
  -- For whatever reason, this gcc command gives us an extra newline that is
  -- troublesome, ergo this hack:
  [gccInclude] <- LT.lines <$> run "gcc" ["--print-file-name=include"]
  voidSh (run "/home/dagit/ftt/rose/compileTree/bin/src2term"
                [ "-I/home/dagit/linux/include"
                , "-I/home/dagit/linux/include/asm/mach-default"
                , "-I/home/dagit/linux/include/asm/mach-generic"
                , "-D__KERNEL__"
                , "--aterm", toTextIgnore from, "-o", toTextIgnore to
                ])
