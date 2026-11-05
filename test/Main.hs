-- |
-- Module      : Main
-- License     : GPL-2.0-or-later
--
-- Run the same commands with the shell implementation of vcsh (@./vcsh@)
-- and with the Haskell implementation (@vcsh@ in @PATH@), each in its own
-- sandbox directory, and compare the output and exit codes.
--
-- Each step is a shell command line in which @$VCSH@ runs vcsh. The
-- command runs in the home directory of the sandbox with stderr merged
-- into stdout. The sandbox path is replaced by @<ROOT>@ in the output.

module Main (main) where

import Control.Monad (forM, unless)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Vcsh.Process (inDir, inEnv, readStdout)

-------------------------------------------------------------------------------
-- Steps
-------------------------------------------------------------------------------

-- | Shell command lines, run in order in the same sandbox.
steps :: [String]
steps =
    -- No repositories
    [ "$VCSH status"
    , "$VCSH list"
    , "$VCSH"
    , "$VCSH help"
    , "$VCSH version | head -1"
    , "$VCSH nosuchrepo"
    , "$VCSH nosuchrepo status"
    , "$VCSH which"
    , "$VCSH which a b"
    , "$VCSH init"
    , "$VCSH rename x"
    , "$VCSH run x"
    , "$VCSH foreach"
    , "$VCSH clone"
    , "$VCSH status nosuchrepo"
    , "VCSH_GITIGNORE=bad $VCSH status"
    , "VCSH_WORKTREE=bad $VCSH status"
    , "$VCSH -c nosuchfile status"

    -- One repository
    , "$VCSH init test1"
    , "$VCSH init test1"
    , "$VCSH status"
    , "$VCSH list"
    , "ls -a .gitignore.d"
    , "git --git-dir=.config/vcsh/repo.d/test1.git config -l | sort"
    , "touch a && $VCSH test1 add a"
    , "$VCSH status"
    , "$VCSH status --terse"
    , "$VCSH status test1"
    , "$VCSH test1 commit -q -m first"
    , "$VCSH test1 log --oneline"
    , "$VCSH st"
    , "$VCSH status --terse"
    , "$VCSH list-tracked"
    , "$VCSH list-tracked test1"
    , "$VCSH list-tracked-by test1"
    , "$VCSH which a"
    , "$VCSH which nosuchfile"
    , "$VCSH write-gitignore test1 && cat .gitignore.d/test1"
    , "$VCSH write-gitignore test1 && ls .gitignore.d"
    , "echo b > b && $VCSH test1 add b && $VCSH test1 commit -q -m second"
    , "$VCSH wr test1 && cat .gitignore.d/test1 && ls .gitignore.d"
    , "mkdir -p d/e && touch d/e/f u && $VCSH list-untracked"
    , "$VCSH list-untracked -a test1"
    , "$VCSH upgrade test1"
    , "$VCSH run test1 git log --oneline"
    , "$VCSH run test1 sh -c 'echo $VCSH_DIRECTORY $VCSH_REPO_NAME; exit 3'"
    , "$VCSH enter test1"
    , "$VCSH test1"

    -- Two repositories
    , "$VCSH init test2"
    , "echo c > c && $VCSH test2 add c && $VCSH co -m one"
    , "$VCSH commit"
    , "$VCSH foreach log --oneline"
    , "$VCSH foreach -g echo hello"
    , "$VCSH list-tracked"
    , "$VCSH list-untracked"
    , "$VCSH which a"
    , "$VCSH rename test2 test3 && $VCSH list"
    , "$VCSH rename test3 test1"
    , "$VCSH rename nosuchrepo test4"

    -- Remotes
    , "git init -q --bare ../remote.git"
    , "$VCSH test1 remote add origin ../remote.git"
    , "$VCSH test1 push -q origin master"
    , "$VCSH pull 2>&1 | grep -v '^hint'"
    , "$VCSH status test1"
    , "$VCSH clone ../remote.git clone1"
    , "$VCSH list"
    , "$VCSH clone -b nobranch ../remote.git clone2"
    , "$VCSH list"

    -- Hooks
    , "mkdir -p .config/vcsh/hooks-enabled"
    , "printf '#!/bin/sh\\necho pre-command $VCSH_COMMAND $VCSH_REPO_NAME\\n'\
      \ > .config/vcsh/hooks-enabled/pre-command"
    , "printf '#!/bin/sh\\necho test1 post-command\\n'\
      \ > .config/vcsh/hooks-enabled/test1.post-command"
    , "chmod +x .config/vcsh/hooks-enabled/*"
    , "$VCSH list"
    , "$VCSH status test1"
    , "$VCSH list-tracked test1"
    , "printf '#!/bin/sh\\nexit 1\\n' > .config/vcsh/hooks-enabled/pre-run"
    , "chmod +x .config/vcsh/hooks-enabled/pre-run && $VCSH run test1 true"
    , "rm -rf .config/vcsh/hooks-enabled"

    -- Configuration files
    , "mkdir -p .config/vcsh && echo VCSH_GITIGNORE=none\
      \ > .config/vcsh/config && $VCSH write-gitignore test1"
    , "echo 'VCSH_BASE=$HOME/base' > conf && $VCSH -c conf list-tracked"
    , "rm .config/vcsh/config conf"

    -- Delete
    , "echo no | $VCSH delete test1"
    , "ls"
    , "echo 'Yes, do as I say' | $VCSH delete test1"
    , "ls"
    , "$VCSH list"
    , "$VCSH delete nosuchrepo"
    ]

-------------------------------------------------------------------------------
-- Running the steps
-------------------------------------------------------------------------------

-- | Replace all occurrences of a non-empty string.
replace :: String -> String -> String -> String
replace old new = go

    where

    go [] = []
    go s@(c : rest)
        | old `isPrefixOf` s = new ++ go (drop (length old) s)
        | otherwise = c : go rest

-- | Run the steps with the given vcsh command in a new sandbox directory,
-- return the output of each step.
runSteps :: FilePath -> String -> IO [String]
runSteps root vcsh = do
    let home = root ++ "/home"
    _ <- readStdout id "mkdir" ["-p", home]
    path <- fromMaybe "/usr/bin:/bin" <$> lookupEnv "PATH"
    let env =
            [ ("PATH", path)
            , ("HOME", home)
            , ("LC_ALL", "C")
            , ("SHELL", "/bin/echo")
            , ("VCSH", vcsh)
            , ("GIT_AUTHOR_NAME", "Test")
            , ("GIT_AUTHOR_EMAIL", "test@example.com")
            , ("GIT_AUTHOR_DATE", "2020-01-01T00:00:00Z")
            , ("GIT_COMMITTER_NAME", "Test")
            , ("GIT_COMMITTER_EMAIL", "test@example.com")
            , ("GIT_COMMITTER_DATE", "2020-01-01T00:00:00Z")
            , ("GIT_CONFIG_NOSYSTEM", "1")
            , ("GIT_CONFIG_COUNT", "1")
            , ("GIT_CONFIG_KEY_0", "init.defaultBranch")
            , ("GIT_CONFIG_VALUE_0", "master")
            ]
    forM steps $ \step -> do
        (_, out) <-
            readStdout
                (inDir home . inEnv env)
                "/bin/sh"
                ["-c", "{ " ++ step ++ "; } 2>&1; echo \"[exit $?]\""]
        return (replace root "<ROOT>" out)

main :: IO ()
main = do
    (_, tmp) <- readStdout id "mktemp" ["-d"]
    (_, pwd) <- readStdout id "pwd" []
    let root = takeWhile (/= '\n') tmp
        script = takeWhile (/= '\n') pwd ++ "/vcsh"
    expected <- runSteps (root ++ "/sh") ("sh " ++ script)
    actual <- runSteps (root ++ "/hs") "vcsh"
    failures <-
        forM (zip3 steps expected actual) $ \(step, e, a) ->
            if e == a
            then return False
            else do
                putStrLn ("FAIL: " ++ step)
                putStrLn "--- expected (shell implementation)"
                putStr e
                putStrLn "--- actual (Haskell implementation)"
                putStr a
                return True
    _ <- readStdout id "rm" ["-rf", root]
    let n = length (filter id failures)
    putStrLn (show (length steps - n) ++ " of " ++ show (length steps)
        ++ " steps match")
    unless (n == 0) exitFailure
