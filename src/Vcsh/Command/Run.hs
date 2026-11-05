-- |
-- Module      : Vcsh.Command.Run
-- License     : GPL-2.0-or-later
--
-- Commands that run a shell, git or another command in one repository or
-- in each repository.

module Vcsh.Command.Run
    ( enter
    , run
    , foreach
    , commit
    , pull
    , push
    , status
    )
where

import Control.Monad (unless, void, when)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode(..))
import System.IO (hIsTerminalDevice, stdout)
import Vcsh.Config (lookupVar)
import Vcsh.Context (Ctx(..), Repo(..), ctxCurrentRepo, childEnv, forEachRepo, use)
import Vcsh.Git (git, gitOutput, gitOutputQuiet)
import Vcsh.Hook (hook)
import Vcsh.Process (inEnv, runForeground)

-- | The exit code of the last command run, 'ExitSuccess' if none.
lastCode :: [ExitCode] -> ExitCode
lastCode [] = ExitSuccess
lastCode codes = last codes

-- | Start @$SHELL@, or @/bin/sh@ if it is not set, in the repository.
enter :: Ctx -> IO ()
enter ctx0 = do
    hook ctx0 "pre-enter"
    ctx <- use ctx0
    let shell = fromMaybe "/bin/sh" (lookupVar "SHELL" (ctxVars ctx))
    case words shell of
        (cmd : args) -> void $ runForeground (inEnv (childEnv ctx)) cmd args
        [] -> return ()
    hook ctx "post-enter"

-- | Run a command in the repository, return its exit code.
run :: Ctx -> String -> [String] -> IO ExitCode
run ctx0 cmd args = do
    hook ctx0 "pre-run"
    ctx <- use ctx0
    code <- runForeground (inEnv (childEnv ctx)) cmd args
    hook ctx "post-run"
    return code

-- | Run a command in each repository, after printing the repository name.
-- If the first argument is 'True' the command is a git command.
foreach :: Ctx -> Bool -> [String] -> IO ()
foreach ctx isGit cmdLine = do
    hook ctx "pre-foreach"
    void $ forEachRepo ctx $ \c -> do
        putStrLn (repoName (ctxCurrentRepo c) ++ ":")
        c1 <- use c
        case (if isGit then "git" : cmdLine else cmdLine) of
            (cmd : args) -> void $ runForeground (inEnv (childEnv c1)) cmd args
            [] -> return ()
    hook ctx "post-foreach"

-- | Commit the staged changes in each repository, return the exit code of
-- the last commit. The arguments are passed to @git commit@.
commit :: Ctx -> [String] -> IO ExitCode
commit ctx args = do
    hook ctx "pre-commit"
    codes <- forEachRepo ctx $ \c -> do
        putStrLn (repoName (ctxCurrentRepo c) ++ ": ")
        c1 <- use c
        code <- git c1 (["commit", "--untracked-files=no", "--quiet"] ++ args)
        putStrLn ""
        return code
    hook ctx "post-commit"
    return (lastCode codes)

-- | Run @git pull@ in each repository, return the exit code of the last
-- one.
pull :: Ctx -> IO ExitCode
pull ctx = do
    codes <- forEachRepo ctx $ \c -> do
        putStr (repoName (ctxCurrentRepo c) ++ ": ")
        hook c "pre-pull"
        c1 <- use c
        code <- git c1 ["pull"]
        putStrLn ""
        hook c1 "post-pull"
        return code
    return (lastCode codes)

-- | Run @git push@ in each repository, return the exit code of the last
-- one.
push :: Ctx -> IO ExitCode
push ctx = do
    hook ctx "pre-push"
    codes <- forEachRepo ctx $ \c -> do
        putStr (repoName (ctxCurrentRepo c) ++ ": ")
        c1 <- use c
        code <- git c1 ["push"]
        putStrLn ""
        return code
    hook ctx "post-push"
    return (lastCode codes)

-- | Lines saying how many commits the current branch is behind and ahead of
-- its upstream branch. No lines if there is no upstream branch or the
-- branches are equal.
aheadBehind :: Ctx -> IO [String]
aheadBehind ctx = do
    (code, out) <-
        gitOutputQuiet ctx
            ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"]
    if code /= ExitSuccess
    then return []
    else do
        let upstream = takeWhile (/= '\n') out
        behind <- count (".." ++ upstream)
        ahead <- count (upstream ++ "..")
        return $
               [ "Behind " ++ upstream ++ " by " ++ show behind ++ " commits"
               | behind /= 0 ]
            ++ [ "Ahead of " ++ upstream ++ " by " ++ show ahead ++ " commits"
               | ahead /= 0 ]

    where

    count range =
        length . lines . snd <$> gitOutput ctx ["log", range, "--oneline"]

-- | Show the status of one repository, or of each repository with the
-- repository name. In the latter case, when the first argument is 'True'
-- (@--terse@), repositories without changes are not shown. Return the exit
-- code of the last @git status@.
status :: Ctx -> Bool -> IO ExitCode
status ctx terse =
    case ctxRepo ctx of
        Just _ -> do
            c <- use ctx
            aheadBehind c >>= mapM_ putStrLn
            git c statusArgs
        Nothing -> do
            tty <- hIsTerminalDevice stdout
            let colorOpts = if tty then ["-c", "color.status=always"] else []
            codes <- forEachRepo ctx $ \c -> do
                c1 <- use c
                ab <- aheadBehind c1
                (code, out) <- gitOutput c1 (colorOpts ++ statusArgs)
                let text =
                        dropWhileEnd (== '\n') (unlines ab ++ out)
                when (not (null text) || not terse) $
                    putStrLn (repoName (ctxCurrentRepo c) ++ ":")
                unless (null text) $ putStrLn text
                unless terse $ putStrLn ""
                return code
            return (lastCode codes)

    where

    statusArgs = ["status", "--short", "--untracked-files=no"]
