-- |
-- Module      : Vcsh.Git
-- License     : GPL-2.0-or-later
--
-- Running git in the environment of a 'Ctx'.

module Vcsh.Git
    ( git
    , gitWith
    , gitOutput
    , gitOutputQuiet
    , gitFiles
    , gitConfig
    , gitVersion
    , parseGitVersion
    )
where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode)
import Vcsh.Context (Ctx, childEnv)
import Vcsh.Process
    (Modifier, inEnv, readStdout, readStdoutQuiet, runForeground, splitNul)

-- | Run git with stdio inherited from vcsh, return its exit code.
git :: Ctx -> [String] -> IO ExitCode
git ctx = runForeground (inEnv (childEnv ctx)) "git"

-- | Like 'git' but also apply the given process configuration.
gitWith :: Modifier -> Ctx -> [String] -> IO ExitCode
gitWith modifier ctx = runForeground (modifier . inEnv (childEnv ctx)) "git"

-- | Run git and return its exit code and stdout. stderr is inherited.
gitOutput :: Ctx -> [String] -> IO (ExitCode, String)
gitOutput ctx = readStdout (inEnv (childEnv ctx)) "git"

-- | Like 'gitOutput' but discard stderr.
gitOutputQuiet :: Ctx -> [String] -> IO (ExitCode, String)
gitOutputQuiet ctx = readStdoutQuiet (inEnv (childEnv ctx)) "git"

-- | The files listed by a git command that takes the @-z@ option, e.g.
-- @ls-files@. The list is empty if git fails.
gitFiles :: Ctx -> [String] -> IO [FilePath]
gitFiles ctx args = splitNul . snd <$> gitOutput ctx (args ++ ["-z"])

-- | Set a git configuration variable of the repository, like
-- @git config <name> <value>@.
gitConfig :: Ctx -> String -> String -> IO ()
gitConfig ctx name value = void $ git ctx ["config", name, value]

-- | Major and minor version from the output of @git --version@, e.g.
-- @git version 2.43.0@.
parseGitVersion :: String -> Maybe (Int, Int)
parseGitVersion str =
    case words str of
        ("git" : "version" : v : _) ->
            case break (== '.') v of
                (major, '.' : rest) | isNumber major ->
                    let minor = takeWhile isDigit rest
                     in if null minor
                        then Nothing
                        else Just (read major, read minor)
                _ -> Nothing
        _ -> Nothing

    where

    isNumber s = not (null s) && all isDigit s

-- | Major and minor version of git, @(0, 0)@ if it cannot be determined.
gitVersion :: Ctx -> IO (Int, Int)
gitVersion ctx = do
    (_, out) <- gitOutput ctx ["--version"]
    return $ fromMaybe (0, 0) (parseGitVersion out)
