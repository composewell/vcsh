-- |
-- Module      : Vcsh.Context
-- License     : GPL-2.0-or-later
--
-- The context in which a vcsh command runs: the configuration, the
-- repository being worked on and the environment of the processes started
-- by the command.
--
-- The processes get the following variables in their environment, in
-- addition to the environment of vcsh:
--
-- * @VCSH_COMMAND@, the command, with @-@ replaced by @_@
-- * @VCSH_REPO_NAME@ and @GIT_DIR@, when there is a repository
-- * @VCSH_DIRECTORY@, the repository name, when the repository is in use,
--   see 'use'

module Vcsh.Context
    (
    -- * Repositories
      Repo (..)
    , repoFromName
    , listRepos

    -- * Context
    , Ctx (..)
    , ctxCurrentRepo
    , withRepo
    , setVar
    , childEnv
    , gitDirExists
    , use
    , forEachRepo
    )
where

import Control.Monad (filterM, forM, unless)
import Data.List (isPrefixOf)
import Vcsh.Config (Config(..), Vars)
import Vcsh.Error (fatal)
import Vcsh.FileSystem
    ((</>), baseName, dropSuffix, isDirectory, isReadable, listDirectory)
import Vcsh.Log (Log)

import qualified Data.Map.Strict as Map

-------------------------------------------------------------------------------
-- Repositories
-------------------------------------------------------------------------------

-- | A vcsh repository.
data Repo = Repo
    { repoName :: String
    , repoGitDir :: FilePath
    }

-- | The repository for a name given on the command line. A name
-- containing a @/@ is the path of the git directory, and the repository
-- name is its last component without the @.git@ suffix.
repoFromName :: Config -> String -> Repo
repoFromName cfg name
    | '/' `elem` name = Repo (dropSuffix ".git" (baseName name)) name
    | otherwise = Repo name (cfgRepoD cfg </> name ++ ".git")

-- | Names of the repositories: the readable directories named @*.git@ in
-- @VCSH_REPO_D@, without the suffix, in sorted order. Names starting with a
-- dot are excluded, like a shell glob does.
listRepos :: Config -> IO [String]
listRepos cfg = do
    let dir = cfgRepoD cfg
    exists <- isDirectory dir
    names <- if exists then listDirectory dir else return []
    let candidates =
            [ n | n <- names
            , not ("." `isPrefixOf` n)
            , dropSuffix ".git" n /= n
            ]
    dirs <- filterM (\n -> isRepoDir (dir </> n)) candidates
    return (map (dropSuffix ".git") dirs)

    where

    isRepoDir p = (&&) <$> isDirectory p <*> isReadable p

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

-- | The context of a command.
data Ctx = Ctx
    { ctxConfig :: Config
    , ctxLog :: Log
    , ctxVars :: Vars         -- ^ Environment for the processes
    , ctxCommand :: String    -- ^ @VCSH_COMMAND@
    , ctxRepo :: Maybe Repo   -- ^ The repository being worked on
    , ctxInUse :: Bool        -- ^ The repository is in use, see 'use'
    }

-- | The repository of the context. The caller must ensure that there is
-- one.
ctxCurrentRepo :: Ctx -> Repo
ctxCurrentRepo ctx =
    case ctxRepo ctx of
        Just r -> r
        Nothing -> error "ctxCurrentRepo: no repository in the context"

-- | Work on the given repository, not yet in use.
withRepo :: Repo -> Ctx -> Ctx
withRepo r ctx = ctx { ctxRepo = Just r, ctxInUse = False }

-- | Add a variable to the environment of the processes.
setVar :: String -> String -> Ctx -> Ctx
setVar name val ctx = ctx { ctxVars = Map.insert name val (ctxVars ctx) }

-- | The environment of the processes started in this context.
childEnv :: Ctx -> [(String, String)]
childEnv ctx = Map.toList (Map.union (Map.fromList overrides) (ctxVars ctx))

    where

    overrides = ("VCSH_COMMAND", ctxCommand ctx) : repoVars
    repoVars =
        case ctxRepo ctx of
            Nothing -> []
            Just r ->
                  ("VCSH_REPO_NAME", repoName r)
                : ("GIT_DIR", repoGitDir r)
                : [("VCSH_DIRECTORY", repoName r) | ctxInUse ctx]

-- | Throw an exception if the git directory of the repository does not
-- exist.
gitDirExists :: Ctx -> IO ()
gitDirExists ctx = do
    let r = ctxCurrentRepo ctx
    exists <- isDirectory (repoGitDir r)
    unless exists $
        fatal ("no repository found for '" ++ repoName r ++ "'") 12

-- | Mark the repository as in use, setting @VCSH_DIRECTORY@. Throw an
-- exception if the git directory does not exist.
use :: Ctx -> IO Ctx
use ctx = do
    gitDirExists ctx
    return ctx { ctxInUse = True }

-- | Run an action for each repository in 'listRepos' order, passing it the
-- context with the repository set.
forEachRepo :: Ctx -> (Ctx -> IO a) -> IO [a]
forEachRepo ctx f = do
    let cfg = ctxConfig ctx
    names <- listRepos cfg
    forM names $ \name -> f (withRepo (repoFromName cfg name) ctx)
