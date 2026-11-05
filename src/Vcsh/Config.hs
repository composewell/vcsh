-- |
-- Module      : Vcsh.Config
-- License     : GPL-2.0-or-later
--
-- Configuration from environment variables and configuration files.
--
-- Configuration files are shell scripts that set variables, e.g.
-- @VCSH_BASE=$HOME/dotfiles@. A file is sourced by @/bin/sh@ with @set -a@,
-- so all the variables it sets are exported, and the resulting environment
-- is read back. Variables exported by a configuration file are also passed
-- to the processes started by vcsh, e.g. git and hooks.

module Vcsh.Config
    (
    -- * Variables
      Vars
    , lookupVar
    , environmentVars
    , sourceIfReadable

    -- * Configuration
    , GitIgnore (..)
    , WorkTree (..)
    , Config (..)
    , loadConfigFiles
    , mkConfig
    )
where

import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import Vcsh.Error (fatal)
import Vcsh.FileSystem ((</>), isReadable)
import Vcsh.Process (inEnv, readStdout, splitNul)

import qualified Data.Map.Strict as Map

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------

-- | Environment variables, name to value.
type Vars = Map String String

-- | The value of a variable. An empty value is treated as unset, like
-- @${VAR:-default}@ in a shell.
lookupVar :: String -> Vars -> Maybe String
lookupVar name vars =
    case Map.lookup name vars of
        Just v | not (null v) -> Just v
        _ -> Nothing

varOr :: String -> String -> Vars -> String
varOr name def vars = fromMaybe def (lookupVar name vars)

-- | Set a variable if it is unset or empty, like @: ${VAR:=default}@.
setDefault :: String -> String -> Vars -> Vars
setDefault name def vars = Map.insert name (varOr name def vars) vars

-- | The environment of the vcsh process.
environmentVars :: IO Vars
environmentVars = Map.fromList <$> getEnvironment

-- | Parse the output of @env -0@.
parseEnv :: String -> Vars
parseEnv = Map.fromList . map (fmap (drop 1) . break (== '=')) . splitNul

-- | Source a shell script using @/bin/sh@ with the given environment and
-- return the resulting environment. Throw an exception if the script
-- fails.
sourceFile :: Vars -> FilePath -> IO Vars
sourceFile vars file = do
    (code, out) <-
        readStdout
            (inEnv (Map.toList vars))
            "/bin/sh"
            ["-c", "set -a; . \"$1\" && env -0", "sh", file]
    case code of
        ExitSuccess -> return (parseEnv out)
        ExitFailure _ -> fatal ("could not source '" ++ file ++ "'") 1

-- | Source a shell script if it is readable, otherwise return the
-- variables unchanged. The path must contain a @/@, otherwise the shell
-- searches it in @PATH@.
sourceIfReadable :: Vars -> FilePath -> IO Vars
sourceIfReadable vars file = do
    r <- isReadable file
    if r then sourceFile vars file else return vars

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | How @write-gitignore@ writes @.gitignore.d/<repo>@ (@VCSH_GITIGNORE@).
data GitIgnore =
      GitIgnoreExact     -- ^ List the tracked files.
    | GitIgnoreNone      -- ^ Do not use @.gitignore.d@.
    | GitIgnoreRecursive -- ^ Also list the contents of tracked directories.
    deriving Eq

-- | How @core.worktree@ is set (@VCSH_WORKTREE@).
data WorkTree =
      WorkTreeAbsolute -- ^ Absolute path of the base directory.
    | WorkTreeRelative -- ^ Path of the base directory relative to the
                       -- repository.
    deriving Eq

-- | vcsh configuration.
data Config = Config
    { cfgConfigHome :: FilePath -- ^ @XDG_CONFIG_HOME@
    , cfgRepoD :: FilePath      -- ^ @VCSH_REPO_D@, the repositories
    , cfgHookD :: FilePath      -- ^ @VCSH_HOOK_D@, the hooks
    , cfgOverlayD :: FilePath   -- ^ @VCSH_OVERLAY_D@, the overlays
    , cfgBase :: FilePath       -- ^ @VCSH_BASE@, the work tree
    , cfgGitIgnore :: GitIgnore -- ^ @VCSH_GITIGNORE@
    , cfgGitAttributes :: Bool  -- ^ @VCSH_GITATTRIBUTES@ is not @none@
    , cfgWorkTree :: WorkTree   -- ^ @VCSH_WORKTREE@
    , cfgDebug :: Bool          -- ^ @VCSH_DEBUG@ is set
    , cfgVerbose :: Bool        -- ^ @VCSH_VERBOSE@ is set
    }

-- | Set the default @XDG_CONFIG_HOME@ and source the configuration files,
-- in this order:
--
-- * @/etc/vcsh/config@
-- * @$XDG_CONFIG_HOME/vcsh/config@
-- * the file given by the @-c@ option, if any
--
-- Throw an exception if the file given by the @-c@ option is not readable.
loadConfigFiles :: Maybe FilePath -> Vars -> IO Vars
loadConfigFiles optConfig vars0 = do
    let home = varOr "HOME" "" vars0
        vars1 = setDefault "XDG_CONFIG_HOME" (home </> ".config") vars0
    vars2 <- sourceIfReadable vars1 "/etc/vcsh/config"
    vars3 <-
        sourceIfReadable vars2 (varOr "XDG_CONFIG_HOME" "" vars2 </> "vcsh/config")
    case optConfig of
        Nothing -> return vars3
        Just file -> do
            let file1 = if '/' `elem` file then file else "." </> file
            r <- isReadable file1
            if r
            then sourceFile vars3 file1
            else fatal ("Can not read configuration file '" ++ file ++ "'") 1

-- | Make the configuration from the variables, using the defaults for the
-- unset variables. Throw an exception if a variable has an invalid value.
mkConfig :: Vars -> IO Config
mkConfig vars = do
    gitIgnore <-
        case var "VCSH_GITIGNORE" "exact" of
            "exact" -> return GitIgnoreExact
            "none" -> return GitIgnoreNone
            "recursive" -> return GitIgnoreRecursive
            _ -> fatal
                    "'$VCSH_GITIGNORE' must equal 'exact', 'none', or \
                    \'recursive'" 1
    workTree <-
        case var "VCSH_WORKTREE" "absolute" of
            "absolute" -> return WorkTreeAbsolute
            "relative" -> return WorkTreeRelative
            _ -> fatal
                    "'$VCSH_WORKTREE' must equal 'absolute', or 'relative'" 1
    return Config
        { cfgConfigHome = configHome
        , cfgRepoD = var "VCSH_REPO_D" (configHome </> "vcsh/repo.d")
        , cfgHookD = var "VCSH_HOOK_D" (configHome </> "vcsh/hooks-enabled")
        , cfgOverlayD =
            var "VCSH_OVERLAY_D" (configHome </> "vcsh/overlays-enabled")
        , cfgBase = var "VCSH_BASE" home
        , cfgGitIgnore = gitIgnore
        , cfgGitAttributes = var "VCSH_GITATTRIBUTES" "none" /= "none"
        , cfgWorkTree = workTree
        , cfgDebug = isSet "VCSH_DEBUG"
        , cfgVerbose = isSet "VCSH_VERBOSE"
        }

    where

    var name def = varOr name def vars
    isSet name = isJust (lookupVar name vars)
    home = var "HOME" ""
    configHome = var "XDG_CONFIG_HOME" (home </> ".config")
