-- |
-- Module      : Vcsh.Hook
-- License     : GPL-2.0-or-later
--
-- Hooks are executables in @VCSH_HOOK_D@, run at defined points of a
-- command. For the hook point @pre-commit@, the files whose names start
-- with @pre-commit@ or @<repo>.pre-commit@ are run, in sorted order.
--
-- Overlays are shell scripts in @VCSH_OVERLAY_D@ that the shell
-- implementation of vcsh sources to replace its functions. This
-- implementation cannot source them and prints an error for each overlay
-- found.

module Vcsh.Hook
    ( hook
    , warnOverlays
    )
where

import Control.Monad (filterM, forM_, unless)
import Data.List (isPrefixOf)
import System.Exit (ExitCode(..))
import Vcsh.Config (Config(..))
import Vcsh.Context (Ctx(..), Repo(..), childEnv)
import Vcsh.Error (die)
import Vcsh.FileSystem
    ((</>), isDirectory, isExecutable, isReadable, listDirectory)
import Vcsh.Log (errorMsg, verbose)
import Vcsh.Process (inEnv, runForeground)

-- | Files in a directory whose names start with the name of the hook
-- point or with @<repo>.@ followed by it, matching the shell globs
-- @"$dir/$point"*@ and @"$dir/$repo.$point"*@ in that order.
matchingFiles :: Ctx -> FilePath -> String -> IO [FilePath]
matchingFiles ctx dir point = do
    exists <- isDirectory dir
    names <- if exists then listDirectory dir else return []
    return [dir </> n | p <- prefixes, n <- names, p `isPrefixOf` n]

    where

    prefixes =
        point : [repoName r ++ "." ++ point | Just r <- [ctxRepo ctx]]

-- | Run the hooks for a hook point. Throw an exception if a hook fails.
hook :: Ctx -> String -> IO ()
hook ctx point = do
    files <- matchingFiles ctx (cfgHookD (ctxConfig ctx)) point
    hooks <- filterM isExecutable files
    forM_ hooks $ \h -> do
        verbose (ctxLog ctx) ("executing '" ++ h ++ "'")
        code <- runForeground (inEnv (childEnv ctx)) h []
        unless (code == ExitSuccess) $ die ("Hook [" ++ h ++ "] failed")

-- | Print an error for each overlay of the current command.
warnOverlays :: Ctx -> IO ()
warnOverlays ctx = do
    files <-
        matchingFiles ctx (cfgOverlayD (ctxConfig ctx)) (ctxCommand ctx)
    overlays <- filterM isReadable files
    forM_ overlays $ \o ->
        errorMsg (ctxLog ctx) ("overlays are not supported, ignoring '"
            ++ o ++ "'")
