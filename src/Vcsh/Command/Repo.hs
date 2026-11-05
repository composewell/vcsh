-- |
-- Module      : Vcsh.Command.Repo
-- License     : GPL-2.0-or-later
--
-- Commands that create, configure, rename and delete a repository. The
-- repository is the one set in the 'Ctx'.

module Vcsh.Command.Repo
    ( upgrade
    , initRepo
    , clone
    , rename
    , delete
    )
where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, void, when)
import Data.List (intercalate)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO (isEOF)
import Vcsh.Config (Config(..), GitIgnore(..), WorkTree(..))
import Vcsh.Context (Ctx(..), Repo(..), ctxCurrentRepo, childEnv, gitDirExists, use, withRepo)
import Vcsh.Error (fatal, orFatal)
import Vcsh.FileSystem
    ( (</>), changeDirectory, createDirectories, isDirectory, moveForce
    , pathExists, removeFile, removeTree)
import Vcsh.Git (git, gitConfig, gitFiles, gitOutputQuiet, gitVersion)
import Vcsh.Hook (hook)
import Vcsh.Log (errorMsg, info)
import Vcsh.Process (inDir, inEnv, readStdout)

-- | @core.worktree@ for 'WorkTreeRelative': the base directory relative to
-- the git directory.
relativeWorkTree :: Ctx -> IO FilePath
relativeWorkTree ctx = do
    let env = ("GIT_WORK_TREE", cfgBase (ctxConfig ctx)) : childEnv ctx
        gitDir = repoGitDir (ctxCurrentRepo ctx)
    (_, out) <-
        readStdout
            (inDir gitDir . inEnv env) "git" ["rev-parse", "--show-cdup"]
    return (takeWhile (/= '\n') out)

-- | Configure the repository with the current settings and add the
-- @.gitignore.d@ and @.gitattributes.d@ files of the repository, if they
-- exist. Return the context with the repository in use.
upgrade :: Ctx -> IO Ctx
upgrade ctx = do
    hook ctx "pre-upgrade"
    -- A fake-bare repository is not bare. With core.bare true git refuses
    -- to use core.worktree.
    gitConfig ctx "core.bare" "false"
    worktree <-
        case cfgWorkTree cfg of
            WorkTreeAbsolute -> return (cfgBase cfg)
            WorkTreeRelative -> relativeWorkTree ctx
    gitConfig ctx "core.worktree" worktree
    when (cfgGitIgnore cfg /= GitIgnoreNone) $
        gitConfig ctx "core.excludesfile" (".gitignore.d" </> name)
    when (cfgGitAttributes cfg) $
        gitConfig ctx "core.attributesfile" (".gitattributes.d" </> name)
    gitConfig ctx "vcsh.vcsh" "true"
    ctx1 <- use ctx
    addIfExists ctx1 (cfgBase cfg </> ".gitignore.d" </> name)
    addIfExists ctx1 (cfgBase cfg </> ".gitattributes.d" </> name)
    hook ctx1 "post-upgrade"
    return ctx1

    where

    cfg = ctxConfig ctx
    name = repoName (ctxCurrentRepo ctx)

    addIfExists c file = do
        exists <- pathExists file
        when exists $ void $ git c ["add", "-f", file]

-- | Create the repository in the base directory and 'upgrade' it. Throw an
-- exception if the git directory exists. The base directory becomes the
-- current directory.
initRepo :: Ctx -> IO Ctx
initRepo ctx = do
    hook ctx "pre-init"
    let gitDir = repoGitDir (ctxCurrentRepo ctx)
        base = cfgBase (ctxConfig ctx)
    exists <- pathExists gitDir
    when exists $ fatal ("'" ++ gitDir ++ "' exists") 10
    createDirectories base `orFatal` ("could not create '" ++ base ++ "'", 50)
    changeDirectory base `orFatal` ("could not enter '" ++ base ++ "'", 11)
    void $ git ctx ["init", "--shared=0600"]
    ctx1 <- upgrade ctx
    hook ctx1 "post-init"
    return ctx1

-- | @clone remote branch@ creates the repository and merges the given
-- branch of the remote into it. Exit without merging if the remote branch
-- does not exist. Throw an exception if the merge would overwrite existing
-- files.
clone :: Ctx -> String -> String -> IO ExitCode
clone ctx0 remote branch = do
    hook ctx0 "pre-clone"
    ctx <- initRepo ctx0
    void $ git ctx ["remote", "add", "origin", remote]
    code <- git ctx ["checkout", "-b", branch]
    if code /= ExitSuccess
    then return code
    else do
        gitConfig ctx ("branch." ++ branch ++ ".remote") "origin"
        gitConfig ctx ("branch." ++ branch ++ ".merge") ("refs/heads/" ++ branch)
        (_, refs) <- gitOutputQuiet ctx ["ls-remote", "origin", branch]
        when (null (lines refs)) $ do
            info (ctxLog ctx)
                "remote is empty, not merging anything.\n\
                \  You should add files to your new repository."
            exitSuccess
        (major, _) <- gitVersion ctx
        void $ git ctx
            (["fetch", "origin"] ++ [branch | major > 1])
        hook ctx "pre-merge"
        merged <- git ctx ["read-tree", "-n", "-mu", "origin/" ++ branch]
        unless (merged == ExitSuccess) $
            fatal ("will stop after fetching and not try to merge!\n\
                \  Once this situation has been resolved, run 'vcsh "
                ++ repoName (ctxCurrentRepo ctx)
                ++ " pull' to finish cloning.") 17
        void $ git ctx ["-c", "merge.ff=true", "merge", "origin/" ++ branch]
        hook ctx "post-merge"
        hook ctx "post-clone"
        hook ctx { ctxInUse = False } "post-clone-retired"
        return ExitSuccess

-- | Rename the repository to the given repository and 'upgrade' it. Throw
-- an exception if the repository does not exist or the new one exists.
rename :: Ctx -> Repo -> IO ()
rename ctx new = do
    gitDirExists ctx
    let oldDir = repoGitDir (ctxCurrentRepo ctx)
        newDir = repoGitDir new
    exists <- isDirectory newDir
    when exists $ fatal ("'" ++ newDir ++ "' exists") 54
    moveForce oldDir newDir
        `orFatal` ("Could not mv '" ++ oldDir ++ "' '" ++ newDir ++ "'", 52)
    void $ upgrade (withRepo new ctx)

-- | Delete the files tracked by the repository and the repository, after
-- asking the user to confirm by typing @Yes, do as I say@. Exit with code
-- 16 if the user does not confirm.
delete :: Ctx -> IO ()
delete ctx0 = do
    let base = cfgBase (ctxConfig ctx0)
        lg = ctxLog ctx0
    changeDirectory base `orFatal` ("could not enter '" ++ base ++ "'", 11)
    ctx <- use ctx0
    info lg "This operation WILL DESTROY DATA!"
    files <- gitFiles ctx ["ls-files"]
    putStrLn $
        "These files will be deleted:\n\n"
        ++ intercalate "\n" files
        ++ "\n\nAGAIN, THIS WILL DELETE YOUR DATA!\n\
           \To continue, type 'Yes, do as I say'"
    eof <- isEOF
    answer <- if eof then return "" else getLine
    unless (answer == "Yes, do as I say") $ exitWith (ExitFailure 16)
    forM_ files $ \file ->
        removeFile file `catch` \(_ :: IOException) ->
            info lg ("could not delete '" ++ file
                ++ "', continuing with deletion")
    let gitDir = repoGitDir (ctxCurrentRepo ctx)
    removeTree gitDir `catch` \(_ :: IOException) ->
        errorMsg lg ("could not delete '" ++ gitDir ++ "'")
