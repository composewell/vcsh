-- |
-- Module      : Vcsh.Command.Files
-- License     : GPL-2.0-or-later
--
-- Commands that list repositories and files, and write the
-- @.gitignore.d@ file of a repository.

module Vcsh.Command.Files
    ( list
    , listTracked
    , listUntracked
    , which
    , writeGitignore
    )
where

import Control.Monad (filterM, when)
import Data.List (foldl', isInfixOf)
import Data.Set (Set)
import System.Exit (exitSuccess)
import Vcsh.Config (Config(..), GitIgnore(..))
import Vcsh.Context
    (Ctx(..), Repo(..), ctxCurrentRepo, forEachRepo, listRepos, use)
import Vcsh.Error (fatal, orFatal)
import Vcsh.FileSystem
    ( (</>), changeDirectory, isDirectory, moveForce, pathExists
    , readTextFile, writeTextFile)
import Vcsh.Git (gitFiles, gitVersion)
import Vcsh.Log (info)

import qualified Data.Set as Set

-- | Sort and remove duplicates, like @sort -u@ with @LC_ALL=C@.
sortUnique :: Ord a => [a] -> [a]
sortUnique = Set.toAscList . Set.fromList

-- | Files tracked by the repository of the context.
trackedFiles :: Ctx -> IO [FilePath]
trackedFiles ctx = gitFiles ctx ["ls-files"]

-- | Files tracked by the given repository, or by any repository if none is
-- given.
trackedFilesOf :: Ctx -> IO [FilePath]
trackedFilesOf ctx =
    case ctxRepo ctx of
        Just _ -> trackedFiles ctx
        Nothing -> concat <$> forEachRepo ctx trackedFiles

-- | Print the names of the repositories.
list :: Ctx -> IO ()
list ctx = listRepos (ctxConfig ctx) >>= mapM_ putStrLn

-- | Print the absolute paths of the files tracked by the repository, or by
-- any repository if there is none in the context.
listTracked :: Ctx -> IO ()
listTracked ctx = do
    files <- trackedFilesOf ctx
    let base = cfgBase (ctxConfig ctx)
    mapM_ (putStrLn . (base </>)) (sortUnique files)

-- | Files not tracked by the repository, as listed by
-- @git ls-files --others@, relative to the current directory. For each file
-- in a directory its top level directory is also included, with a trailing
-- @/@.
untrackedFiles :: Bool -> Bool -> Ctx -> IO (Set FilePath)
untrackedFiles allFiles recursive ctx = do
    files <-
        gitFiles ctx $
            ["ls-files", "--others"]
            ++ ["--exclude-standard" | not allFiles]
            ++ ["--directory" | not recursive]
    dirs <- filterM isDirectory (map (takeWhile (/= '/')) files)
    return $ Set.fromList (files ++ map (++ "/") dirs)

-- | @listUntracked all recursive@ prints the files not tracked by the
-- repository, or by any repository if there is none in the context. With
-- @all@, files ignored by @.gitignore@ are included. With @recursive@, the
-- files in untracked directories are listed instead of the directories.
listUntracked :: Bool -> Bool -> Ctx -> IO ()
listUntracked allFiles recursive ctx = do
    sets <-
        case ctxRepo ctx of
            Just _ -> (: []) <$> untrackedFiles allFiles recursive ctx
            Nothing -> forEachRepo ctx (untrackedFiles allFiles recursive)
    case sets of
        [] -> return ()
        (s : rest) ->
            mapM_ putStrLn (Set.toAscList (foldl' Set.intersection s rest))

-- | Print the tracked files whose path contains the given string, as
-- @<repo>: <file>@. Throw an exception if there is no such file.
which :: Ctx -> String -> IO ()
which ctx str = do
    matches <- concat <$> forEachRepo ctx matching
    case sortUnique matches of
        [] -> fatal ("'" ++ str ++ "' does not exist") 1
        out -> mapM_ putStrLn out

    where

    matching c = do
        files <- trackedFiles c
        let name = repoName (ctxCurrentRepo c)
        return [name ++ ": " ++ f | f <- files, str `isInfixOf` f]

-- | The path and its parent directories, e.g. @a/b/c@, @a/b@ and @a@.
withParents :: FilePath -> [FilePath]
withParents path =
    case break (== '/') (reverse path) of
        (_, '/' : parent) -> path : withParents (reverse parent)
        _ -> [path]

-- | The @.gitignore.d@ entries of the repository: ignore everything except
-- the tracked files. Git versions older than 2.7 also need the parent
-- directories of the files. With 'GitIgnoreRecursive' the contents of the
-- tracked directories are not ignored either. Relative paths are relative
-- to the current directory.
gitignoreEntries :: Ctx -> IO [String]
gitignoreEntries ctx = do
    version <- gitVersion ctx
    files <- trackedFiles ctx
    let paths =
            sortUnique $
                if version >= (2, 7) then files else concatMap withParents files
    entries <- mapM entry paths
    return ("*" : concat entries)

    where

    entry path = do
        dir <-
            if cfgGitIgnore (ctxConfig ctx) == GitIgnoreRecursive
            then isDirectory path
            else return False
        return $ ("!/" ++ path) : ["!/" ++ path ++ "/*" | dir]

-- | Write @.gitignore.d/<repo>@ in the base directory, ignoring all the
-- files not tracked by the repository. An existing file with different
-- contents is moved to @.gitignore.d/<repo>.bak@.
writeGitignore :: Ctx -> IO ()
writeGitignore ctx0 = do
    let cfg = ctxConfig ctx0
        lg = ctxLog ctx0
        base = cfgBase cfg
    when (cfgGitIgnore cfg == GitIgnoreNone) $ do
        info lg "Not writing gitignore as '$VCSH_GITIGNORE' is set to 'none'"
        exitSuccess
    ctx <- use ctx0
    changeDirectory base `orFatal` ("could not enter '" ++ base ++ "'", 11)
    content <- unlines <$> gitignoreEntries ctx
    let file =
            base </> ".gitignore.d" </> repoName (ctxCurrentRepo ctx)
        backup = file ++ ".bak"
    exists <- pathExists file
    old <- if exists then Just <$> readTextFile file else return Nothing
    when (old == Just content) exitSuccess
    when exists $ do
        info lg ("'" ++ file ++ "' differs from new data, moving it to '"
            ++ backup ++ "'")
        moveForce file backup
            `orFatal` ( "could not move '" ++ file ++ "' to '" ++ backup ++ "'"
                      , 53)
    writeTextFile file content
        `orFatal` ("could not write to '" ++ file ++ "'", 57)
