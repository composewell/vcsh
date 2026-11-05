-- |
-- Module      : Vcsh.FileSystem
-- License     : GPL-2.0-or-later
--
-- File system operations on 'FilePath' arguments, implemented using
-- streamly-coreutils and streamly-core.

module Vcsh.FileSystem
    (
    -- * Paths
      (</>)
    , baseName
    , dropSuffix

    -- * Tests
    , pathExists
    , isDirectory
    , isReadable
    , isExecutable

    -- * Directories
    , changeDirectory
    , createDirectories
    , listDirectory

    -- * Files
    , readTextFile
    , writeTextFile
    , moveForce
    , removeFile
    , removeTree
    )
where

import Data.Function ((&))
import Data.List (isSuffixOf, sort)
import Streamly.FileSystem.Path (Path)
import Streamly.Coreutils.FileTest (FileTest)

import qualified Streamly.Coreutils.Directory as Directory
import qualified Streamly.Coreutils.FileTest as FileTest
import qualified Streamly.Coreutils.Ls as Ls
import qualified Streamly.Coreutils.Mkdir as Mkdir
import qualified Streamly.Coreutils.Mv as Mv
import qualified Streamly.Coreutils.Rm as Rm
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.FileIO as FileIO
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------

-- | Join two paths with a @/@, like @"$a/$b"@ in a shell.
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

infixr 5 </>

-- | The last component of a path, like the @basename@ command.
baseName :: FilePath -> FilePath
baseName p =
    case reverse (dropWhile (== '/') (reverse p)) of
        "" -> if null p then "" else "/"
        p1 -> reverse (takeWhile (/= '/') (reverse p1))

-- | Remove a suffix if the string ends with it and is not equal to it, like
-- the second argument of the @basename@ command.
dropSuffix :: String -> String -> String
dropSuffix suffix str
    | suffix `isSuffixOf` str && str /= suffix =
        take (length str - length suffix) str
    | otherwise = str

toPath :: FilePath -> IO Path
toPath = Path.fromString

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- | Like the @test@ command, symbolic links are followed. An empty path
-- fails the test.
testPath :: FileTest -> FilePath -> IO Bool
testPath _ "" = return False
testPath t p = do
    path <- toPath p
    FileTest.test path t

-- | True if the path exists, like @test -e@.
pathExists :: FilePath -> IO Bool
pathExists = testPath FileTest.pathExists

-- | True if the path is a directory, like @test -d@.
isDirectory :: FilePath -> IO Bool
isDirectory = testPath FileTest.isDir

-- | True if the path is readable by the current user, like @test -r@.
isReadable :: FilePath -> IO Bool
isReadable = testPath FileTest.isReadable

-- | True if the path is executable by the current user, like @test -x@.
isExecutable :: FilePath -> IO Bool
isExecutable = testPath FileTest.isExecutable

-------------------------------------------------------------------------------
-- Directories
-------------------------------------------------------------------------------

-- | Change the current directory of the process, like @cd@.
changeDirectory :: FilePath -> IO ()
changeDirectory p = toPath p >>= Directory.cd

-- | Create a directory and its missing parents, like @mkdir -p@.
createDirectories :: FilePath -> IO ()
createDirectories p = toPath p >>= Mkdir.mkdir (Mkdir.makeParents True)

-- | Sorted names of the entries in a directory, excluding @.@ and @..@.
-- Raise an exception if the directory cannot be read.
listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = do
    path <- toPath dir
    names <-
        Ls.ls id path
            & fmap (either id id)
            & Stream.mapMaybe Path.takeFileName
            & fmap Path.toString
            & Stream.fold Fold.toList
    return (sort names)

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

-- | Read a file as UTF-8 text.
readTextFile :: FilePath -> IO String
readTextFile p = do
    path <- toPath p
    FileIO.read path & Unicode.decodeUtf8 & Stream.fold Fold.toList

-- | Write UTF-8 text to a file, replacing the file if it exists.
writeTextFile :: FilePath -> String -> IO ()
writeTextFile p str = do
    path <- toPath p
    Stream.fromList str
        & Unicode.encodeUtf8
        & Stream.fold (FileIO.write path)

-- | Rename a file or directory, replacing an existing destination file,
-- like @mv -f@.
moveForce :: FilePath -> FilePath -> IO ()
moveForce src dst = do
    srcPath <- toPath src
    dstPath <- toPath dst
    Mv.mv (Mv.overwrite Mv.OverwriteAlways) srcPath dstPath

-- | Remove a file, ignoring a missing file, like @rm -f@.
removeFile :: FilePath -> IO ()
removeFile p = toPath p >>= Rm.rm (Rm.withForce Rm.Force)

-- | Remove a directory tree, ignoring a missing path, like @rm -rf@.
removeTree :: FilePath -> IO ()
removeTree p = toPath p >>= Rm.rm (Rm.withForce Rm.Force . Rm.recursive True)
