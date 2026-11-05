-- |
-- Module      : Vcsh.Process
-- License     : GPL-2.0-or-later
--
-- Running external commands, implemented using streamly-process.
--
-- Before starting a process, stdout and stderr are flushed so that the
-- output of vcsh and of the process appear in the order of execution.

module Vcsh.Process
    (
    -- * Process configuration
      Modifier
    , inEnv
    , inDir

    -- * Running processes
    , runForeground
    , readStdout
    , readStdoutQuiet

    -- * Parsing output
    , splitNul
    )
where

import Control.Exception (IOException, catch, try)
import Data.Function ((&))
import Streamly.Data.Stream (Stream)
import System.Exit (ExitCode(..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Streamly.Internal.System.Process (Config, ProcessFailure(..))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.System.Process as Process
import qualified Streamly.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Process configuration
-------------------------------------------------------------------------------

-- | Modifies the configuration of a process to be started.
type Modifier = Config -> Config

-- | Run the process with exactly the given environment.
inEnv :: [(String, String)] -> Modifier
inEnv env = Process.setEnv (Just env)

-- | Run the process in the given working directory.
inDir :: FilePath -> Modifier
inDir dir = Process.setCwd (Just dir)

-------------------------------------------------------------------------------
-- Running processes
-------------------------------------------------------------------------------

flushStdio :: IO ()
flushStdio = hFlush stdout >> hFlush stderr

-- | Run a process with stdin, stdout and stderr inherited from vcsh, and
-- wait for it to exit. If the executable cannot be started, print the
-- error on stderr and return exit code 127, like a shell.
runForeground :: Modifier -> FilePath -> [String] -> IO ExitCode
runForeground modifier cmd args = do
    flushStdio
    Process.foreground modifier cmd args `catch` notFound

    where

    notFound (e :: IOException) = do
        hPutStrLn stderr (cmd ++ ": " ++ show e)
        return (ExitFailure 127)

toExitCode :: ProcessFailure -> ExitCode
toExitCode (ProcessFailure code) = ExitFailure code

collect :: Stream IO Char -> IO (ExitCode, String)
collect chars = do
    r <- try (Stream.fold Fold.toList chars)
    return $ either (\e -> (toExitCode e, "")) (\s -> (ExitSuccess, s)) r

-- | Run a process and return its exit code and its stdout as UTF-8 text.
-- stderr is inherited from vcsh. The output is empty if the process fails.
readStdout :: Modifier -> FilePath -> [String] -> IO (ExitCode, String)
readStdout modifier cmd args = do
    flushStdio
    Process.toBytesWith modifier cmd args
        & Unicode.decodeUtf8
        & collect

-- | Like 'readStdout' but discard stderr, like @2> /dev/null@.
readStdoutQuiet :: Modifier -> FilePath -> [String] -> IO (ExitCode, String)
readStdoutQuiet modifier cmd args = do
    flushStdio
    Process.toChunksEitherWith modifier cmd args
        & Stream.catRights
        & Unicode.decodeUtf8Chunks
        & collect


-------------------------------------------------------------------------------
-- Parsing output
-------------------------------------------------------------------------------

-- | Split NUL separated output, e.g. of @git ls-files -z@ or @env -0@. A
-- trailing NUL does not produce an empty last element.
splitNul :: String -> [String]
splitNul s =
    case break (== '\0') s of
        ("", []) -> []
        (x, []) -> [x]
        (x, _:rest) -> x : splitNul rest
