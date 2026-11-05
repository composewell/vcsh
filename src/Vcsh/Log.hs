-- |
-- Module      : Vcsh.Log
-- License     : GPL-2.0-or-later
--
-- Messages printed by vcsh, each prefixed with the program name and the
-- message kind, e.g. @vcsh: info: ...@.

module Vcsh.Log
    ( Log (..)
    , info
    , errorMsg
    , verbose
    , debug
    , failureMsg
    )
where

import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import Vcsh.Error (Failure(..))

-- | Program name and the verbosity flags.
data Log = Log
    { logSelf :: String
    , logVerbose :: Bool
    , logDebug :: Bool
    }

prefixed :: Log -> String -> String -> String
prefixed lg kind msg = logSelf lg ++ ": " ++ kind ++ ": " ++ msg

-- | Print an informational message on stdout.
info :: Log -> String -> IO ()
info lg = putStrLn . prefixed lg "info"

-- | Print an error message on stderr.
errorMsg :: Log -> String -> IO ()
errorMsg lg = hPutStrLn stderr . prefixed lg "error"

-- | Print a message on stdout in verbose or debug mode.
verbose :: Log -> String -> IO ()
verbose lg msg =
    when (logVerbose lg || logDebug lg) $ putStrLn (prefixed lg "verbose" msg)

-- | Print a message on stdout in debug mode.
debug :: Log -> String -> IO ()
debug lg msg = when (logDebug lg) $ putStrLn (prefixed lg "debug" msg)

-- | The message to print on stderr for a 'Failure', and the exit code.
failureMsg :: Log -> Failure -> (String, Int)
failureMsg lg (Fatal msg code) = (prefixed lg "fatal" msg, code)
failureMsg _ (Die msg) = (msg, 1)
