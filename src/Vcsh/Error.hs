-- |
-- Module      : Vcsh.Error
-- License     : GPL-2.0-or-later
--
-- Exceptions that terminate vcsh with a message and an exit code. The exit
-- codes are listed in @doc/error_codes.md@.

module Vcsh.Error
    ( Failure (..)
    , fatal
    , die
    , orFatal
    )
where

import Control.Exception (Exception, IOException, catch, throwIO)

-- | A failure that terminates the program.
data Failure =
      Fatal String Int
      -- ^ Printed as @vcsh: fatal: <message>@, exits with the code.
    | Die String
      -- ^ Printed as is, exits with code 1.
    deriving Show

instance Exception Failure

-- | Throw an exception to terminate the program with a fatal error message
-- and the given exit code.
fatal :: String -> Int -> IO a
fatal msg code = throwIO (Fatal msg code)

-- | Throw an exception to terminate the program with the given message and
-- exit code 1.
die :: String -> IO a
die msg = throwIO (Die msg)

-- | Run an action and, if it throws an 'IOException', throw an exception
-- to terminate the program with the given fatal error message and exit
-- code.
orFatal :: IO a -> (String, Int) -> IO a
orFatal action (msg, code) =
    action `catch` \(_ :: IOException) -> fatal msg code
