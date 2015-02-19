{- |

Description : standard process exit codes
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

standard process exit codes

 -}

module Fluffy.Sys.Exit
  ( exit, exitAbnormal
  , exitIORead
  , exitUsage
  , exitUtility
  )
where

import System.Exit  ( ExitCode( ExitFailure )
                    , exitSuccess, exitWith
                    )

-- exit ------------------------------------------------------------------------

-- | akin to C's _exit(int); exit process with a given value

exit :: Int -> IO a
exit 0 = exitSuccess
exit e = _exit e

_exit :: Int -> IO a
_exit = exitWith . ExitFailure

-- exitAbnormal ------------------------

-- | exit; all worked, but got an unusual conclusion (e.g., grep found nothing) 

exitAbnormal :: IO a
exitAbnormal  = _exit 1

-- exitUtility -------------------------

-- | exit; because a utility function (e.g., --help) was invoked

exitUtility :: IO a
exitUtility   = _exit 2

-- exitUsage ---------------------------

-- | exit; due to a user error invoking the program

exitUsage :: IO a
exitUsage     = _exit 3

-- exitIORead --------------------------

-- | exit; due to an IO read failure

exitIORead :: IO a
exitIORead    = _exit 4