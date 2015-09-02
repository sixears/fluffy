{- |

Description : standard process exit codes
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

standard process exit codes

 -}

module Fluffy.Sys.Exit
  ( Die
  , die, dieInternal, dieParse, exit, exExit
  , eUsage, eUtility
  , exitAbnormal, exitIORead, exitUsage, exitUtility
  , handleDie
  )
where

-- base --------------------------------

import Data.Word          ( Word8 )
import Control.Exception  ( Exception )
import System.Exit        ( ExitCode( ExitFailure )
                          , exitSuccess, exitWith
                          )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow, handle, throwM )

-- Fluffy ------------------------------

import Fluffy.Sys.IO  ( warn )

--------------------------------------------------------------------------------

-- exit --------------------------------

-- | akin to C's _exit(int); exit process with a given value

exit :: Word8 -> IO a
exit 0 = exitSuccess
exit e = exitWith . ExitFailure . fromEnum $ e


-- exitAbnormal ------------------------

-- | exit; all worked, but got an unusual conclusion (e.g., grep found nothing)

exitAbnormal :: IO a
exitAbnormal = exit 1

-- exitUtility -------------------------

-- | exit; because a utility function (e.g., --help) was invoked

eUtility :: Word8
eUtility = 3
exitUtility :: IO a
exitUtility = exit eUtility

-- exitUsage ---------------------------

-- | exit; due to a user error invoking the program

eUsage :: Word8
eUsage = 2

exitUsage :: IO a
exitUsage = exit eUsage

-- exitIORead --------------------------

-- | exit; due to an IO read failure

exitIORead :: IO a
exitIORead = exit 4

-- exitParse ---------------------------

-- | exit; due to a parse failure

eParse :: Word8
eParse = 5

-- exitInternal ------------------------

eInternal :: Word8
eInternal = 254

-- Die -------------------------------------------------------------------------

data Die = Die Word8 String

instance Show Die where
  show (Die _ s) = s

instance Exception Die

-- die ---------------------------------

die :: MonadThrow m => Word8 -> String -> m a
die i s = throwM (Die i s)

dieParse :: MonadThrow m => String -> m a
dieParse = die eParse

dieInternal :: MonadThrow m => String -> m a
dieInternal = die eInternal

-- handleDie ---------------------------

-- | place this at the head of main to exit from Die throws, e.g.,
--
-- > main = handleDie $ do
-- >   ...

handleDie :: IO () -> IO ()
handleDie = handle h
  where h (Die i s) = exExit i s

-- exExit ------------------------------

-- | exit with an error message and a defined exit code

exExit :: Word8 -> String -> IO a
exExit i s = warn s >> exit i
