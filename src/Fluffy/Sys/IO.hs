{- |

Description : miscellaneous I/O functions
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

miscellaneous I/O functions

 -}

module Fluffy.Sys.IO
  ( devnullRO, ePutStrLn, ePutStrLns, putLn, touch, warn )
where

-- base --------------------------------
  
import Control.Monad  ( void )
import System.IO      ( Handle, IOMode( ReadMode, WriteMode )
                      , hPutStrLn, openFile , stderr )

--------------------------------------------------------------------------------

-- touch -------------------------------

-- | create an empty file; will nuke an existing file

touch :: FilePath -> IO ()
touch fn = void $ openFile fn WriteMode

-- devnull -----------------------------

-- | @/dev/null@

devnull :: String
devnull = "/dev/null"

-- devnullRO ---------------------------

-- | open @/dev/null@ RO

devnullRO :: IO Handle
devnullRO = openFile devnull ReadMode

-- ePutStrLn ---------------------------

-- | write line (with an added newline) to stderr

ePutStrLn :: String -> IO()
ePutStrLn  = hPutStrLn stderr

-- warn --------------------------------

-- | write line (with an added newline) to stderr

warn :: String -> IO()
warn  = hPutStrLn stderr

-- ePutStrLns --------------------------

-- | write lines (each with an added newline) to stderr

ePutStrLns :: [String] -> IO()
ePutStrLns = mapM_ (hPutStrLn stderr)

-- putLn -------------------------------

-- | write a newline to stdout

putLn :: IO ()
putLn = putStrLn ""

-- that's all, folks! ----------------------------------------------------------
