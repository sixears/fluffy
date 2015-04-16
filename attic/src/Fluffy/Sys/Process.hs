module Fluffy.Sys.Process
  ( getProgBaseName, getProgFullName )
where

-- | you should probably be using getProgName from System.Environment rather 
--   than this

-- base --------------------------------
  
import Control.Monad  ( liftM )
import Data.List      ( isSuffixOf )
  
-- filepath ----------------------------

import System.FilePath.Posix   ( (</>), dropFileName, takeFileName )

-- unix --------------------------------

import System.Posix.Files      ( readSymbolicLink )
import System.Posix.Directory  ( getWorkingDirectory )

-- this package ------------------------

import Fluffy.Data.List  ( splitOn )

--------------------------------------------------------------------------------

readExe :: IO FilePath
readExe = readSymbolicLink "/proc/self/exe"

-- | you should probably be using getProgName from System.Environment rather 
--   than this

getProgBaseName :: IO String
getProgBaseName = do
  full_name <- getProgFullName
  return (reverse $ takeWhile (/= '/') $ reverse full_name)

hsFromGHCCmdLine :: IO FilePath
hsFromGHCCmdLine = do
  cmdline <- (liftM ((splitOn '\0'))) (readFile "/proc/self/cmdline")
  cwd     <- getWorkingDirectory
  let hss = filter (isSuffixOf ".hs") cmdline
  if null hss
  then (liftM dropFileName) readExe >>= return . (</> "ghc")
  else return $ cwd </> (head hss)

getProgFullName :: IO FilePath
getProgFullName = do
  exe <- readExe
  if (takeFileName exe) == "ghc" then hsFromGHCCmdLine else return exe
