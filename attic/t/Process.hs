-- base --------------------------------

import Control.Monad  ( unless, when )
import Data.List      ( isSuffixOf )

-- unix --------------------------------

import System.Posix.Files  ( fileAccess, fileExist )

-- this package ------------------------

import Fluffy.Sys.Exit     ( exit )
import Fluffy.Sys.Process  ( getProgBaseName, getProgFullName )

--------------------------------------------------------------------------------


main :: IO()
main = do
  full_name <- getProgFullName
  base_name <- getProgBaseName
  putStrLn base_name
  when   (null base_name)                     $ exit 7
  unless (base_name == "process")             $ exit 4
  putStrLn full_name
  when   (null full_name)                     $ exit 8
  unless ("/process" `isSuffixOf` full_name)  $ exit 5
  unless ('/' == head full_name)              $ exit 6
  
  let do_unless io b = unless b io
  fileExist full_name >>= do_unless (exit 9)
  let file_executable fn = fileAccess fn True False True
  file_executable full_name >>= do_unless (exit 10)