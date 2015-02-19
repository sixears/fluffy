{-# LANGUAGE TemplateHaskell #-}

module TH_Helper where

import Language.Haskell.TH         ( stringE )
import Language.Haskell.TH.Syntax  ( Q, Exp )

bs1 :: IO (Bool, String)
bs1 = return (True, "bob") 

bs2 :: IO (Bool, String)
bs2 = return (False, "bob")

bs3 :: IO (Bool, String)
bs3 = return (True, "")    

bs4 :: IO (Bool, String)
bs4 = return (False, "")   


cq1 :: Q Exp
cq1 = [| return True  :: IO Bool |]   

cq2 :: Q Exp
cq2 = [| return False :: IO Bool |]   

cq3 :: Q Exp
cq3 = [| error "Elumpit" :: IO Bool |]


flumpet :: Q Exp
flumpet = stringE "flumpet"

