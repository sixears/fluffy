{-# LANGUAGE TemplateHaskell #-}

import Control.Lens  ( (&), (^.), makeLenses )

import Fluffy.Control.Lens  ( (~:~), (++=), (=++), lempty, prefix, prepend )
import Test.TAP             ( is, test )

data X = X { _s :: String }

$( makeLenses ''X )

x :: X
x = X "lock"

main :: IO ()
main =
  test [ is (prefix  s x 'c'     ^. s) "clock"   "prefix"
       , is ((x &   s ~:~ 'b')   ^. s) "block"   "~:~"
       , is (prepend s x "bul"   ^. s) "bullock" "prepend"
       , is ((x &   s ++= "hem") ^. s) "lockhem" "++="
       , is ((x &   s =++ "hem") ^. s) "hemlock" "=++"
       , is ((x `lempty` s) ^. s)      ""        "lempty"
       ]
