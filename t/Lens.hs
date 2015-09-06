{-# LANGUAGE TemplateHaskell #-}

import Control.Lens  ( Lens', (&), (^.), (.~), makeLenses )

import Fluffy.Control.Lens  ( (~:~), (++=), (=++)
                            , convLens, lempty, prefix, prepend )
import Test.TAP             ( is, test )

data X = X { _s :: String }

$( makeLenses ''X )

x :: X
x = X "lock"

data Y = Y { _cent :: Float }

$( makeLenses ''Y )

y :: Y
y = Y 19

faren :: Lens' Y Float
faren = convLens ((+32) . (/5) . (*9)) ((/9) . (*5) . (\f -> f - 32)) cent

main :: IO ()
main =
  test [ is (prefix  s x 'c'     ^. s) "clock"   "prefix"
       , is ((x &   s ~:~ 'b')   ^. s) "block"   "~:~"
       , is (prepend s x "bul"   ^. s) "bullock" "prepend"
       , is ((x &   s ++= "hem") ^. s) "lockhem" "++="
       , is ((x &   s =++ "hem") ^. s) "hemlock" "=++"
       , is ((x `lempty` s) ^. s)      ""        "lempty"
       , is ((y & faren .~ 68) ^. cent) 20 "faren->cent"
       , is ((y & cent .~ 20) ^. faren) 68 "cent->faren"
       ]
