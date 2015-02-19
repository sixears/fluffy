#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

import Test.TAP          ( is, test )
import Fluffy.Text.PCRE  ( subst, substg )

--------------------------------------------------------------------------------

main :: IO()
main =
  test [ is ("^Type.readType \"(.*)\" \"(.*)\" :: \\1" `subst` "$2 $1" 
               $ "Type.readType \"Double\" \"1.1\" :: Double")     
            "1.1 Double"                                               "subst 1"
         
       , is ("readType \"(.*)\" (?:\"(y.*)\")?.* :: \\1" `subst` "[$2]" 
               $ "Type.readType \"Double\" \"x\" :: Double")
            "Type.[]"                                 "subst 2 (optional group)"
         
       , is (("b(\\S+)" `subst` "$1") "foo bar baz") "foo ar baz"
                                                        "subst 3 (before after)"
       , is (("oo" `subst` "aa") "fooboo") "faaboo"     "subst 3 (repeat match)"
       , is (("oo" `substg` "aa") "fooboo") "faabaa"    "substg 1 (repeat match)"
       , is (("oo" `substg` "aa") "foobee") "faabee" "substg 3 (no repeat match)"
       ]
