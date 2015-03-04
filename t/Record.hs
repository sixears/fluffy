{-# LANGUAGE TemplateHaskell #-}

-- test-tap ----------------------------

import Test.TAP  ( is, test )

-- this package --------------------------------------------

import Fluffy.Language.TH         ( pprintQ )
import Fluffy.Language.TH.Record  ( mkLensedRecord )
import Fluffy.Text.PCRE           ( substg )


--------------------------------------------------------------------------------

main :: IO ()
main =
  test [ is (substg "\\b(ff|o|asn|s)_\\d+\\b" "$1" . pprintQ $
               mkLensedRecord "R"
                              -- last item deliberately not prefixed with '_'
                              [ ("_i", "Int"), ("_t", "String"), ("f","Float") ]
                              [ ''Show ])
            (substg "\\b(ff|o|asn|s|t)_\\d+\\b" "$1" .
             substg "\\b(_?[iRtf])(?:_\\d)\\b" "$1" $
               pprintQ [d| data R = R { _i :: Int, _t :: String, f :: Float }
                              deriving Show
                           i ff o = fmap asn (ff $ _i o) where asn s = o { _i = s }
                           t ff o = fmap asn (ff $ _t o) where asn s = o { _t = s }
                         |])
            "mkLensedRecord"
       ]
