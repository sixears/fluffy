{-# LANGUAGE TemplateHaskell #-}

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( Lens' )

-- local packages ------------------------------------------

-- test-tap ----------------------------

import Test.TAP  ( is, test )

-- this package ------------------------

import Fluffy.Language.TH         ( pprintQ )
import Fluffy.Language.TH.Record  ( mkLensedRecord, mkLensedRecordDef )
import Fluffy.Text.PCRE           ( substg )


--------------------------------------------------------------------------------

main :: IO ()
main =
  test [ is (substg "\\b(ff|o|asn|s)_\\d+\\b" "$1" . pprintQ $
               mkLensedRecord "R"
                              -- last item deliberately not prefixed with '_'
                              [ ("_i", "Int")
                              , ("_t", "Maybe String")
                              , ("f","Float") 
                              ]
                              [ ''Show ])
            (substg "\\b(ff|o|asn|s|t)_\\d+\\b" "$1" .
             substg "\\b(_?[iRtf])(?:_\\d)\\b" "$1" $
               pprintQ [d| data R = R { _i :: Int
                                      , _t :: Maybe String
                                      , f  :: Float 
                                      }
                              deriving Show

                           i :: Lens' R Int
                           i ff o = fmap asn (ff $ _i o)
                             where asn s = o { _i = s }

                           t :: Lens' R (Maybe String)
                           t ff o = fmap asn (ff $ _t o)
                             where asn s = o { _t = s }
                         |])
            "mkLensedRecord"
       , is (substg "\\b(ff|o|asn|s)_\\d+\\b" "$1" . pprintQ $
               mkLensedRecordDef "R"
                              -- last item deliberately not prefixed with '_'
                              [ ("_i", "Int"   , [| 7 |])
                              , ("_t", "Maybe String", [| "foo" |])
                              , ("c" , "Char"  , [| 'w' |])
                              ]
                              [ ''Show ])
            (substg "\\b(ff|o|asn|s|t)_\\d+\\b" "$1" .
             substg "Data.Default.Class.def" "def"   .
             substg "\\b(_?[iRtc])(?:_\\d)\\b" "$1" $
               pprintQ [d| data R = R { _i :: Int
                                      , _t :: Maybe String
                                      , c  :: Char }
                             deriving Show

                           instance Default R where
                             def = R 7 "foo" 'w'

                           i :: Lens' R Int
                           i ff o = fmap asn (ff $ _i o)
                             where asn s = o { _i = s }

                           t :: Lens' R (Maybe String)
                           t ff o = fmap asn (ff $ _t o)
                             where asn s = o { _t = s }
                         |])
            "mkLensedRecordDef"
       ]
