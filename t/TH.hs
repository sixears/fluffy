#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

{-# LANGUAGE TemplateHaskell #-}

-- template-haskell --------------------

import Language.Haskell.TH  ( Exp( LitE, VarE )
                            , Lit( IntegerL, StringL )
                            )

-- local packages ------------------------------------------

-- test-tap ----------------------------

import Test.TAP  ( is, ok, test )

-- Fluffy ------------------------------

import Fluffy.Language.TH  ( catchQ, checkBoolString
                           , composeApE
                           , construct, constructQ
                           , mComposeE, stringE
                           )

import TH_Helper

----------------------------------------

data D = D Int String deriving ( Show, Eq )

d :: D
d = $( return $ construct 'D [LitE $ IntegerL 7, LitE $ StringL "bar"] )

d' :: D
d' = $( constructQ 'D [ [| 8 |] , [| "baz" |] ] )

e :: Int
e = $( return $ composeApE (VarE 'length) (stringE "foo") )

main :: IO()
main = do
  cQ1 <- $( cq1 >>= catchQ )
  cQ2 <- $( cq2 >>= catchQ )
  cQ3 <- $( cq3 >>= catchQ )

  cBS1 <- $( checkBoolString [| bs1 |] flumpet )
  cBS2 <- $( checkBoolString [| bs2 |] flumpet )
  cBS3 <- $( checkBoolString [| bs3 |] flumpet )
  cBS4 <- $( checkBoolString [| bs4 |] flumpet )

  let mce = $(return . mComposeE $ fmap VarE [ 'length, 'tail, 'head ])

  test [ ok (d == D 7 "bar")                                         "construct"
       , ok (d' == D 8 "baz" )                                      "constructQ"

       , is cQ1  (True,  "")                                               "cQ1"
       , is cQ2  (False, "")                                               "cQ2"
       , is cQ3  (False, "Elumpit")                                        "cQ3"

       , is cBS1 (Just "flumpet:\nbob")                                   "cBS1"
       , is cBS2 (Just "flumpet:\nbob")                                   "cBS2"
       , is cBS3 Nothing                                                  "cBS3"
       , is cBS4 (Just "flumpet")                                         "cBS4"

       , is (mce [[1 :: Int,2,3],[4,5,6]]) 2                               "mce"

       , is e 3                                                     "composeApE"
       ]
