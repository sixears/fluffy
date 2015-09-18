{-# LANGUAGE ScopedTypeVariables #-}

import Fluffy.Data.AList  ( alist_by_key )

-- base --------------------------------

import Data.List  ( group, sort )

-- lens --------------------------------

import Control.Lens  ( _1, _2, view )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck as QC

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [alist_props]

uniqs :: Ord a => [a] -> [a]
uniqs = map head . group . sort

uniqKeys :: Ord a => [(a,b)] -> [a]
uniqKeys = uniqs . map fst

findVals :: (Eq a) => a -> [(a, b)] -> [b]
findVals k = map (view _2) . filter ((==k) . view _1)

alist_props :: TestTree
alist_props = testGroup "alist props"
  [
    QC.testProperty "alist keys match" $
      \xs -> uniqKeys (alist_by_key (xs :: [(Int, Int)])) == uniqKeys xs
  , QC.testProperty "alist vals match" $
      \(xs :: [(Int, Int)]) -> all (\(k,vs) -> vs == findVals k xs) (alist_by_key xs)
  ]
