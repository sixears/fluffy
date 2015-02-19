#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

module Main where

-- base --------------------------------

import Data.Char         ( isPrint, ord )
import Data.List         ( elemIndex, isInfixOf, sort )
import Data.Maybe        ( catMaybes, isJust )
import System.IO.Unsafe  ( unsafePerformIO )

-- lens --------------------------------

import Control.Lens  ( both, over )

-- QuickCheck --------------------------

import Test.QuickCheck ( Arbitrary( arbitrary ), Gen, Property
                       , (==>), choose, elements, generate, listOf, resize )

-- random ------------------------------

import System.Random  ( RandomGen, mkStdGen, random )

-- Fluffy ------------------------------

import Fluffy.Data.List  ( Diff(..)
                         , breakAt, deleteSorted, diff, findEnds, findIndices
                         , isSublistOf, mSpan, splitOn2, subsequencePositions )
import Test.TAP ( check, test, is )

-- mspan -----------------------------------------------------------------------

-- ensure mSpan reconstitutes to the original string
prop_mspan_spans :: String -> Bool
prop_mspan_spans s = concat (mSpan isPrint s) == s

-- no holes
prop_mspan_contiguity :: String -> Property
prop_mspan_contiguity s = (not . null) s ==> not (any null (mSpan isPrint s))

-- ensure that p contiguously and alternately holds/not-holds for each sublist
prop_mspan_alternates :: String -> Property
prop_mspan_alternates s = (not . null) s ==>
  let ss = mSpan isPrint s
      pCheck p s' = takeWhile p s' == s'
      pAlt' _ [] = True
      pAlt' p (t : ts) = pCheck p t && pAlt' (not . p) ts
      pAlt p xs = if p (head (head xs))
                  then pAlt' p xs
                  else pAlt' (not . p) xs
   in pAlt isPrint ss

-- findEnds --------------------------------------------------------------------

prop_findEnds_isInfix :: String -> String -> Bool
prop_findEnds_isInfix xs ys =
  isJust (xs `findEnds` ys) == xs `isInfixOf` ys

prop_findEnds_reconstitute :: String -> String -> String -> Bool
prop_findEnds_reconstitute xs pfx sfx = let ys = pfx ++ xs ++ sfx
                                         in case xs `findEnds` ys of
                                           Just (s,e) -> s ++ xs ++ e == ys
                                           Nothing      -> False

-- splitOn2 --------------------------------------------------------------------

prop_splitOn2_rebuild :: String -> String -> Bool
prop_splitOn2_rebuild xs ys = case splitOn2 xs ys of
                                ("", as) -> as == ys
                                (as, "") -> as == ys
                                (as, bs) -> as ++ xs ++ bs == ys

prop_splitOn2_infix :: String -> String -> Bool
prop_splitOn2_infix xs ys | null xs           = -- this is depended on, e.g., in
                                                -- Fluffy.Getopt.setvalAList
                                                splitOn2 xs ys == ("", ys)
                          | xs `isInfixOf` ys = let (as, bs) = splitOn2 xs ys
                                                 in    not (xs `isInfixOf` as)
                                                    && as ++ xs ++ bs == ys
                          | otherwise         = splitOn2 xs ys == (ys, "")

-- isSublistOf -----------------------------------------------------------------

is_sub :: Eq a => [a] -> [a] -> Bool
is_sub [] _ = True
is_sub _ [] = False
is_sub (x:xs) ys =
  case elemIndex x ys of
    Nothing      -> False
    Just x_index -> is_sub xs (drop (x_index+1) ys)

-- we deliberately don't always generate a sublist of a string because
-- xs `isSublistOf` ys clearly isn't a precondition, so we want to test
-- the negative case as much as the positive case
prop_isSublistOf :: String -> String -> Bool
prop_isSublistOf xs ys = isSublistOf xs ys == is_sub xs ys

-- findIndices -----------------------------------------------------------------

randBool :: Bool
randBool = unsafePerformIO . generate $ choose (False,True)

genSublist :: RandomGen g => g -> [a] -> (g, [a])
genSublist g [] = (g, [])
genSublist g (x:xs) = case random g of
                    (True , g') -> let (g'', xs') = genSublist g' xs
                                    in (g'', x:xs')
                    (False, g') -> genSublist g' xs

-- maybe we should use a specific String type with an Arbitrary implementation
-- here, but for my purposes genSublist should do
prop_findIndices :: String -> Bool
prop_findIndices ys = 
  let xs  = snd $ genSublist (mkStdGen . sum $ fmap ord ys) ys
      is_ = findIndices xs ys
   in fmap (ys !!) is_ == xs

-- deleteSorted ----------------------------------------------------------------

dropIndices :: [a] -> [Int] -> [a]
dropIndices =
  dropIndices_ 0
  where dropIndices_ _ []     _       =  []
        dropIndices_ _ as     []      =  as
        dropIndices_ n (a:as) (i:iis)  =  if i == n
                                         then dropIndices_ (n+1) as iis
                                         else dropIndices_ (n+1) (a:as) (i:iis)

prop_deleteSorted :: String -> Bool
prop_deleteSorted ys = 
  let xs  = snd $ genSublist (mkStdGen . sum $ fmap ord ys) ys
      is_ = deleteSorted xs ys
      is' = fmap (ys !!) $ findIndices xs ys
   in    is_ `isSublistOf` ys
      && sort (is_ ++ is') == sort ys

-- diff ------------------------------------------------------------------------

prop_diff :: String -> String -> Bool
prop_diff xs ys  =  let diffs             = diff xs ys
                        mapdf (First a)    = Just a
                        mapdf (Second _)   = Nothing
                        mapdf (Both a _)   = Just a
                        mapdf (Change a _) = Just a
                        mapds (First _)    = Nothing
                        mapds (Second a)   = Just a
                        mapds (Both _ a)   = Just a
                        mapds (Change _ a) = Just a
                        xs' = catMaybes $ fmap mapdf diffs
                        ys' = catMaybes $ fmap mapds diffs
                     in xs' == xs && ys' == ys

-- | generate a string being a list of "quick" interleaved with "", "a", "aa",
--   etc.; along with the start, end positions of the word "quick" within the
--   string

-- subSequences ----------------------------------------------------------------

data SubSeq = SubSeq String [(Int,Int)]
  deriving Show

instance Arbitrary SubSeq where
  arbitrary = genSubStrings

genSubStrings :: Gen SubSeq
genSubStrings = do
  let count = 5
  strs <- resize 3 . listOf $ elements ["quick"]
  as   <- resize (count+1) . listOf . elements $ fmap (`replicate` 'a') [0..3]
  let zs = zip as strs
      -- the start,end positions of each "quick" in the output string
      positions = scanl (\ (_,b) (c,d) -> (b+c, b+c+d)) (0,0)
--                     $ fmap (\ (a,b) -> (length a, length b)) zs
                    $ fmap (over both length) zs
  -- return $ SubSeq (concatMap (\ (a,s) -> a ++ s) zs) (tail positions)
  return $ SubSeq (uncurry (++) =<< zs) (tail positions)

prop_subsequences :: SubSeq -> Bool
prop_subsequences (SubSeq s pos) =
  subsequencePositions "quick" s == fmap fst pos

-- breakAt ---------------------------------------------------------------------

prop_breakAt_reconstitute :: String -> Char -> Bool
prop_breakAt_reconstitute xs x = if x `elem` xs
                                 then let (h,t) = breakAt x xs
                                       in h ++ [x] ++ t == xs
                                 else (xs,[]) == breakAt x xs

prop_breakAt_firstElem :: String -> Char -> Bool
prop_breakAt_firstElem xs x = x `notElem` fst (breakAt x xs)
  
--------------------------------------------------------------------------------

main :: IO()
main =
  test [ check prop_mspan_spans            "prop_mspan_spans"
       , check prop_mspan_contiguity       "prop_mspan_contiguity"
       , check prop_mspan_alternates       "prop_mspan_alternates"
       , check prop_findEnds_isInfix       "prop_findEnds_isInfix"
       , check prop_findEnds_reconstitute  "prop_findEnds_reconstitute"
       , check prop_splitOn2_rebuild       "prop_splitOn2_rebuild"
       , check prop_splitOn2_infix         "prop_splitOn2_infix"
       , check prop_isSublistOf            "prop_isSublistOf"
       , check prop_findIndices            "prop_findIndices"
       , check prop_deleteSorted           "prop_deleteSorted"
       , check prop_diff                   "prop_diff"
       , check prop_subsequences           "prop_subsequences"
       , check prop_breakAt_reconstitute   "prop_breakAt_reconstitute"
       , check prop_breakAt_firstElem      "prop_breakAt_firstElem"
       

       , is (isSublistOf "" "") True "isSublistOf \"\""
       , is (isSublistOf "t" "t") True "isSublistOf t"
       , is (isSublistOf "" "t") True "isSublistOf \"\" t"
       , is (isSublistOf "t" "") False "isSublistOf t \"\""
       , is (isSublistOf "Toms" "Thomase") True "isSublistOf Toms"
       , is (isSublistOf "Thomas" "Toms") False "isSublistOf Thomas"
       , is (findIndices "" "")            []        "findIndices \"\""
       , is (findIndices "t" "t")          [0]       "findIndices t"
       , is (findIndices "" "t")           []        "findIndices \"\" t"
       , is (findIndices "t" "")           []        "findIndices t \"\""
       , is (findIndices "Toms" "Thomase") [0,2,3,5] "findIndices Toms"
       , is (findIndices "Thomas" "Toms")  [0]       "findIndices Thomas"
       ]
