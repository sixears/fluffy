{-# LANGUAGE Rank2Types #-}

{- |

Description : miscellaneous list fns
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

miscellaneous list fns

 -}

module Fluffy.Data.List
  ( Diff(..)
  , breakAt, deleteSorted, diff, findEnds, findIndices, isSublistOf, mSpan
  , spanEnds, splitBy, splitBy2, splitOn, splitOn2, splitOnL
  , stripBy, stripOn, stripPre, subsequencePositions, tr1, zipWithL
  )
where

-- base --------------------------------

import Data.List   ( isInfixOf, isPrefixOf )

-- Diff --------------------------------

import qualified Data.Algorithm.Diff as DAD

-- lens --------------------------------

import Control.Lens  ( (&), (%~), _2, over )

-- safe --------------------------------

import Safe  ( tailSafe )

--------------------------------------------------------------------------------

-- splitBy ---------------------------------------------------------------------

-- | split on a predicate over individual elements, dropping all elements that
--   satisfy the predicate

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f l@(x:xs)
  | f x       = splitBy f xs
  | otherwise = h : splitBy f t
                where (h,t) = break f l

-- splitBy2 --------------------------------------------------------------------

{- | split a list into the initial segment that doesn't satisfy p; and
     the trailing segment after any portion that does satisfy p
 -}

splitBy2 :: (a -> Bool) -> [a] -> ([a], [a])
splitBy2 p xs = over _2 (dropWhile p) $ break p xs

-- splitOn ---------------------------------------------------------------------

{- | split a list on a given possible element, returning the non-empty
     contiguous sublists
-}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = splitBy (==x)

{- | split on sublist into 2 pieces

     split a list into start/end by the first occurrence of sublist, such that
     @null /= str ==> str = start ++ sublist ++ end@

     Note the precondition; splitting on @[]@ always gives @([], [])@ which is
     indistinguishable from @splitOn2 l l@.
-}
splitOn2 :: Eq a => [a] -> [a] -> ([a],[a])
splitOn2 sublist list =
  let pfx _ []                            = []
      pfx u ss@(s:s') | u `isPrefixOf` ss = []
                      | otherwise         = s : pfx u s'
      p = pfx sublist list
   in (p, drop (length p + length sublist) list)

{- | split a list by a possible sublist, returning all the contiguous sublists.
     consecutive lists give rise to empty sublists in the return, e.g.,
     @splitOnL ":" "foo::bar" == ["foo", "", "bar"]@
-}
splitOnL :: Eq a => [a] -> [a] -> [[a]]
-- split on [] gives each individual element as its own list
splitOnL [] []     = [[]]
splitOnL [] (a:as) = [a] : splitOnL [] as

splitOnL sublist list
  | sublist `isInfixOf` list = a : splitOnL sublist b
  | otherwise                = [list]
  where (a,b) = splitOn2 sublist list

-- stripBy ---------------------------------------------------------------------

-- | strip items that match some predicate from both ends of a string

stripBy :: (a -> Bool) -> [a] -> [a]
stripBy p = reverse . dropWhile p . reverse . dropWhile p

-- stripOn ---------------------------------------------------------------------

-- | strip instances of a specific item from both ends of a string

stripOn :: Eq a => a -> [a] -> [a]
stripOn = stripBy . (==)

-- stripPre --------------------------------------------------------------------

{- | use the first list as a list of items; strip any consecutive such items
     from the second list
-}
stripPre :: Eq a => [a] -> [a] -> [a]
stripPre = dropWhile . flip elem

-- breakAt -----------------------------

{- | split a list at a single point equalling some value, do not include that
     value in the return (that is, 'tail' the second list - if the value appears
     multiple times, the subsequent appearances in the list are left untouched).
-}
breakAt :: Eq a => a -> [a] -> ([a], [a])
-- breakAt x xs = apSnd safeTail $ break (==x) xs
breakAt x xs = break (==x) xs & _2 %~ tailSafe

-- mSpan -------------------------------

{- | split a list into segments, each the longest possible contiguous segment
     either satisying P or not satisfying P; thus alternating
 -}

mSpan :: (a -> Bool) -> [a] -> [[a]]
mSpan p xs' =
  case span p xs' of
    (xs, []) -> [xs] -- terminating condition comes first!  else mSpan p "" would
                     -- never terminate
    ([], xs) -> mSpan (not . p) xs
    (hs, ts) -> hs : mSpan (not . p) ts

-- findEnds --------------------------------------------------------------------

-- | xs `isInfixOf` ys => pfx ++ xs ++ sfx == ys
--   where Just (pfx, sfx) = findEnds xs ys

findEnds :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
findEnds [] ys = Just ([], ys)
findEnds _  [] = Nothing
findEnds xs ys = if xs `isPrefixOf` ys
                  then Just ([], drop (length xs) ys)
                  else case findEnds xs (tail ys) of
                         Just (s,e) -> Just (head ys : s, e)
                         Nothing    -> Nothing

-- spanEnds ---------------------------------------------------------------------

{- | 'span', for both ends; divide a list into three pieces; being the initial
     contiguous segment that satisfies some p, the bit in the middle, and the
     tail contiguous segment that satisfies some p
 -}

spanEnds :: (a -> Bool) -> [a] -> ([a], [a], [a])
spanEnds p xs = let (start, mid_end) = span p xs
                    (end, r_mid)     = span p (reverse mid_end)
                 in (start, reverse r_mid, reverse end)

-- tr1 -------------------------------------------------------------------------

-- | replace all instances of x with y across a list

tr1 :: (Eq a) => a -> a -> [a] -> [a]
tr1 x y = fmap (\z -> if x == z then y else z)

-- zipWithL --------------------------------------------------------------------

-- | like zipWith, but uses a zip that is as long as the longer list
--   by mapping to Maybe, with Nothing filling out any spaces

zipWithL :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithL f as bs = if length as > length bs
             then zipWith f (fmap Just as) (fmap Just bs ++ repeat Nothing)
             else zipWith f (fmap Just as ++ repeat Nothing) (fmap Just bs)

-- isSublistOf -------------------------------------------------------------

isSublistOf :: (Eq a) => [a] -> [a] -> Bool

-- | xs `isSublistOf` ys iff every member x of xs is found in ys, in order

[]     `isSublistOf` _       =  True
_      `isSublistOf` []      =  False
(x:xs) `isSublistOf` (y:ys)  =  if x == y
                                   then xs `isSublistOf` ys
                                   else (x:xs) `isSublistOf` ys

-- findIndices -----------------------------------------------------------------

-- | find indices of a sublist within a list.  xs `isSublistOf` ys is
--   a precondition.

findIndices :: Eq a => [a] -> [a] -> [Int]

findIndices xs' ys'  =
  findIndices_ xs' (zip ys' [0..])
  where findIndices_ [] _  =  []
        findIndices_ _ []  =  []
        findIndices_ (x:xs) (y:ys)  =  if x == fst y
                                       then snd y : findIndices_ xs ys
                                       else findIndices_ (x:xs) ys

-- deleteSorted ----------------------------------------------------------------

-- | remove sublist elements from a list; iff the sublist elements appear in
--   order

deleteSorted :: Eq a => [a] -> [a] -> [a]
deleteSorted [] ys  =  ys
deleteSorted _  []  =  []
deleteSorted (x:xs) (y:ys)  =  if x == y
                               then deleteSorted xs ys
                               else y : deleteSorted (x:xs) ys

-- diff ------------------------------------------------------------------------

-- | description of differences between two lists (note: positional; this is
--   the difference of two lists, not two sets)

data Diff a = First a     -- ^ item appears in the first list only
            | Second a    -- ^ item appears in the second list only
            | Both a a    -- ^ item appears in both lists
            | Change a a  -- ^ item in first list is changed for item in second
                          --   list
  deriving (Eq, Show)

-- | generate a list of differences between xs & ys

diff :: Eq a => [a] -> [a] -> [Diff a]
diff xs ys =
  collapse Nothing $ DAD.getDiff xs ys
  where
    -- take a list of DAD.Diffs, convert to Diffs, spotting patterns of
    -- n -x- First followed by n -x- Second; collapsing those into n -x- Change

    collapse :: Eq a => Maybe (Either [a] [a]) -> [DAD.Diff a] -> [Diff a]

    collapse_ (Just (Left  firsts))   =  fmap First  firsts
    collapse_ (Just (Right seconds))  =  fmap Second seconds
    collapse_ Nothing                 =  []

    collapse__  (DAD.First  a : ds)  = collapse (Just $ Left [a]) ds
    collapse__  (DAD.Second a : ds)  = collapse (Just $ Right [a]) ds
    collapse__  (DAD.Both a b : ds)  = Both a b : collapse Nothing ds
    collapse__  []                   = error "collapse__: empty list"

    collapse pending [] = collapse_ pending

    collapse Nothing            ds  =  collapse__ ds
    collapse (Just (Left []))   ds  =  collapse__ ds
    collapse (Just (Right []))  ds  =  collapse__ ds

    collapse (Just (Left   firsts))  (DAD.First a : ds)  =
      collapse (Just $ Left (firsts ++ [a])) ds
    collapse (Just (Right  seconds)) (DAD.Second a : ds)  =
      collapse (Just $ Right (seconds ++ [a])) ds
    collapse pending (DAD.Both a b : ds) =
      collapse_ pending ++ [Both a b] ++ collapse Nothing ds

    collapse (Just (Left (f:fs)))  (DAD.Second a : ds)  =
      Change f a : collapse (Just $ Left fs) ds
    collapse (Just (Right (s:ss)))  (DAD.First a : ds)  =
      Change a s : collapse (Just $ Right ss) ds

-- subsequencePositions --------------------------------------------------------

-- | find the indices of (non-overlapping) instances of a sequence xs in ys;
--   e.g., @"abab" `subsequencePositions` "fabababab" ==> [1,5]@

subsequencePositions :: Eq a => [a] -> [a] -> [Int]
subsequencePositions =
  ssPs 0
  where ssPs i [] ys = [i..length ys-1]
        ssPs _ _  [] = []
        ssPs i xs ys = if xs `isPrefixOf` ys
                       then i : ssPs (i+length xs) xs (drop (length xs) ys)
                       else ssPs (i+1) xs (tail ys)

--------------------------------------------------------------------------------
