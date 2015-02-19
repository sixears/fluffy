{- |

Description : helper functions for working with regexen
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

helper functions for working with regexen

 -}
module Fluffy.Text.Regex
  ( reFold, rePair, reTriple )
where

-- base --------------------------------
  
import Debug.Trace  ( trace )

-- regex-applicative -------------------

import Text.Regex.Applicative  ( RE, findLongestPrefix )

--------------------------------------------------------------------------------

-- reFold ----------------------------------------------------------------------

-- | apply a list of regex-based transformers to a starting point to allow
--   each to possibly transform the thing

reFold :: Show s => p               -- ^ initial object
                 -> [RE s (p -> p)] -- ^ regex transformers
                 -> [s]             -- ^ thing to match against
                 -> (p, [s])        -- ^ result object, and any leftover
reFold p res s =
  foldl f (p, s) res
  where -- f :: (p, [s]) -> RE s (p -> p) -> (p, [s])
        f (p, ss) r = case findLongestPrefix r ss of
                        Just (pt, ss') -> (pt p, ss')
                        Nothing        -> (p, ss)

-- rePair ----------------------------------------------------------------------

{- | sequentially apply a pair of regexen, looking for longest matches; return a
     a pair of results.  if the first regex matches, then the second is tried on 
     any leftovers.
-}

rePair :: (RE c t1, RE c t2) -> [c] -> (Maybe (t1, Maybe t2), [c])
rePair (r1, r2) s =
  case findLongestPrefix r1 s of
    Just (t1, s1) -> case findLongestPrefix r2 s1 of
                       Just (t2, s2) -> (Just (t1, Just t2), s2)
                       Nothing       -> (Just (t1, Nothing), s1)
    Nothing       -> (Nothing, s)


-- reTriple --------------------------------------------------------------------

{- | sequentially apply a triple of regexen, looking for longest matches; return
     a pair of results.  if the first regex matches, then the second is tried on 
     any leftovers; if the second matches, then the third is tried on any 
     leftovers from the second
-}

reTriple ::    (RE c t1, RE c t2, RE c t3) -> [c] 
            -> (Maybe (t1, Maybe (t2, Maybe t3)), [c])
reTriple (r1, r2, r3) s = 
  case findLongestPrefix r1 s of
    Just (t1, s1) -> 
      case findLongestPrefix r2 s1 of
        Just (t2, s2) -> 
          case findLongestPrefix r3 s2 of 
            Just (t3, s3) -> (Just (t1, Just (t2, Just t3)), s3)
            Nothing       -> (Just (t1, Just (t2, Nothing)), s2)
        Nothing       -> (Just (t1, Nothing), s1)
    Nothing       -> (Nothing, s)
