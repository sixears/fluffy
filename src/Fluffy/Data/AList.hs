{-|

Description : functions for association lists
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

functions for association lists
 
 -}

module Fluffy.Data.AList
  ( alist_by_key, alist_dups )
where
  
-- containers --------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set

-- lens --------------------------------

import Control.Lens  ( over, _2 )

-- alist_by_key ----------------------------------------------------------------

-- | group items in an alist by common key

alist_by_key :: (Ord k, Ord v) => -- Ord k because of Map k v
                                -- Ord v because of Set v
              [(k, v)] -> [(k, [v])]
alist_by_key as =
  let -- as' is as with singleton set values
      -- as'   :: [(k, Set.Set v)]
      as'    = over (traverse._2) Set.singleton as
      -- d is a map from key to a set of values
      -- d     :: Map.Map k (Set.Set v)
      d      = Map.fromListWith (flip Set.union) as'
      -- dups is the set of keys that have two or more values
      -- dups  :: [(k, Set.Set v)]
   in over (traverse._2) Set.toList (Map.toList d)

-- alist_dups ------------------------------------------------------------------

-- | find the dup keys in an alist
alist_dups :: (Ord k, Ord v) => -- Ord k because of Map k v
                                -- Ord v because of Set v
              [(k, v)] -> [(k, [v])]
alist_dups = filter (\ (_,b) -> 1 < length b) . alist_by_key

-- that's all, folks! ----------------------------------------------------------
