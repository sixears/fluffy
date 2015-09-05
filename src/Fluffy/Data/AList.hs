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

-- base --------------------------------

import Data.Function  ( on )
import Data.List      ( groupBy, sortBy )
import Data.Ord       ( comparing )
-- import Data.Tuple     ( fst )

-- containers --------------------------

-- import qualified Data.Map as Map
-- import qualified Data.Set as Set

-- lens --------------------------------

import Control.Lens  ( _1, view )

-- alist_by_key ----------------------------------------------------------------

-- | group items in an alist by common key

alist_by_key :: (Ord k) => [(k, v)] -> [(k, [v])]
alist_by_key =
  map squeesh . groupBy ((==) `on` view _1) . sortBy (comparing $ view _1)
  where squeesh ((k,v0) : kvs) = (k, v0 : map snd kvs)
        squeesh []             = error "squeesh on an empty list"

-- alist_dups ------------------------------------------------------------------

-- | find the dup keys in an alist
alist_dups :: (Ord k, Ord v) => -- Ord k because of Map k v
                                -- Ord v because of Set v
              [(k, v)] -> [(k, [v])]
alist_dups = filter (\ (_,b) -> 1 < length b) . alist_by_key

-- that's all, folks! ----------------------------------------------------------
