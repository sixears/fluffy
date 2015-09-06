{-# LANGUAGE Rank2Types #-}

{-|

Description : ancillary functions for working with lenses
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

ancillary functions for working with lenses

 -}

module Fluffy.Control.Lens
  ( (~:~), (++=), (=++), convLens, lempty, prefix, prepend )
where

-- base --------------------------------

import Control.Monad  ( MonadPlus, mplus )

-- lens --------------------------------

import Control.Lens   ( Lens', (%~), (&), (.~), (^.), set )

--------------------------------------------------------------------------------

-- | for some Lens' l focussed unto a list y, cons new value x onto y
prefix :: Lens' s [a] -> s -> a -> s
prefix l s a = (l %~ (a :)) s

-- e.g.,
--   empty_hbscan & unparsed ~:~ (tLeaf "bob")
--   HBInfo { _unparsed = [+ "bob"], ... }

-- | a cons setter, in the style of (?~), but we can't call it (:~) because prefix
--   : means a data constructor.  Use as <data> & <lens> ~:~ <value> to prepend
--   <value> onto the target of the lens.
(~:~) :: MonadPlus m => Lens' s (m a) -> a -> s -> s
l ~:~ a = l %~ (return a `mplus`)

-- | for some Lens' l focussed unto a list y, prepend (using ++) new list
--   x onto y
prepend :: Lens' s [a] -> s -> [a] -> s
prepend l s a = (l %~ (a ++)) s

-- | infix version of suffix; e.g., x = X "ba"; x & s ++= "ab"; x == X "baab"
(++=) :: Lens' s [a] -> [a] -> s -> s
l ++= a = l %~ (++a)
-- | infix version of prefix; e.g., x = X "ba"; x & s =++ "ab"; x == X "abba"
(=++) :: Lens' s [a] -> [a] -> s -> s
l =++ a = l %~ (a++)

-- | set a list lens to empty
lempty :: s -> Lens' s [a] -> s
lempty st l = st & l .~ []

-- lensLens ----------------------------

-- | provide a conversion on top of a lens, by applying 'to' and 'from' fns
convLens :: (a -> b) -> (b -> a) -> Lens' x a -> Lens' x b
-- type Lens' s a    = Lens s s a a
-- type Lens s t a b = Functor f => (a -> f b) -> s -> f t
convLens aToB bToA l functor p =
  ((p &) . set l . bToA) <$> functor (aToB (p ^. l))
