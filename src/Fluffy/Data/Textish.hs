{-# LANGUAGE FlexibleInstances    #-}

{-|

Description : types that may be converted from a Data.Text.Lazy.Builder
Copyright   : (c) Martyn J. Pearce 2015
License     : BSD
Maintainer  : haskell@sixears.com

class for types that may be converted from a Data.Text.Lazy.Builder

 -}

module Fluffy.Data.Textish
  ( Textish( tpack ) )
where

-- text --------------------------------

import qualified  Data.Text.Lazy  as  LazyText
import qualified  Data.Text       as  StrictText

import Data.Text.Lazy.Builder  ( Builder, toLazyText )

-- Textish ---------------------------------------------------------------------

-- | Textish represents a type that may be converted from a
--   `Data.Text.Lazy.Builder`

class Textish t                   where  tpack :: Builder -> t
instance Textish Builder          where  tpack = id
instance Textish LazyText.Text    where  tpack = toLazyText
instance Textish StrictText.Text  where  tpack = LazyText.toStrict . toLazyText
instance Textish String           where  tpack = LazyText.unpack . toLazyText

