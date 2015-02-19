{-# LANGUAGE FlexibleInstances -- needed for instance
                               -- Replacement [Either Int String]
                               -- else
    {-    Illegal instance declaration for `Replacement [Either Int String]'
          (All instance types must be of the form (T a1 ... an)
           where a1 ... an are *distinct type variables*,
           and each type variable appears at most once in the instance head.
           Use -XFlexibleInstances if you want to disable this.)
        In the instance declaration for `Replacement ([Either Int String])'
    -}
  #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-| 

Description : high-level functions for working with perl-compatible regular 
              expressions
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

high-level functions for working with perl-compatible regular expressions

-}

module Fluffy.Text.PCRE
  ( Replacement(..), subst, substg )
where

-- base --------------------------------

import Data.Char        ( isNumber )
import Debug.Trace      ( trace )

-- array -------------------------------

import Data.Array  ( elems )

-- regex-pcre --------------------------

import Text.Regex.PCRE  ( (=~), mrAfter, mrBefore, mrSubs )

-- Replacement -----------------------------------------------------------------

{- | like Perl's s///; perform patten substition.

     e.g., "^Type.readType \"(.*)\" \"(.*)\" :: \\1" `subst` "$2 $1"
             $ "Type.readType \"Double\" \"1.1\" :: Double"
          --> "1.1 Double"

     The substitution string, as shown, may include $nn where $nn is an integer
     $0 will give back the whole outer match.
     Note that use of a capture group reference that doesn't exist in the regex
     will cause a runtime exception.  However, an optional group that doesn't
     match (i.e., something appended with a '?') will just give an empty string.

     e.g., "readType \"(.*)\" (?:\"(y.*)\")?.* :: \\1" `subst` "[$2]"
             $ "Type.readType \"Double\" \"x\" :: Double"
            --> "[]"
-}

class Replacement a where
  replacement :: a -> [Either Int String]

instance Replacement [Either Int String] where
  replacement = id

_rplc :: String -> [Either Int String]
_rplc xs = let (ys, zs) = break (`elem` "\\$") (tail xs)
            in Right (head xs : ys) : replacement zs

instance Replacement String where
  replacement [] = []
  replacement ('\\' : i : xs) = _rplc (i : xs)
  replacement ('$' : i : xs) | isNumber i =
    let (is, ys) = span isNumber xs
     in Left (read (i : is)) : replacement ys
  replacement xs = _rplc xs

-- subst -----------------------------------------------------------------------

-- | perl's s/// in Haskell; no /g modifier, so replace only the first instance
--   of the regex found

subst :: Replacement r
      => String -- ^ the match regex
      -> r      -- ^ the replacement for matched strings
      -> String -- ^ the string to match/replace against
      -> String

subst match replace as =
  let res = as =~ match
   in case elems (mrSubs res) :: [String] of
     []   -> as
     grps -> concat [ mrBefore res
                    , replacement replace >>= \ r -> case r of
                                                       Left i  -> grps !! i
                                                       Right s -> s

                    , mrAfter res
                    ]

-- substg ----------------------------------------------------------------------

-- | like subst, but with Perl's /g modifier - i.e., replace every found
--   instance of the match regex

substg :: Replacement r => String -> r -> String -> String
substg match replace as =
  let res = as =~ match
   in case elems (mrSubs res) :: [String] of
     []   -> as
     grps -> concat [ mrBefore res
                    , replacement replace >>= \ r -> case r of
                                                       Left i  -> grps !! i
                                                       Right s -> s

                    , substg match replace (mrAfter res)
                    ]
