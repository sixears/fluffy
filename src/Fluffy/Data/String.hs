{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeSynonymInstances
 #-}

{- | 

Description : String-specific utility functions
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

String-specific utility functions

 -}

module Fluffy.Data.String
  ( TrOptions
  , chomp, lc, lcfirst, lines'
  , trim, trimBegin, trimEnd
  , uc, ucfirst, unlines'
  )
where

import Data.Char      ( isSpace, toLower, toUpper )
import Data.Default   ( Default( def ) )
import Data.List      ( dropWhileEnd, intercalate )

import Fluffy.Data.List  ( stripBy )

-- TrOptions -------------------------------------------------------------------

-- | options for controlling the action of transliterate; currently none
data TrOptions = TrOptions {
                 } deriving (Show)

instance Default TrOptions where
  def = TrOptions

-- uc --------------------------------------------------------------------------

-- | capitalize all letters
uc :: String -> String
uc = fmap toUpper

-- lc --------------------------------------------------------------------------

-- | lowercase all letters
lc :: String -> String
lc = fmap toLower

-- ucfirst ---------------------------------------------------------------------

-- | capitalize first letter

ucfirst :: String -> String
ucfirst []     = []
ucfirst (c:cs) = toUpper c : cs

-- lcfirst ---------------------------------------------------------------------

-- | lowercase first letter

lcfirst :: String -> String
lcfirst []     = []
lcfirst (c:cs) = toLower c : cs

-- lines' ----------------------------------------------------------------------

lines' :: String -> [String]

-- | like lines, but never returns an empty list

lines' [] = [""]
lines' s  = lines s

-- unlines' --------------------------------------------------------------------

-- | like unlines, but without the trailing newline

unlines' :: [String] -> String
unlines' = intercalate "\n"

-- trim ------------------------------------------------------------------------

-- | trim a string to n places, replacing with another string if needed
--   e.g., ensure a string is no more than 80 chars, replacing end with '...'
--   if required:  trim 80 "..." s

-- trim :: Int -> String -> String -> String
-- trim n r s = if length s > n
--              then take (n - length r) s ++ r
--              else s

-- trimBegin -------------------------------------------------------------------

-- | remove whitespace from the end of a string

trimBegin :: String -> String
trimBegin = dropWhile isSpace

-- trimEnd ---------------------------------------------------------------------

-- | remove whitespace from the end of a string

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

-- trim ------------------------------------------------------------------------

-- | remove whitespace from start & end of a string

trim :: String -> String
trim = stripBy isSpace

-- chomp -----------------------------------------------------------------------

-- | remove all carriage returns & newlines from string end

chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse

--------------------------------------------------------------------------------
