{-# LANGUAGE FlexibleContexts #-}

module Fluffy.Data.Time
  ( timeFormatDuration, timeFormatHHMM, timeFormatHuman, timeScanDuration, toHHMM )
where

-- base --------------------------------

import Control.Applicative  ( optional )
import Data.Maybe           ( catMaybes )
import Data.Word            ( Word32 )
import Text.Printf          ( printf )

-- lens --------------------------------

import Control.Lens  ( (^.), _1 )

-- regex -------------------------------

import Text.Regex.Applicative         ( (=~), string )
import Text.Regex.Applicative.Common  ( decimal )

-- time --------------------------------

import Data.Time.Clock  ( DiffTime )
import Data.Time.Format ( FormatTime, defaultTimeLocale, formatTime )

--------------------------------------------------------------------------------

-- toHHMM ------------------------------

toHHMM :: Integral a => a -> (a, a)
toHHMM s = (m `quot` 60, m `rem` 60)
           where m = s `quot` 60

-- timeFormatHuman ---------------------

timeFormatHuman :: FormatTime t => t -> String
timeFormatHuman = formatTime defaultTimeLocale "%a %b %e %l:%M%P %Z"

-- timeFormatHHMM ----------------------

timeFormatHHMM :: FormatTime t => t -> String
timeFormatHHMM  = formatTime defaultTimeLocale "%l:%M%p"

-- timeFormatDuration ------------------

-- | format a duration (# seconds) in terms of days/hours/mins/secs as necessary

timeFormatDuration :: Integral n => n -> String
timeFormatDuration dur = let s' = toInteger dur
                             (m', s) = s' `quotRem` 60
                             (h', m) = m'  `quotRem` 60
                             (d,  h) = h'  `quotRem` 24
                             dhms    = [(d,'d'),(h,'h'),(m,'m'),(s,'s')]
                             trim0   = dropWhile ((== 0) . (^. _1))
                             times   = reverse $ trim0 $ reverse $ trim0 dhms
                             pp :: (Integer,Char) -> String
                             pp (i,c) = printf "%02d%c" i c
                          in case times of
                               []         -> "0s"
                               (x,c) : [] -> show x ++ [c]
                               (x,c) : xs -> show x ++ [c] ++ concatMap pp xs

-- timeScanDuration ------------------------------------------------------------

timeScanDuration :: (Num n) => String -> Maybe n
timeScanDuration s =
  let l4 a b c d = sum $ catMaybes [a,b,c,d]
      re = l4 <$> (optional (fmap (*(24*60*60)) (decimal <* string "d")))
              <*> (optional (fmap (*(60*60))    (decimal <* string "h")))
              <*> (optional (fmap (*60)         (decimal <* string "m")))
              <*> (optional                     (decimal <* string "s"))
   in s =~ re
--------------------------------------------------------------------------------
