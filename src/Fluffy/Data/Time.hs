{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}


module Fluffy.Data.Time
  ( FDurOpts(..)
  , dhms, formatDuration, formatDuration'
  , timeFormatHHMM, timeFormatHuman, scanDuration, toHHMM
  )
where

-- base --------------------------------

import Control.Applicative  ( optional )
import Data.Bool            ( bool )
import Data.List            ( dropWhileEnd )
import Data.Maybe           ( catMaybes, isJust )
import Data.Monoid          ( (<>) )
import Data.Word            ( Word8 )
import Numeric.Natural      ( Natural )

-- data-default ------------------------

import Data.Default  ( Default ( def ) )

-- lens --------------------------------

import Control.Lens  ( (^.), _1, _2, mapped, over )

-- regex -------------------------------

import Text.Regex.Applicative         ( (=~), string, sym )
import Text.Regex.Applicative.Common  ( decimal )

-- text --------------------------------

import Data.Text.Lazy.Builder  ( Builder, singleton )

-- text-format -------------------------

import Data.Text.Format  ( left )

-- time --------------------------------

import Data.Time.Format ( FormatTime, defaultTimeLocale, formatTime )

-- local imports -------------------------------------------

-- fluffy ------------------------------

import Fluffy.Data.Textish  ( Textish( tpack ) )

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

-- formatDuration ----------------------

-- | Options for formatting durations
data FDurOpts = FDurOpts { noRHSTrim :: Bool -- ^ don't trim zeroes from the RHS
                         }

instance Default FDurOpts where
  def = FDurOpts { noRHSTrim = False }

tbuilderDuration :: Integral n => FDurOpts -> n -> Builder
tbuilderDuration opts dur =
  let (d,h,m,s) = dhms ((fromIntegral . abs) dur)
      dhms'    = (d,'d') : over (mapped . _1) fromIntegral [(h,'h'),(m,'m'),(s,'s')]
      fstZero  = ((== 0) . (^. _1))
      trim0    = dropWhile fstZero
      trim0End = bool (dropWhileEnd fstZero) id (noRHSTrim opts)
      times    = (trim0End . trim0) dhms'
      pp  :: Int -> (Natural,Char) -> Builder
      pp n (i,c) =    left n '0' (fromIntegral i :: Int)
                   <> singleton c
   in (if dur < 0 then "-" else "") `mappend`
      case times of
        []         -> "0s"
        (x,c) : xs -> mconcat $ pp 1 (x,c) : fmap (pp 2) xs

-- | Format a duration (# seconds) in terms of days/hours/mins/secs as
--   necessary.  Output format is something like 7h07m; insignificant trailing
--   zero portions (e.g., 00m00s) will not be output; likewise for zero heads
--   (e.g., 0d).  Numbers will be 0-padded to 2 digits, except for the leading
--   leftmost number which will not be padded.  Thus 7d07h07m is a valid output.

formatDuration :: (Integral n, Textish t) => n -> t
formatDuration = tpack . tbuilderDuration def

formatDuration' :: (Integral n, Textish t) => FDurOpts -> n -> t
formatDuration' opts = tpack . tbuilderDuration opts

dhms :: Natural -> (Natural, Word8, Word8, Word8)
dhms s' = let qr x y = over _2 fromIntegral $ x `quotRem` y 
              (m', s) = s' `qr` 60
              (h', m) = m' `qr` 60
              (d,  h) = h' `qr` 24
           in (d, h, m, s)


-- scanDuration ------------------------

scanDuration :: (Num n) => String -> Maybe n
scanDuration s =
  let l4 m a b c d = -- (if isJust m then ((-1) *) else (1*)) $
                     bool (1*) ((-1) *) m . sum $
                     catMaybes [a,b,c,d]
      re = l4 <$> fmap isJust (optional (sym '-'))
              <*> optional (fmap (*(24*60*60)) (decimal <* string "d"))
              <*> optional (fmap (*(60*60))    (decimal <* string "h"))
              <*> optional (fmap (*60)         (decimal <* string "m"))
              <*> optional                     (decimal <* string "s")
   in s =~ re

--------------------------------------------------------------------------------
