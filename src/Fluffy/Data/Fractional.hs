module Fluffy.Data.Fractional
  ( (//), frac, idiv )
where
  
-- frac --------------------------------

{- | the fractional part of a real-like number; such that
   
   > fromInteger (truncate x) + frac x == x

-}

frac :: RealFrac a => a -> a
frac x = x - (fromInteger . truncate) x

-- | divide two int-like-things, get a real-like-thing
idiv :: (Integral a, Integral b, RealFrac c) => a -> b -> c
idiv a b = fromIntegral a / fromIntegral b
(//) :: (Integral a, Integral b, RealFrac c) => a -> b -> c
(//) = idiv

infixl 7 // -- same as /

