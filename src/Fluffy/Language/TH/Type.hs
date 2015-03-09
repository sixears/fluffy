{-# LANGUAGE TemplateHaskell #-}

{- |

Description : helper functions for working with types in Template Haskell
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

helper functions for working with types in Template Haskell

 -}

module Fluffy.Language.TH.Type
  ( readT, readTS, readType, strToT )
where

-- base --------------------------------

import Data.Char      ( isAlpha )
import Text.Printf    ( printf )

-- template-haskell --------------------

import Language.Haskell.TH  ( Exp   ( AppE, LamE, SigE
                                    , VarE )
                            , Pat   ( VarP )
                            , Type  ( AppT, ConT, ListT )
                            , ExpQ
                            , mkName, newName
                            )

--------------------------------------------------------------------------------

-- runQ $ [t| Bool |] -->
--   ConT GHC.Bool.Bool

-- runQ $ [t| [Bool] |] -->
--   AppT ListT (ConT GHC.Bool.Bool)

-- runQ $ [t| Maybe Bool |] -->
--   AppT (ConT Data.Maybe.Maybe) (ConT GHC.Bool.Bool)

-- runQ $ [t| Either Bool Int |] -->
--   AppT (AppT (ConT Data.Either.Either) (ConT GHC.Bool.Bool))
--        (ConT GHC.Types.Int)

-- runQ [t| Show b => b -> IO b |] -->
--   ForallT [PlainTV b_1]
--           [ClassP GHC.Show.Show [VarT b_1]]
--           (AppT (AppT ArrowT (VarT b_1))
--                 (AppT (ConT GHC.Types.IO) (VarT b_1)))

-- runQ [d| x :: b -> IO b; x = undefined |] -->
--   [ SigD x (ForallT [PlainTV b_0]
--                     []
--                     (AppT (AppT ArrowT (VarT b_0))
--                           (AppT (ConT GHC.Types.IO) (VarT b_0))
--                     )
--            )
--   , ValD (VarP x) (NormalB (VarE GHC.Err.undefined)) []
--   ]]


-- runQ [d| foo :: Bool; foo = True |] -->
--   [ SigD foo (ConT GHC.Bool.Bool)
--   , ValD (VarP foo) (NormalB (ConE GHC.Bool.True)) []
--   ]

-- runQ [d| foo :: [Bool]; foo = [True] |] -->
--   [ SigD foo (AppT ListT (ConT GHC.Bool.Bool))
--   , ValD (VarP foo) (NormalB (ListE [ConE GHC.Bool.True])) []
--   ]

-- runQ [d| foo :: Maybe Bool; foo = Just True |] -->
--   [ SigD foo (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Bool.Bool))
--   , ValD (VarP foo)
--          (NormalB (AppE (ConE Data.Maybe.Just) (ConE GHC.Bool.True)))
--          []
--   ]

-- runQ [d| foo :: Either Bool Int; foo = Left True |] -->
--   [ SigD foo (AppT (AppT (ConT Data.Either.Either) (ConT GHC.Bool.Bool))
--                    (ConT GHC.Types.Int))
--   , ValD (VarP foo)
--          (NormalB (AppE (ConE Data.Either.Left) (ConE GHC.Bool.True)))
--          []
--   ]

-- | convert a string to a type, handling also [x] as list x and ?x as Maybe x

-- strToT "Int"   --> ConT Int
-- strToT "[Int]" --> AppT ListT (ConT Int)
-- strToT "?Int"  --> AppT (ConT Data.Maybe.Maybe) (ConT Int)

strToT :: String -> Type

strToT ('[' : str)
  | last str == ']' = AppT ListT (strToT (init str))
  | otherwise       = error $ "trailing ']' not found: '[" ++ str ++ "'"
strToT ('?' : str)  = AppT (ConT ''Maybe) (strToT str)
strToT str
  | not (null str) && isAlpha (head str)
               = let (a,b) = span (\c -> isAlpha c || c `elem` "._") str
                  in case dropWhile (== ' ') b of
                       "" -> ConT $ mkName a
                       b' -> AppT (ConT $ mkName a) (strToT b')
  | otherwise  = error $ "cannot parse type '" ++ str ++ "'"

--------------------------------------------------------------------------------

-- runQ [| read "7" :: Int |] -->
--   SigE (AppE (VarE Text.Read.read) (LitE (StringL "7"))) (ConT GHC.Types.Int)

-- (\s t -> runQ [| read s :: strToT t |]) "7" "Int" -->
--   SigE (AppE (VarE Text.Read.read) (ListE [LitE (CharL '7')]))
--        (ForallT [PlainTV strToT_2,PlainTV t_3]
--                 []
--                 (AppT (VarT strToT_2) (VarT t_3)))

-- let s = "7"; let t = "Int"; runQ [| read s :: (strToT t) |] -->
--   SigE (AppE (VarE Text.Read.read) (VarE s_1629866126))
--        (ForallT [PlainTV strToT_0,PlainTV t_1]
--                 []
--                 (AppT (VarT strToT_0) (VarT t_1)))

{- | take a string s and a type name t, do read s :: t.
     Uses strToT for type names
-}

-- let x = readQ "True" "Bool"; $x -->
--   True

{- | take a typename, and produces a 'read' exp that is of type
     String -> Type
-}
readT :: String -> ExpQ
readT t = do
  s <- newName "s"
  r <- [| readType t |]
  return $ LamE [VarP s] (SigE (AppE r (VarE s)) (strToT t))

-- | take a typename t and a string s, 'read' the s of type t as a Q Exp

readTS :: String -> String -> ExpQ
readTS t s = [| readType t s |] >>= \e -> return $ SigE e (strToT t)

{- | Read a string i with type name t, with more informative errors than read.
     Note that the type of the returned value is (as for read) determined by
     the calling context; the initial string argument is used purely as a name
     to use in error messages
-}

readType :: (Show a, Read a) => String -> String -> a
readType t s =
  case reads s of
    [(v, "")] -> v
    [(v, x)] -> error $ printf "garbage found after %s: '%s'" (show v) x
    []       -> error $ printf "failed to parse '%s' as %s" s t
    l        -> error $ printf "ambiguous parse of '%s': [%s]" s (show l)
