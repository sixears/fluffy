{-# LANGUAGE FlexibleInstances
           , TemplateHaskell 
           , TypeSynonymInstances 
  #-}

{- |

Description : helper functions for working with Template Haskell
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

helper functions for working with Template Haskell

 -}

module Fluffy.Language.TH
  ( Expish(..), Typeish(..)
  , assign, assignN, catchQ, checkBoolString, composeE, construct, constructQ
  , fnApply, fnApplyQ
  , intE, listOfN, mAppE, mAppEQ, mComposeE
  , nameE, nameEQ, pprintQ, stringE, stringEQ
  , tsArrows, tupleL
  )
where

-- base --------------------------------

import qualified Control.Exception as Ex

import System.IO.Unsafe  ( unsafePerformIO )

-- template-haskell --------------------

import Language.Haskell.TH  ( Body  ( NormalB                                 )
                            , Dec   ( ValD                                    )
                            , Exp   ( AppE, ConE, InfixE, LitE, VarE          )
                            , Lit   ( IntegerL, StringL                       )
                            , Name
                            , Pat   ( VarP                                    )
                            , Type  ( AppT, ArrowT, ConT, ListT, TupleT, VarT )
                            , ExpQ, PatQ
                            , appE, bindS, caseE, conE, conP
                            , doE, listE, litP, match, mkName
                            , newName, noBindS, normalB, pprint, runQ
                            , tupE, tupP, varE, varP, wildP
                            )

-- Fluffy ------------------------------

import Fluffy.Text.PCRE  ( subst )

-- Typeish ---------------------------------------------------------------------

-- | a thing with a natural conversion to a Type

class Typeish a where
  toType :: a -> Type

instance Typeish Type where
  toType = id

instance Typeish Name where
  toType = ConT

-- assign ----------------------------------------------------------------------

-- | 'assignN', taking a string as first arg

assign :: String -> Exp -> Dec
assign name = assignN (mkName name)

-- assignN ---------------------------------------------------------------------

-- | assign a (function?) name to a (function?) body

assignN :: Name -> Exp -> Dec
assignN name body = ValD (VarP name) (NormalB body) []

-- Expish ----------------------------------------------------------------------

-- | a thing with a natural conversion to an Exp

class Expish a where
  toExp :: a -> Exp
  
instance Expish Exp where
  toExp = id
  
instance Expish Name where
  toExp = VarE
  
instance Expish String where
  toExp = VarE . mkName

-- fnApply ---------------------------------------------------------------------

-- | apply a fn to a list of arguments

-- e.g., fnApply (VarE 'mkOpt) [chars, strings, doc, type, ...]
-- fnApply :: Exp -> [Exp] -> Exp
-- fnApply = foldl AppE
fnApply :: Expish e => e -> [Exp] -> Exp
fnApply = foldl AppE . toExp

-- fnApplyQ --------------------------------------------------------------------

-- | apply a fn in Q context to a list of non-Q arguments

fnApplyQ :: ExpQ -> [Exp] -> ExpQ
fnApplyQ fq es = fq >>= \f -> return $ fnApply f es

-- construct -------------------------------------------------------------------

{- | construct a Datum with a Data Constructor, in TH world

     thus, e.g., given
   >   data D = D Int String deriving (Show)

     then
   > d = $( return $ construct 'D [(LitE $ IntegerL 7), (LitE $ StringL "bar")] )

     is equivalent to
   > d = D 7 "bar"

-}

-- runQ [| D 7 "foo" |]
--   ==>
-- AppE (AppE (ConE 'D) (LitE (IntegerL 7))) (LitE (StringL "foo"))

construct :: Name -> [Exp] -> Exp
construct dname = foldl AppE (ConE dname)

-- | like 'construct'; but in Q monad for easier direct splicing; e.g.,
--
-- > d' = $( constructQ 'D [ [| 7 |] , [| "bar" |] ] )

constructQ :: Name -> [ExpQ] -> ExpQ
constructQ dname valsQ =
  sequence valsQ >>= \vals -> return $ construct dname vals

-- nameE -----------------------------------------------------------------------

-- | a literal string as a Name (fn name, for example)

nameE :: String -> Exp
nameE = VarE . mkName

-- nameEQ ----------------------------------------------------------------------

-- | 'nameE' in Q context

nameEQ :: String -> ExpQ
nameEQ = return . nameE

-- intE ------------------------------------------------------------------------

-- | a literal integer as an Exp

intE :: Integral a => a -> Exp
intE = LitE . IntegerL . toInteger

-- stringE ---------------------------------------------------------------------

-- | a literal string as an Exp

stringE :: String -> Exp
stringE = LitE . StringL

-- stringEQ --------------------------------------------------------------------

-- | 'stringE' in Q context

stringEQ :: String -> ExpQ
stringEQ = return . stringE

-- stringP ---------------------------------------------------------------------

-- | literal string as a pattern

stringP :: String -> PatQ
stringP = litP . StringL

-- mAppE -----------------------------------------------------------------------

-- | multi-appE; apply apply apply.  Typically the first thing is a function
--   e.g., a VarE '(++) or a data constructor e.g., ConE 'Just.
mAppE :: [Exp] -> Exp
mAppE [] = error "mAppE on an empty list"
mAppE (e:es) = foldl AppE e es

-- mAppEQ ----------------------------------------------------------------------

-- | 'mAppE' in Q context

mAppEQ :: [ExpQ] -> ExpQ
mAppEQ = fmap mAppE . sequence -- fmap == liftM

-- composeE --------------------------------------------------------------------

-- | compose two expressions using @.@

composeE :: Exp -> Exp -> Exp
composeE a b = InfixE (Just a) (VarE '(.)) (Just b)

-- mComposeE -------------------------------------------------------------------

-- | Produce a chain of function compositions; e.g., a . b . c from [a,b,c].
--   See tests for example usage.

mComposeE :: [Exp] -> Exp
mComposeE = foldr1 composeE

-- catchQ ----------------------------------------------------------------------

{- | produce an expression that evaluates a fn (which is of type IO Bool),
     and returns the bool value; but catches any exception and returns the
     string value of the exception

     Exp ( :: IO Bool) -> ExpQ ( :: IO (Bool, String) )
-}

catchQ :: Exp -> ExpQ
catchQ f = do
--  e <- newName "e"
--  g <- newName "g"
  handler  <- [| (\e -> return (False, show e))
                 :: Ex.SomeException -> IO (Bool, String) |]
  returnOK <- [| \g -> return (g, "") |]

  return $ -- Ex.catch (f >>= returnOK) handler
    AppE (AppE (VarE 'Ex.catch) (AppE (AppE (VarE '(>>=)) f) returnOK)) handler

-- checkBoolString -------------------------------------------------------------

-- let f = return (False, "") :: IO (Bool, String);
-- runQ [| do (b,s) <- f; case (b,s) of
--                          (True , "") -> return Nothing;
--                          (False, "") -> return (Just "flum");
--                          (_    , s') -> return (Just s') |]
-- ===>
-- DoE [ BindS (TupP [ VarP b, VarP s ])
--             (VarE f)
--     , NoBindS (CaseE (TupE [ VarE b, VarE s ])
--                      [ Match (TupP [ ConP 'True [] , LitP (StringL "") ])
--                              (NormalB (AppE (VarE 'return) (ConE 'Nothing)))
--                              []
--                      , Match (TupP [ ConP 'False [], LitP (StringL "") ])
--                              (NormalB (AppE (VarE 'return)
--                                             (AppE (ConE 'Just)
--                                                   (LitE (StringL "flum")))
--                                       ))
--                              []
--                      , Match (TupP [ WildP, VarP s' ])
--                              (NormalB (AppE (VarE 'return)
--                                             (AppE (ConE 'Just) (VarE s'))))
--                              []
--                      ])
--     ]

-- :t \ f df -> do (b,s) <- f; case (b,s) of
--                               (True, "")  -> return Nothing;
--                               (False, "") -> return (Just df);
--                               (_, s')     -> return (Just s')
--  :: Monad m  => m (Bool, String) -> String -> m (Maybe String)

checkBoolString :: ExpQ -> ExpQ -> ExpQ
 -- Exp( m (Bool, String) ) -> Exp( String )-> Q Exp( Maybe String )

-- > let f = return (True, "bob") :: IO (Bool, String);
-- > $( checkBoolString (VarE $ mkName "f") (LitE $StringL "flumpet") )
-- ===> Just "bob"

-- > let f = return (True, "") :: IO (Bool, String);
-- > $( checkBoolString (VarE $ mkName "f") (LitE $StringL "flumpet") )
-- ===> Nothing

-- > let f = return (False, "") :: IO (Bool, String);
-- > $( checkBoolString (VarE $ mkName "f") (LitE $StringL "flumpet") )
-- ===> Just "flumpet"

{- | take an IO( Bool, String ) and a string; if the string part
     (hypothetically: the text of an exception) is non-null, return that (as a
     Just) prepended by the default exception text; else if the
     bool part is true, return Nothing; else return the default exception text

     $checkBoolString f df -> case $f of
                                 (True , "") -> return Nothing
                                 (False, "") -> return $ Just df
                                 (_    , s)  -> return $ Just s
-}

checkBoolString f df = do
   b  <- newName "b"
   s  <- newName "s"
   s' <- newName "s'"
   let cc = -- concat [ df, "\n:", s' ]
            appE (varE 'concat) (listE [ df, stringEQ ":\n", varE s' ])
     -- do (b,s) <- f
   doE [ bindS (tupP [ varP b, varP s ]) f
         -- case (b,s) of
       , noBindS (caseE (tupE [ varE b, varE s ])
                          -- (True, "") -> return Nothing
                        [ match (tupP [ conP 'True [], stringP "" ])
                                (normalB (appE (varE 'return) (conE 'Nothing)))
                                []
                          -- (False, "") -> return $ Just df
                          , match (tupP [ conP 'False [], stringP "" ])
                                  (normalB (appE (varE 'return)
                                                 (appE (conE 'Just) df)))
                                  []
                          -- (_, s') -> return $ Just $ concat df, "\n:", s'
                          , match (tupP [ wildP, varP s' ])
                                  (normalB (appE (varE 'return)
                                                 (appE (conE 'Just) cc)))
                                  []
                          ])
         ]

-- pprintQ ---------------------------------------------------------------------

-- | 'show' for ExpQ; replaces instances of readType "(.*)" "(.*)" :: \1 with $2

pprintQ :: ExpQ -> String
pprintQ =
    subst (    "\\(Fluffy.Language.TH.Type.readType\\s+\"(.*)\"\\s+::"
            ++ "\\s+GHC.Base.String\\s+->\\s+\\1\\)\\s+\"(.*)\"")
          "$2"
  . subst "Fluffy.Language.TH.Type.readType \"(.*)\" \"(.*)\" :: \\1" "$2"
  . pprint . unsafePerformIO . runQ

-- tsArrows --------------------------------------------------------------------

-- | given [ a,b,c,... ]; form a type sig for a -> b -> c -> ...
tsArrows :: Typeish t => [t] -> Type

tsArrows = foldr1 (\ a b -> AppT (AppT ArrowT a) b) . fmap toType

-- tupleL ----------------------------------------------------------------------

-- | construct a tuple type from a list of names

tupleL :: Typeish t => [t] -> Type
tupleL = _tupleL . fmap toType
  where _tupleL (t0:ts@(_:_)) = foldl AppT (AppT (TupleT (1 + length ts)) t0) ts
        _tupleL ts            = error ("too few names; a tuple must have >= 2 ("
                               ++ show ts ++ ")")

-- listOfN ---------------------------------------------------------------------

-- | given a name t, return a Type for [t]
listOfN :: Name -> Type
listOfN = AppT ListT . VarT
