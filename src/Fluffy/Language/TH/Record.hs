{-# LANGUAGE ScopedTypeVariables
           , TemplateHaskell
  #-}

{- |

Description : helper functions for working with records in Template Haskell
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

helper functions for working with records in Template Haskell

 -}

module Fluffy.Language.TH.Record
  ( mkLensedRecord, mkLensedRecordDef )
where

-- base --------------------------------

import Control.Monad  ( mapM )
import Debug.Trace   ( trace )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( Lens', (^.), _1, _3, makeLenses )

-- template-haskell --------------------

import Language.Haskell.TH  ( Body    ( NormalB )
                            , Clause  ( Clause )
                            , Con     ( RecC )
                            , Dec     ( DataD, FunD, InstanceD, SigD )
                            , Exp     ( AppE, ConE, InfixE, RecUpdE, VarE )
                            , ExpQ
                            , Name
                            , Pat     ( VarP )
                            , Q
                            , Strict  ( NotStrict )
                            , Type    ( AppT, ConT )
                            , mkName, nameBase, newName
                            )

-- Fluffy ------------------------------

import Fluffy.Language.TH       ( assign, mAppE, nameE )
import Fluffy.Language.TH.Type  ( strToT )

--------------------------------------------------------------------------------

-- mkRecord --------------------------------------------------------------

-- | construct a simple record from a specification

-- > runQ [d| data Foo = Foo { _name :: String } |] -->
--     [DataD []
--            Foo_7
--            []
--            [RecC Foo_8 [(_name_9,NotStrict,ConT GHC.Base.String)]]
--            []]

mkRecord :: String             {- ^ record type name to construct.  The
                                    type and the single data constructor
                                    will both use this name.
                               -}
         -> [(String, String)] -- ^ pairs of field name, type (as string)
                               --   see 'Fluffy.Data.Type.strToT'
         -> [Name]             -- ^ class names to derive from
                               --   (e.g., ''Show)
         -> Dec

mkRecord name flds drvs =
  let qname = mkName name
   in DataD [] qname []
            [RecC qname (fmap (\(n,t) -> (mkName n, NotStrict, strToT t)) flds)]
            drvs

-- mkLensedRecord --------------------------------------------------------------

-- | make a record with lenses to access any fields whose name begins with '_'

mkLensedRecord :: String -> [(String, String)] -> [Name] -> Q [Dec]

mkLensedRecord nam flds drvs = do
  let create_record = mkRecord nam flds drvs
--  create_lenses <- sequence . fmap (mkRLens_ . tail) . filter ((== '_') . head)
--                            $ fmap fst flds  
  create_lenses <- sequence [ mkRLens_(tail fname) | (fname, ftype) <- flds, head fname == '_' ]
  return (create_record : concat create_lenses)

-- mkLensedRecordDQ ------------------------------------------------------------

-- | like mkLensedRecord, but creates a "constructor" (mk...) using defaults

mkLensedRecordDQ :: String -> String -> [(String, String, ExpQ)] -> [Name]
                 -> Q [Dec]

mkLensedRecordDQ nam mkNam flds drvs = do
  create_record <- mkRecordDQ nam mkNam flds drvs
  create_lenses <- sequence . fmap (mkRLens_ . tail) . filter ((== '_') . head)
                            $ fmap (^. _1) flds
  return (create_record ++ concat create_lenses)

-- mkLensedRecordDef -----------------------------------------------------------

-- | like mkLensedRecord, but creates an instance of Default using
--   defaults

mkLensedRecordDef :: String -> [(String, String, ExpQ)] -> [Name] -> Q [Dec]

mkLensedRecordDef nam flds drvs = do
  let (fnams, ftyps, fdfltsq) = unzip3 flds
  fdflts :: [Exp] <- sequence fdfltsq

  let create_record = mkRecordDef nam (zip3 fnams ftyps fdflts) drvs
  create_lenses <- sequence . fmap (mkRLens_ . tail) . filter ((== '_') . head)
                            $ fmap (^. _1) flds
  return (create_record ++ concat create_lenses)

-- mkDef -----------------------------------------------------------------------

{- | create a "constructor" fn, that is, a fn that takes several values
     and returns a fixed instance of a record; e.g., given

     data X = X { foo :: Int, bar :: String }

     mkDef "mkX" "X" [| 7 |] [| "herbert" |]

     will create a fn 'mkX' that returns a { foo = 7, bar = "herbert" }
     instance.  (Note though that the above doesn't quite work, since the
     Oxford Brackets ([| ... |]) produce ExpQs, but we need Exps).
 -}

mkDef :: String -- ^ name of "constructor" fn
      -> String -- ^ name of data constructor
      -> [Exp]  -- ^ values to build with
      -> Dec
mkDef fname cname vals = assign fname (rConstruct (mkName cname) vals)

-- mkRecordDef -----------------------------------------------------------------

-- | like mkRecord, but creates an instance of Default using defaults

mkRecordDef :: String -> [(String, String, Exp)] -> [Name] -> [Dec]
mkRecordDef nam flds drvs =
  let mk   = mkDef "def" nam (fmap (^. _3) flds)
      inst = InstanceD [] (AppT (ConT ''Default) (ConT $ mkName nam)) [mk]
   in [ mkRecord nam (fmap (\(x,y,_) -> (x,y)) flds) drvs, inst ]

-- mkRecordD -------------------------------------------------------------------

-- | like mkRecord, but creates a "constructor" (mk...) using defaults

mkRecordD :: String -> String -> [(String, String, Exp)] -> [Name] -> [Dec]
mkRecordD nam mkNam flds drvs =
  let mk = mkDef mkNam nam (fmap (^. _3) flds)
   in [ mkRecord nam (fmap (\(x,y,_) -> (x,y)) flds) drvs, mk ]


-- mkRecordDQ ------------------------------------------------------------------

-- | Q variant of mkRecordD: takes ExpQs for defaults (thus Oxford
--   Brackets ([| ... |]) work; returns a Q [Dec] thus may be spliced directly

mkRecordDQ :: String -> String -> [(String, String, ExpQ)] -> [Name]
                 -> Q [Dec]

mkRecordDQ nam mkNam flds drvs = do
  dflts :: [Exp] <- mapM (^. _3) flds
  let args :: [(String, String, Exp)] = zipWith (\(a,b,_) x -> (a,b,x)) flds dflts
  return $ mkRecordD nam mkNam args drvs

-- rConstruct ------------------------------------------------------------------

-- build a record value from a list of elements
-- tname - data constructor name
-- vals  - values for data elements to build from

-- rConstruct (mkName "myRecord") [(VarE x_0), (VarE y_1)]         -->
--   AppE (AppE (ConE (mkName "myRecord")) (VarE x_0)) (VarE y_1)  -->
--   myRecord x y

rConstruct :: Name -> [Exp] -> Exp
rConstruct tname vals = mAppE (ConE tname : vals)

-- mkRLens ---------------------------------------------------------------------

-- | make a lens for a record field

-- given
--   data Record = Record { field :: String }
-- we get
--   field :: Record -> String
-- we want
--   name :: Functor f => (String -> f String) -> Record -> f Record
-- where
--   Functor f => fmap :: (a -> b) -> f a -> f b
-- thus
--   Functor f => fmap (String -> Record) -> f String -> f Record
-- so we define
--   name ff o = fmap asn ((ff (field o)) :: f String)
--               where asn :: String -> Record
--                     asn s = o { field = s }

-- runQ [d| name ff o = fmap asn (ff $ field o)
--                       where asn s = o { field = s } |]
--   -->
--   [FunD name_0 [Clause [VarP ff_1,VarP o_2]
--                        (NormalB -- fmap asn (ff $ field o)
--                                 (AppE (AppE (VarE GHC.Base.fmap)
--                                             (VarE asn_3))
--                                       -- ff $ field o
--                                       (InfixE (Just (VarE ff_1))
--                                               (VarE '($))
--                                               (Just (AppE (VarE 'field)
--                                                           (VarE o_2))))))
--                        -- asn = o { field = s }
--                        [FunD asn_3
--                              [Clause [VarP s_4]
--                                      (NormalB -- o { field = s }
--                                               (RecUpdE (VarE o_2)
--                                               [('field, VarE s_4)]))
--                                      []]]
--                ]]

mkRLens :: Name -> Name -> Q [Dec]
mkRLens name field = do -- [d| name ff o = fmap asn (ff $ field o)
                        --                  where asn s = o { field = s } |]
  s   <- newName "s"
  o   <- newName "o"
  ff  <- newName "ff"
  asn <- newName "asn"
  let -- update (a field value)
      upd  = RecUpdE (VarE o) [(field, VarE s)]     -- o { field = s }
      -- assign (to a field value)
      asn' = FunD asn [Clause [VarP s]               -- asn s = o { field = s }
                              (NormalB upd)
                              []]
      -- apply ff (to give us the functor, e.g., f String above)
      apff = InfixE (Just (VarE ff))                  -- ff $ field o
                    (VarE '($))
                    (Just (AppE (VarE field) (VarE o)))
      -- fmap assignment over the functor
      fmp  = AppE (AppE (VarE 'fmap) (VarE asn)) apff -- fmap asn (ff $ field o)
  return [FunD name [Clause [VarP ff, VarP o] (NormalB fmp) [asn']]]

-- | mkRLens, creating a lens 's' for a field of the form '_s'

mkRLens_ :: String -> Q [Dec]
mkRLens_ name = mkRLens (mkName name) (mkName ('_' : name))


mkRLensT :: Name -> Name -> String -> String -> Q [Dec]
mkRLensT name field rtype ftype = do
  fn <- mkRLens name field
  let sig = AppT (AppT (ConT ''Lens') (ConT $ mkName ftype)) (ConT $ mkName rtype)
  return $ SigD name sig : fn
  
mkRLensT_ :: String -> String -> String -> Q [Dec]
mkRLensT_ name rtype ftype = mkRLensT (mkName name) (mkName ('_' : name)) rtype ftype