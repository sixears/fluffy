{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell  #-}

module Fluffy.Console.Getopt
  ( ArgArity(..), OptAct(..), OptDesc(..)
  , dieUsage, dieUsage', getOptions, optBool, optPosInt, parseCmdLine )
where

-- base --------------------------------
  
import Data.List           ( intercalate )
import System.Environment  ( getArgs, getProgName )
import Text.Printf         ( printf )
import Text.Read           ( readMaybe )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadThrow )

-- lens --------------------------------

import Control.Lens  ( Lens', (^.), makeLenses, set, view )

-- transformers ------------------------

import Control.Monad.IO.Class  ( MonadIO, liftIO )

-- Fluffy ------------------------------

import Fluffy.Sys.Exit  ( die, dieInternal, eUsage, eUtility )

--------------------------------------------------------------------------------

dieUsage :: MonadThrow m => String -> m a
dieUsage = die eUsage

dieUsage' :: MonadIO m => String -> m a
dieUsage' = liftIO . dieUsage

dieUtility :: MonadThrow m => String -> m a
dieUtility = die eUtility

dieUtility' :: MonadIO m => String -> m a
dieUtility' = liftIO . dieUtility


-- ArgArity --------------------------------------------------------------------

data ArgArity = ARGS_NONE | ARGS_ONE

usageLine :: ArgArity -> [String] -> String
usageLine ARGS_NONE []  = ""
usageLine ARGS_NONE _   = error "too many arg types specified (should be none)"
usageLine ARGS_ONE  [x] = x
usageLine ARGS_ONE  _   = error $ "wrong number of many arg types specified "
                                  ++ "(should be one)"

-- checkArgs ---------------------------

wrongArgumentCount :: MonadThrow m => String -> [String] -> m a
wrongArgumentCount s as =
  dieUsage $ unlines (concat [ "wrong number of arguments: expected "
                             , s, ", got ", show (length as)
                             , if null as then "" else ":"
                             ]
                      : as)

checkArgs :: MonadThrow m => ArgArity -> [String] -> m ()
checkArgs ARGS_NONE as =
  case as of
    [] -> return ()
    _  -> wrongArgumentCount "0" (fmap (\a -> "  '" ++ a ++ "'") as)
checkArgs ARGS_ONE as =
  case as of
    [_] -> return ()
    _  -> wrongArgumentCount "1" (fmap (\a -> "  '" ++ a ++ "'") as)

-- OptAct ----------------------------------------------------------------------

data OptAct o = NO_ARG  (forall m . MonadIO m => o -> m o)
              | ONE_ARG (forall m . MonadIO m => String -> o -> m o)

-- OptDesc ---------------------------------------------------------------------

data OptDesc o = OptDesc { _names   :: [String]
                         , _typ     :: String
                         , _summary :: String
                         , _action  :: OptAct o
                         }

$( makeLenses ''OptDesc )

optnames :: OptDesc o -> String
optnames o = intercalate "|" . fmap n $ view names o
  where n s = case s of
                c : [] -> [ '-', c ]
                cs     -> "--" ++ cs


summarize :: Int -> Int -> OptDesc o -> String
summarize nm_w tp_w o =
  printf ("%-" ++ show nm_w ++ "s  %-" ++ show tp_w ++ "s  %s")
         (optnames o) (view typ o) (view summary o)

--------------------------------------------------------------------------------

-- getOptions --------------------------

getOptions :: (MonadIO m)
           => ArgArity -> [String] -> [OptDesc o] -> o
           -> m (o, [String])

getOptions arity arg_types optdescs o = liftIO $ do
  p <- getProgName
  let help_text = unlines (helpText p arity arg_types optdescs)
      help_arg  = NO_ARG $ \_ -> dieUtility' help_text
                                 
      help_opt  = OptDesc ["help"] "this help" "" help_arg
  as <- getArgs
  (opts, args) <- getOptions' (help_opt : optdescs)  (o, []) as
  checkArgs arity args
  return (opts, args)

-- getOptions' -------------------------

getOptions' :: (MonadIO m)
            => [OptDesc o] -> (o, [String]) -> [String]
            -> m (o, [String])
getOptions' optdescs (o, acc) ss =
  case ss of
    []                        -> return (o, reverse acc)

    (nm@('-' : '-' : opt) : ss') ->
      case opt of
        []     -> return (o, reverse acc ++ ss') -- "--"; terminate options
        _ : [] -> dieUsage' $ "not a legal option invocation: '" ++ nm ++ "'"
        _      -> handleOpt nm optdescs (o, acc) ss'

    (nm@('-' : opt) : ss') ->
      case opt of
        []     -> getOptions' optdescs (o, "-" : acc) ss' -- "-"; just an arg
        _ : [] -> handleOpt nm optdescs (o, acc) ss'
        _      -> dieUsage' $ "clustered options not supported: '" ++ opt ++ "'"

    (s : ss')       -> getOptions' optdescs (o, s : acc) ss'

-- handleOpt ---------------------------

handleOpt :: (MonadIO m)
          => String -> [OptDesc o] -> (o, [String]) -> [String]
          -> m (o, [String])
handleOpt opt optdescs (o, acc) ss =
  case filter ((dropWhile (== '-') opt `elem`) . view names) optdescs of
    []     -> dieUsage' $ "not a valid option: '" ++ opt ++ "'"
    od : _ -> case od ^. action of
                NO_ARG  f -> f o >>= \ o' -> getOptions' optdescs (o', acc) ss
                ONE_ARG g ->
                  case ss of
                    []      -> dieUsage' $
                                 "option: '" ++ opt ++ "' is missing a value"
                    s : ss' -> g s o >>= \ o' -> getOptions' optdescs (o', acc) ss'

-- helpTexts ---------------------------

helpTexts :: [OptDesc o] -> [String]
helpTexts os = let name_width = maximum $ fmap (length . optnames) os
                   type_width = maximum $ fmap (length . view typ) os
                in fmap (("  " ++) . summarize name_width type_width) os

helpText :: String -> ArgArity -> [String] -> [OptDesc o] -> [String]
helpText progname arity arg_types optdescs =
  let args                   = usageLine arity arg_types
      (help_texts, option_t) = if null optdescs
                               then ([], "")
                               else ("" : helpTexts optdescs, " <option>*")
      args_t                 = if null args then "" else ' ' : args
      usage_line             = "usage: " ++ progname ++ option_t ++ args_t
   in usage_line : help_texts

-- parseCmdLine ------------------------

parseCmdLine :: (MonadIO m, Read i) => (i -> o -> m o) -> String -> o -> m o
parseCmdLine f s o =
  case readMaybe s of
    Nothing -> dieUsage' ("failed to parse '" ++ s ++ "' as an Int")
    Just i  -> f i o

checkPositiveInt :: MonadThrow m => Int -> m Int
checkPositiveInt i = if i <= 0
                     then dieUsage $ "interval must be > 0 (got " ++ show i ++ ")"
                     else return i

optPosInt :: (o -> Int -> o) -> OptAct o
optPosInt setter = ONE_ARG $
  parseCmdLine ( \ i o -> liftIO (checkPositiveInt i) >> return (setter o i))

optBool :: Lens' o Bool -> OptAct o
optBool l = NO_ARG  (return . set l True)
