{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell  #-}

{-|

Description : simple options parser
Copyright   : (c) Martyn J. Pearce 2015
License     : BSD
Maintainer  : haskell@sixears.com

simple command-line options parser, with automatic usage and help texts

 -}


module Fluffy.Console.Getopt
  ( ArgArity(..), OptAct(..), OptDesc(..)
  , dieUsage, dieUsage', getOptions, optBool, optPosInt, parseOptVal )
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

import Fluffy.Sys.Exit  ( die, eUsage, eUtility )

--------------------------------------------------------------------------------

-- | standard error for user usage failure (MonadThrow version)

dieUsage :: MonadThrow m => String -> m a
dieUsage = die eUsage

-- | standard error for user usage failure (MonadIO version)

dieUsage' :: MonadIO m => String -> m a
dieUsage' = liftIO . dieUsage

-- | standard 'error' for user requested a utility (e.g., --help)
--   (MonadThrow version)

dieUtility :: MonadThrow m => String -> m a
dieUtility = die eUtility

-- | standard 'error' for user requested a utility (e.g., --help)
--   (MonadIO version)

dieUtility' :: MonadIO m => String -> m a
dieUtility' = liftIO . dieUtility


-- ArgArity --------------------------------------------------------------------

-- | how many arguments a parser expects; the actual number passed is checked
--   against this, and potentially an error is thrown

data ArgArity = ARGS_NONE | ARGS_ONE

-- | descriptive line for general usage.  Arg 2 ([String]) is for the list of
--   argument types

usageLineArgs :: ArgArity -> [String] -> String
usageLineArgs ARGS_NONE []  = ""
usageLineArgs ARGS_NONE _   = error "too many arg types specified (should be none)"
usageLineArgs ARGS_ONE  [x] = x
usageLineArgs ARGS_ONE  _   = error $ "wrong number of many arg types specified "
                                  ++ "(should be one)"

-- checkArgs ---------------------------

-- | throw a dieUsage error with descriptive text for the user; wrong number of
--   arguments passed

wrongArgumentCount :: MonadThrow m => String -> [String] -> m a
wrongArgumentCount s as =
  dieUsage $ unlines (concat [ "wrong number of arguments: expected "
                             , s, ", got ", show (length as)
                             , if null as then "" else ":"
                             ]
                      : as)

-- | how to check the list of arguments against the expected arity

checkArgs :: MonadThrow m => ArgArity -> [String] -> m ()
checkArgs ARGS_NONE as =
  case as of
    [] -> return ()
    _  -> wrongArgumentCount "0" (fmap (\a -> "  '" ++ a ++ "'") as)
checkArgs ARGS_ONE as =
  case as of
    [_] -> return ()
    _   -> wrongArgumentCount "1" (fmap (\a -> "  '" ++ a ++ "'") as)

-- OptAct ----------------------------------------------------------------------

-- | how to enact an option (possibly including parsing a value)
data OptAct o = NO_ARG  (forall m . MonadIO m => o -> m o)
              | ONE_ARG (forall m . MonadIO m => String -> o -> m o)

-- OptDesc ---------------------------------------------------------------------

-- | option definition

data OptDesc o = OptDesc { _names   :: [String]  -- ^ invocation names,
                                                 --   without leading hyphen(s)
                         , _typ     :: String    -- ^ name of option type (for
                                                 --   help output)
                         , _summary :: String    -- ^ summary help text
                         , _action  :: OptAct o  -- ^ what to do upon invocation
                         }

$( makeLenses ''OptDesc )

-- | single string representation of the option names, including any leading
--   hyphens; option names are separated by pipe characters
optnames :: OptDesc o -> String
optnames o = intercalate "|" . fmap n $ view names o
  where n s = case s of
                c : [] -> [ '-', c ]
                cs     -> "--" ++ cs

--------------------------------------------------------------------------------

-- getOptions --------------------------

-- | read arguments from the cmdline; parse them according to option
--   descriptions; filter out arguments; check arity of arguments; append
--   standard --help.  Will throw on error.

getOptions :: (MonadIO m)
           => ArgArity         -- ^ valid arity of argument count
           -> [String]         -- ^ argument type names (for usage text)
           -> [OptDesc o]      -- ^ descriptions of available options
           -> o                -- ^ options starting value
           -> m (o, [String])  -- ^ updated options value, and accumulated
                               --   argument strings

getOptions arity arg_types optdescs o = liftIO $ do
  p <- getProgName
  let help_text = unlines (usageText p arity arg_types optdescs)
      help_arg  = NO_ARG $ \_ -> dieUtility' help_text

      help_opt  = OptDesc ["help"] "this help" "" help_arg
  as <- getArgs
  (opts, args) <- getOptions' (help_opt : optdescs)  (o, []) as
  checkArgs arity args
  return (opts, args)

-- getOptions' -------------------------

-- | parse a set of strings to update options and find arguments

getOptions' :: (MonadIO m)
            => [OptDesc o]    -- ^ descriptions of available options
            -> (o, [String])  -- ^ option accumulation, and cmdline args
            -> [String]       -- ^ input cmdline strings
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

-- | handle an individual option

handleOpt :: (MonadIO m)
          => String          -- ^ name of the invoking option, including any
                             --   leading dashes
          -> [OptDesc o]     -- ^ avaiable option descriptors
          -> (o, [String])   -- ^ input accumulation of options and cmdline args
          -> [String]        -- ^ as-yet-unparsed cmdline strings (including
                             --   possible value(s) for this option)
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

-- usageTexts --------------------------

-- | single summary usage line for a given option description
summarize :: Int        -- ^ width to use for name
          -> Int        -- ^ width to use for type
          -> OptDesc o  -- ^ option descriptor
          -> String
summarize nm_w tp_w o =
  printf ("%*s  %*s  %s") (-1*nm_w) (optnames o)
                          (-1*tp_w) (view typ o)
                          (view summary o)

-- | summary usage lines for options
usageTexts :: [OptDesc o] -> [String]
usageTexts os = let name_width = maximum $ fmap (length . optnames) os
                    type_width = maximum $ fmap (length . view typ) os
                 in fmap (("  " ++) . summarize name_width type_width) os

-- | summary usage text to use if --help is called
usageText :: String -> ArgArity -> [String] -> [OptDesc o] -> [String]
usageText progname arity arg_types optdescs =
  let args                   = usageLineArgs arity arg_types
      (help_texts, option_t) = if null optdescs
                               then ([], "")
                               else ("" : usageTexts optdescs, " <option>*")
      args_t                 = if null args then "" else ' ' : args
      usage_line             = "usage: " ++ progname ++ option_t ++ args_t
   in usage_line : help_texts

-- parseOptVal ------------------------

-- | parse an option val from the cmdline; apply it to the existing accumulation
--   with a given fn
parseOptVal :: (MonadIO m, Read i)
             => String           -- ^ typename to use for error msgs
             -> (i -> o -> m o)  -- ^ fn to apply value to accumulation
             -> String           -- ^ incoming value from cmdline
             -> o                -- ^ input accumulation
             -> m o
parseOptVal typnam f s o =
  case readMaybe s of
    Nothing -> dieUsage' ("failed to parse '" ++ s ++ "' as an " ++ typnam)
    Just s' -> f s' o

checkPositiveInt :: MonadThrow m => Int -> m Int
checkPositiveInt i = if i <= 0
                     then dieUsage $ "integer must be > 0 (got " ++ show i ++ ")"
                     else return i

-- | a cmdline option whose value must be an integer greater than zero
optPosInt :: (o -> Int -> o) -> OptAct o
optPosInt setter =
  ONE_ARG $
  parseOptVal "Int"
              ( \ i o -> liftIO (checkPositiveInt i) >> return (setter o i))

-- | a cmdline option with no argument; if called, sets its target lens to true
optBool :: Lens' o Bool -> OptAct o
optBool l = NO_ARG  (return . set l True)
