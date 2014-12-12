{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cretheus.Schema.Schema where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Foldable
import Data.Text hiding (singleton, filter)
import qualified Data.Aeson as A

import Data.Cretheus.Types.Internal
import Data.Cretheus.Schema.Types

import Debug.Trace

data Tree f a = Node a
              | Tree (f (Tree f a))
              deriving (Foldable, Functor)

-- | Pass values with all possible outermost constructors to 'parseJSON',
-- and collect the results.
--
fillOut :: FromJSON a => a -> Tree [] (ParserM a)
fillOut _ = Tree
               [ Node (parseJSON (Array u))
               , Node (parseJSON (Object u))
               , Node (parseJSON (String u))
               , Node (parseJSON (Number u))
               , Node (parseJSON (Bool u))
               , Node (parseJSON Null)
               ]
    where u = error "I wasn't meant to be evaluated!"

-- | For all
toSchemas :: forall a. (A.ToJSON a, Typeable a, FromJSON a) => a -> JSONSchema
toSchemas proxy = case schemas of
    [x]  -> x
    []   -> error "No non-mzero parsing"
    xs   -> OneOf xs
  where
    all :: [[JSONProp]]
    all = filter (not . Prelude.null) $ mkProps <$> toList (fillOut proxy)
    schemas :: [JSONSchema]
    schemas = (\x -> JSONSchema { title = Nothing
                         , type' = typeRepTyCon $ typeOf () -- TODO
                         , description = Nothing
                         , properties = x
                         , validators = []
                         }) <$> all
