{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cretheus.Schema.Schema where

import Control.Arrow (first)
import Data.Cretheus.Types.Internal
import Control.Applicative
import Data.Typeable
import Data.Foldable
import Data.Text hiding (singleton, filter)

import Debug.Trace

data Tree f a = Node a
              | Tree (f (Tree f a))
              deriving (Foldable, Functor)

-- | Pass values with all possible outermost constructors to 'parseJSON',
-- and collect the results.
--
fillOut :: FromJSON a => a -> Tree [] (ParserM a, Text)
fillOut _ = Tree
               [ Node (parseJSON (Array u), "array")
               , Node (parseJSON (Object u), "object")
               , Node (parseJSON (String u), "string")
               , Node (parseJSON (Number u), "number")
               , Node (parseJSON (Bool u), "bool")
               , Node (parseJSON Null, "null")
               ]
    where u = error "I wasn't meant to be evaluated!"

-- | For all
toSchemas :: (Typeable a, FromJSON a) => a -> SchemaH
toSchemas proxy = case schemas of
    [x]  -> x
    _:_  -> error "Not implemented" -- should use JSON Schema's 'oneOf'
    _    -> error "No non-mzero parsing"
  where
    all = toList $ fillOut proxy
    sacc = filter (\(p,_) -> p /= Nil ) $  first mkSchemaM <$> all
    schemas = uncurry (flip wrapSchema) <$> sacc
