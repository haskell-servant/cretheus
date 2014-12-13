{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Data.Cretheus.Schema.Types where

import Data.Typeable
import Control.Applicative
import GHC.Generics
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Monad.Operational
import Data.Proxy

import Data.Cretheus.Types.Internal

data JSONSchema = JSONSchema { title :: Maybe T.Text
                             , type' :: TyCon
                             , description :: Maybe T.Text
                             , properties :: [JSONProp]
                             , validators :: [JSONValidator]
                             }
                | OneOf [JSONSchema]
                | NullSchema
                deriving (Typeable)

instance A.ToJSON TyCon where
    toJSON = A.String . T.pack . show

instance A.ToJSON JSONSchema where
    toJSON JSONSchema{..} =
        A.object [ "type" A..= type'
                 , "properties" A..= A.object (propsToPairs properties)
                 , "required" A..= reqs
                 ]
      where
        freqs (JSONProp (JSONProp'{..})) = required
        freqs _ = False
        reqs = key' <$> filter freqs properties
        key' (JSONProp x) = key x
    toJSON (OneOf xs) = A.object [ "oneOf" A..= A.toJSON xs ]
    toJSON NullSchema = A.Null

-- | Make a 'JSONSchema' from a 'ParserM'. Note that the title and
-- description of the schema are set to @Nothing@.
mkJSONSchema :: forall a. (Typeable a, A.ToJSON a) => ParserM a -> JSONSchema
mkJSONSchema p = JSONSchema { title = Nothing
                            , type' = typeRepTyCon $ typeOf () -- TODO
                            , description = Nothing
                            , properties = mkProps p
                            , validators = []
                            }

propsToPairs :: [JSONProp] -> [A.Pair]
propsToPairs [] = []
propsToPairs (JSONProp (JSONProp'{..}):xs) = (key A..= val):propsToPairs xs
propsToPairs (JSONProp Nil:xs) = []
propsToPairs (JSONProp Empty:xs) = propsToPairs xs

data JSONProp = forall a. A.ToJSON a => JSONProp (JSONProp' a)

pack :: A.ToJSON a => JSONProp' a -> JSONProp
pack = JSONProp

data JSONProp' a =
    JSONProp' { key :: T.Text
              , val :: JSONSchema
              , required :: Bool
              , defaultVal :: Maybe a
              }
    | Nil
    | Empty
    deriving (Generic)

instance A.ToJSON a => A.ToJSON (JSONProp' a) where
    toJSON (JSONProp' {..}) = A.object [ "key" A..= key
                                      , "val" A..= val
                                      ]
    toJSON _ = A.Null

mkProp :: forall a. A.ToJSON a => Parser a -> [JSONProp]
mkProp (_ :.: txt) = [pack $ ((JSONProp' { key        = txt
                               , val        = toSchemas (undefined::a)
                               , required   = True
                               , defaultVal = Nothing
                               })::JSONProp' ()) ]
    where undefined = error "here"
{-mkProp (_ :.:? txt) = [pack $ JSONProp' { key        = txt-}
                               {-, val        = mkJSONSchema (undefined::ParserM a)-}
                               {-, required   = False-}
                               {-, defaultVal = Nothing-}
                               {-}]-}
{-mkProp (a :.!= def) = [pack $ (mkProp a){ required = False, defaultVal = Just def }]-}
mkProp Mzero                  = []
mkProp (WithObject _ f _)     = mkProps (f undefined)
mkProp (WithText _ f _)       = mkProps (f undefined)
mkProp (WithArray _ f _)      = mkProps (f undefined)
mkProp (WithScientific _ f _) = mkProps (f undefined)
mkProp (WithBool _ f _)       = mkProps (f undefined)

mkProps :: ParserM a -> [JSONProp]
mkProps = eval . view
    where
      eval' :: A.ToJSON a => Parser a -> (a -> ParserM b) -> [JSONProp]
      eval' x f = mkProp x ++ mkProps (f undefined)
      -- We need to pattern match on the constructors to bring the
      -- typeable constraint into scope
      eval ::  ProgramView Parser a -> [JSONProp]
      eval (x@(v :.: t) :>>= f)          = eval' x f
      eval (x@(v :.:? t) :>>= f)         = eval' x f
      eval (x@(v :.!= t) :>>= f)         = eval' x f
      eval (Mzero :>>= f)                = []
      eval (x@(WithText{}) :>>= f)       = eval' x f
      eval (x@(WithObject{}) :>>= f)     = eval' x f
      eval (x@(WithBool{}) :>>= f)       = eval' x f
      eval (x@(WithScientific{}) :>>= f) = eval' x f
      eval (x@(WithArray{}) :>>= f)      = eval' x f
      eval (Return x)                    = []



data JSONValidator where
    Minimum :: (A.ToJSON a, Ord a) => a -> JSONValidator
    Maximum :: (A.ToJSON a, Ord a) => a -> JSONValidator

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
