{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Cretheus.Types.Internal where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Monad
import Data.Data
import Data.Scientific
import Control.Monad.Operational
import Data.Typeable
import Data.Text hiding (singleton, filter)

class FromJSON a where
    parseJSON :: Value -> ParserM a

instance FromJSON a => A.FromJSON a where
    parseJSON = interpretM . parseJSON . interpretV'

--------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------

-- It'll be important to keep track of which fields may be evaluated, and
-- which may not. In essence, we want all arguments that may be coming
-- from inside the outer constructor of the `Value` passed to `parseJSON`
-- to not be used - e.g., the first argument of `(:.:)`.
data Parser a where
    (:.:)  :: (A.ToJSON a, Typeable a, FromJSON a) => A.Object -> Text -> Parser a
    (:.:?) :: (A.ToJSON b, Typeable b, FromJSON b) => A.Object -> Text -> Parser (Maybe b)
    (:.!=) :: (A.ToJSON a, Typeable a, Show a) => ParserM (Maybe a) -> a -> Parser a
    Mzero  :: Parser a
    Mplus  :: ParserM a -> ParserM a -> Parser a
    -- | @With@ constructors
    WithObject :: (A.ToJSON a, Typeable a) => String -> (A.Object -> ParserM a) -> Value -> Parser a
    WithText :: (A.ToJSON a, Typeable a) => String -> (Text -> ParserM a) -> Value -> Parser a
    WithArray :: (A.ToJSON a, Typeable a) => String -> (A.Array -> ParserM a) -> Value -> Parser a
    WithScientific :: (A.ToJSON a, Typeable a) => String -> (Scientific -> ParserM a) -> Value -> Parser a
    WithBool :: (A.ToJSON a, Typeable a) => String -> (Bool -> ParserM a) -> Value -> Parser a


type ParserM a = Program Parser a

interpretP :: Parser a -> A.Parser a
interpretP (v :.: t) = v A..: t
interpretP (v :.:? t) = v A..:? t
interpretP (v :.!= t) = interpretM v A..!= t
interpretP Mzero = mzero
interpretP (WithObject s f v) = A.withObject s (interpretM . f) (interpretV v)
interpretP (WithText s f v) = A.withText s (interpretM . f) (interpretV v)
interpretP (WithArray s f v) = A.withArray s (interpretM . f) (interpretV v)
interpretP (WithScientific s f v) = A.withScientific s (interpretM . f) (interpretV v)
interpretP (WithBool s f v) = A.withBool s (interpretM . f) (interpretV v)

-- | Convert a ParserM to a Parser.
-- Currently this is used to interface with all 'aeson' functions, but
-- that suffers from a (probably avoidable) performance penalty.
interpretM :: ParserM a -> A.Parser a
interpretM = interpretWithMonad interpretP

instance MonadPlus (Program Parser) where
    mzero = singleton Mzero
    mplus = error "mplus: not implemented"


--------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------

-- Non-strict version of 'A.Value'. We need this so we can call 'parseJSON'
-- with undefined.
data Value = Object A.Object
           | Array A.Array
           | String Text
           | Number Scientific
           | Bool Bool
           | Null
             deriving (Eq, Show, Typeable, Data)

interpretV :: Value -> A.Value
interpretV (Object !v) = A.Object v
interpretV (Array !v)  = A.Array v
interpretV (String !v) = A.String v
interpretV (Number !v) = A.Number v
interpretV (Bool !v)   = A.Bool v
interpretV Null        = A.Null

interpretV' :: A.Value -> Value
interpretV' (A.Object v) = Object v
interpretV' (A.Array v)  = Array v
interpretV' (A.String v) = String v
interpretV' (A.Number v) = Number v
interpretV' (A.Bool v)   = Bool v
interpretV' A.Null       = Null
