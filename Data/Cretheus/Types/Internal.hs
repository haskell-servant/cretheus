{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
    (:.:)  :: (Typeable a, FromJSON a) => A.Object -> Text -> Parser a
    (:.:?) :: (Typeable b, FromJSON b) => A.Object -> Text -> Parser (Maybe b)
    (:.!=) :: (Typeable a, Show a) => ParserM (Maybe a) -> a -> Parser a
    Mzero  :: Parser a
    Mplus  :: ParserM a -> ParserM a -> Parser a
    WithText :: Typeable a => String -> (Text -> ParserM a) -> Value -> Parser a

type ParserM a = Program Parser a

interpretP :: Parser a -> A.Parser a
interpretP (v :.: t) = v A..: t
interpretP (v :.:? t) = v A..:? t
interpretP (v :.!= t) = interpretM v A..!= t
interpretP Mzero = mzero
interpretP (WithText s f v) = A.withText s (interpretM . f) (interpretV v)

-- | Convert a Parser to a Parser.
-- Currently this is used to interface with all 'aeson' functions, but
-- that suffers from a (probably avoidable) performance penalty.
interpretM :: ParserM a -> A.Parser a
interpretM = interpretWithMonad interpretP

instance MonadPlus (Program Parser) where
    mzero = singleton Mzero
--------------------------------------------------------------------------
-- Schemas
--------------------------------------------------------------------------

data Schema' where
    Required :: Text -> TypeRep -> Schema'
    Optional :: Text -> TypeRep -> Schema'
    Default :: Text -> String -> TypeRep -> Schema'
    And :: Schema' -> Schema' -> Schema'
    Or  :: Schema' -> Schema' -> Schema'
    Nil :: Schema'     -- Leaf that doesn't parse
    Empty :: Schema'   -- Leaf that parses with no further requirements
    deriving (Show, Eq)

data SchemaH = SchemaH { typ :: Text
                       , properties :: [(Text, A.Value)]
                       , required :: [Text]
                       } deriving (Show, Eq)

wrapSchema :: Text -> Schema' -> SchemaH
wrapSchema typ = addToSchema (SchemaH typ [] [])

addToSchema :: SchemaH -> Schema' -> SchemaH
addToSchema (SchemaH n xs reqs) (Required field trep) =
    SchemaH n ((field, A.object ["type" A..= show trep]):xs) (field:reqs)
addToSchema (SchemaH n xs reqs) (Optional field trep) =
    SchemaH n ((field, A.object ["type" A..= show trep]):xs) reqs
addToSchema (SchemaH n xs reqs) (Default field def trep) =
    SchemaH n ((field, A.object ["type" A..= show trep, "default" A..= def]):xs) reqs
addToSchema sch (And s1 s2) = addToSchema (addToSchema sch s2) s1
addToSchema sch Empty = sch
addToSchema sch Nil = sch

-- | Make a schema from a reified parser. Assumes that only the
-- @cretheus@-provided combinators (and instance methods) are used on the
-- inner-value of 'parseJSON''s first argument (the 'Value').
mkSchema :: forall a. Typeable a => Parser a -> Schema'
mkSchema (_ :.:  txt) = Required txt $ typeOf (undefined::a)
mkSchema (_ :.:? txt) = Optional txt $ typeOf (undefined::a)
mkSchema (a :.!= def) = case mkSchemaM a of
    Optional txt typ -> Default txt (show def) typ
    Required txt typ -> Default txt (show def) typ
    Optional txt typ `And` x -> Default txt (show def) typ `And` x
    z -> z
mkSchema Mzero       = Nil
{-[>mkSchema (Mplus a b) = mkSchema a `Or` mkSchema b<]-}
mkSchema (WithText _ f _) = mkSchemaM (f undefined)

mkSchemaM :: Typeable a => ParserM a -> Schema'
mkSchemaM = eval . view
    where
      -- We need to pattern match on the constructors to bring the
      -- typeable constraint into scope
      eval :: Typeable a => ProgramView Parser a -> Schema'
      eval (x@(v :.: t) :>>= f)    = mkSchema x `And` mkSchemaM (f undefined)
      eval (x@(v :.:? t) :>>= f)   = mkSchema x `And` mkSchemaM (f undefined)
      eval (x@(v :.!= t) :>>= f)   = mkSchema x `And` mkSchemaM (f undefined)
      eval (Mzero :>>= f)          = Nil
      eval (x@(WithText{}) :>>= f) = mkSchema x `And` mkSchemaM (f undefined)
      eval (Return x)              = Empty

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
