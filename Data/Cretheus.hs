{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------
-- |
-- Module : Data.Cretheus
-- Copyright : (C) 2014 Zalora SEA
-- License : BSD3
-- Maintainer : Julian K. Arni <jkarni@gmail.com>
-- Stability : experimental
--
--
-- Drop in replacement for 'aeson', with a little extra functionality. In
-- particular, 'cretheus' allows you to get JSON schemas from your FromJSON
-- instances [0].
--
-- Depends on, rather than forks, the 'aeson' library, and either
-- re-exports aeson's functions directly, or proxies them under a thin
-- layer of transformations. You can therefore pick a version of aeson by
-- just adding it to your cabal file, and not have to wait for bug-fixes
-- upstream to land here.
--
-- *How it works:*
--
-- @cretheus@ has its own, reified version of aesons's @Parser@ type. Thus,
-- when you defined a @FromJSON@ instance using the appropriate
-- combinators, @cretheus@ can apply @parseJSON@ to dummy values with each
-- of the possible outermost constructors, collect the results, and
-- generate schemas from those.
--
-- [0] Modulo some restrictions. See 'mkSchema'. These restrictions
-- influence whether a JSON schema can be created, but not whether the
-- aeson-functionality still works.
--------------------------------------------------------------------------
module Data.Cretheus where

import Control.Monad
import Control.Monad.Operational
import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BS
import Data.Data
import Data.Foldable
import Data.Function (on)
import Data.Monoid
import Data.Text hiding (singleton)
import Data.Typeable
import Data.Scientific

import Debug.Trace
-- Schema

data Schema' where
    Required :: Text -> TypeRep -> Schema'
    Optional :: Text -> TypeRep -> Schema'
    Default :: Text -> String -> TypeRep -> Schema'
    And :: Schema' -> Schema' -> Schema'
    Or  :: Schema' -> Schema' -> Schema'
    Nil :: Schema'     -- Leaf that doesn't parse
    Empty :: Schema'   -- Leaf that parses with no further requirements
    deriving Show

instance A.ToJSON Schema' where
    toJSON x = A.toJSON $ addToSchema (SchemaH [] []) x

data SchemaH = SchemaH [(Text, A.Value)] [Text]

instance A.ToJSON SchemaH where
    toJSON (SchemaH xs reqs) =
        A.object [ "type" A..= ("object"::String)
                 , "properties" A..= A.object xs
                 , "required" A..= reqs
                 ]


addToSchema :: SchemaH -> Schema' -> SchemaH
addToSchema (SchemaH xs reqs) (Required field trep) =
    SchemaH ((field, A.object ["type" A..= show trep]):xs) (field:reqs)
addToSchema (SchemaH xs reqs) (Optional field trep) =
    SchemaH ((field, A.object ["type" A..= show trep]):xs) reqs
addToSchema (SchemaH xs reqs) (Default field def trep) =
    SchemaH ((field, A.object ["type" A..= show trep, "default" A..= def]):xs) reqs
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
                            z -> trace ("Here:\t" ++ show z ++ "\n") z
mkSchema Mzero       = Nil
{-[>mkSchema (Mplus a b) = mkSchema a `Or` mkSchema b<]-}
mkSchema (WithText _ f _) = mkSchemaM (f undefined)

mkSchemaM :: Typeable a => ParserM a -> Schema'
mkSchemaM = eval . view
    where
      -- We need to pattern match on the constructors to bring the
      -- typeable constraint into scope
      eval :: Typeable a => ProgramView Parser a -> Schema'
      eval (x@(v :.: t) :>>= f) = mkSchema x `And` mkSchemaM (f undefined)
      eval (x@(v :.:? t) :>>= f) = mkSchema x `And` mkSchemaM (f undefined)
      eval (x@(v :.!= t) :>>= f) = mkSchema x `And` mkSchemaM (f undefined)
      eval (x@Mzero :>>= f) = mkSchema x `And` mkSchemaM (f undefined)
      eval (x@(WithText{}) :>>= f) = mkSchema x `And` mkSchemaM (f undefined)
      eval (Return x) = Empty
-- Compat

(.:) :: (Typeable a, FromJSON a) => A.Object -> Text -> ParserM a
x .: t  = singleton $ x :.: t

(.:?) :: (Typeable b, FromJSON b) => A.Object -> Text -> ParserM (Maybe b)
x .:? t = singleton $ x :.:? t

(.!=) :: (Typeable a, Show a) => ParserM (Maybe a) -> a -> ParserM a
p .!= t = singleton $ p :.!= t

withText :: (Typeable a) => String -> (Text -> ParserM a) -> Value -> ParserM a
withText s f v = singleton $ WithText s f v

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

class FromJSON a where
    parseJSON :: Value -> ParserM a

instance FromJSON a => A.FromJSON a where
    parseJSON = interpretM . parseJSON . interpretV'


-- It'll be important to keep track of which fields may be evaluated, and
-- which may not. In essence, we want all arguments that may be coming
-- from inside the outer constructor of the `Value` passed to `parseJSON`
-- to not be used - e.g., the first argument of `(:.:)`.
data Parser a where
    (:.:)  :: (Typeable a, FromJSON a) => A.Object -> Text -> Parser a
    (:.:?) :: (Typeable b, FromJSON b) => A.Object -> Text -> Parser (Maybe b)
    (:.!=) :: (Typeable a, Show a) => ParserM (Maybe a) -> a -> Parser a
    Mzero  :: Typeable a => Parser a
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


data Tree f a = Node a
              | Tree (f (Tree f a))
              deriving (Foldable, Functor)

-- | Pass values with all possible outermost constructors to 'parseJSON',
-- and collect the results.
--
fillOut :: FromJSON a => a -> Tree [] (ParserM a)
fillOut _ = Tree
               [ Node $ parseJSON (Array u)
               , Node $ parseJSON (Object u)
               , Node $ parseJSON (String u)
               , Node $ parseJSON (Number u)
               , Node $ parseJSON (Bool u)
               , Node $ parseJSON Null
               ]
    where u = error "I wasn't meant to be evaluated!"

----Test------------------------------------------------------------------

instance FromJSON String where
    parseJSON = withText "String" (pure . unpack)

instance FromJSON a => FromJSON (Maybe a) where
    parseJSON Null = pure Nothing
    parseJSON a    = Just <$> parseJSON a

data Test1 = Test1 { field1 :: String
                   , field2 :: String
                   } deriving (Show, Typeable)

-- 'A.decode' still works...
t1 :: Maybe Test1
t1 = A.decode "{\"field1\": \"Field 1\", \"field2\": \"Field 2\"}"

t1' :: BS.ByteString
t1' = (A.encode . mkSchemaM <$> (toList $ fillOut undefined::[ParserM Test1])) !! 1


instance FromJSON Test1 where
    parseJSON (Object v) = do
        f1 <- (++ "a suffix") <$> v .: "field1"
        f2 <- (v .:? "field2") .!= "default-string"
        let f1' = "a prefix" ++ f1
        return (Test1 f1' f2)
    parseJSON _            = singleton Mzero
