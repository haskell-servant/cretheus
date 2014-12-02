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
module Data.Cretheus where

import Control.Monad
import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Text
import Data.Typeable
import Data.Scientific
import Data.Proxy

-- Schema

data Typ

data a :*: b = a :*: b
data a :+: b = a :+: b


data Schema' where
    Required :: Text -> Typ -> Schema'
    Optional :: Text -> Typ -> Schema'
    Default :: Text -> String -> Typ -> Schema'
    And :: Schema' -> Schema' -> Schema'
    Or  :: Schema' -> Schema' -> Schema'

mkSchema :: Show a => ReifiedP a -> Schema'
mkSchema (a :.:  txt) = Required txt $ typeOf' (Proxy::Proxy a)
mkSchema (a :.:? txt) = Optional txt $ typeOf' (Proxy::Proxy a)
mkSchema (a :.!= def) = case mkSchema a of
                            Optional txt typ -> Default txt (show def) typ
{-mkSchema (a :>>= f) = m-}
{-mkSchema (Ret a)     = return a-}
{-mkSchema Mzero       = -}
mkSchema (Mplus a b) = mkSchema a `And` mkSchema b


typeOf' :: Proxy a -> Typ
typeOf' = undefined
-- Compat

(.:) ::  FromJSON a => A.Object -> Text -> ReifiedP a
(.:)  = (:.:)

(.:?) :: FromJSON b => A.Object -> Text -> ReifiedP (Maybe b)
(.:?) = (:.:?)

(.!=) :: Show a => ReifiedP (Maybe a) -> a -> ReifiedP a
(.!=) = (:.!=)

-- Non-strict version of 'A.Value'
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
    parseJSON :: Value -> ReifiedP a

instance FromJSON a => A.FromJSON a where
    parseJSON = interpretR . parseJSON . interpretV'


-- It'll be important to keep track of which fields may be evaluated, and
-- which may not. In essence, we want all arguments that may be coming
-- from inside the outer constructor of the `Value` passed to `parseJSON`
-- to not be used - e.g., the first argument of `(:.:)`.
data ReifiedP a where
    (:.:)  :: FromJSON a => A.Object -> Text -> ReifiedP a
    (:.:?) :: FromJSON b => A.Object -> Text -> ReifiedP (Maybe b)
    (:.!=) :: Show a => ReifiedP (Maybe a) -> a -> ReifiedP a
    (:>>=) :: ReifiedP a -> (a -> ReifiedP b) -> ReifiedP b
    Ret    :: a -> ReifiedP a
    Mzero  :: ReifiedP a
    Mplus  :: ReifiedP a -> ReifiedP a -> ReifiedP a
    WithText :: String -> (Text -> ReifiedP a) -> Value -> ReifiedP a

{-
- (:<*>) === RP (a -> b) -> RP a -> RP b
-   \---> +/-  AND
- <*> = m1 >>= \x1 -> m2 >>= \x2 -> return $ x1 x2
-}


instance Monad ReifiedP where
    (>>=) = (:>>=)
    return = Ret

instance MonadPlus ReifiedP where
    mzero = Mzero
    mplus = Mplus

instance Functor ReifiedP where
    fmap = liftM

instance Applicative ReifiedP where
    pure = return
    (<*>) = ap

instance Alternative ReifiedP where
    empty = mzero
    (<|>) = mplus

instance Monoid (ReifiedP a) where
    mempty = mzero
    mappend = mplus

-- | Convert a ReifiedP to a Parser.
-- Currently this is used to interface with all 'aeson' functions, but
-- that suffers from a (probably avoidable) performance penalty.
interpretR :: ReifiedP a -> A.Parser a
interpretR (a :.:  b) = a A..: b
interpretR (a :.:? b) = a A..:? b
interpretR (a :.!= b) = interpretR a A..!= b
interpretR (a :>>= f) = interpretR a >>= interpretR . f
interpretR (Ret a)     = return a
interpretR Mzero       = mzero
interpretR (Mplus a b) = mplus (interpretR a) (interpretR b)
interpretR (WithText a f v) = A.withText a (interpretR . f) (interpretV v)


----Test------------------------------------------------------------------

instance FromJSON String where
    parseJSON = WithText "String" $ pure . unpack

data Test1 = Test1 { field1 :: String
                   , field2 :: String
                   } deriving Show

t1 :: Maybe Test1
t1 = A.decode "{\"field1\": \"Field 1\", \"field2\": \"Field 2\"}"

t1' :: String
t1' = show $ show' <$> (toList $ fillOut undefined::[ReifiedP Test1])

data Tree f a = Node a
              | Tree (f (Tree f a))
              deriving (Foldable, Functor)

-- | Pass values with all possible outermost constructors to 'parseJSON',
-- and collect the results.
--
fillOut :: FromJSON a => a -> Tree [] (ReifiedP a)
fillOut _ = Tree
               [ Node $ parseJSON (Array u)
               , Node $ parseJSON (Object u)
               , Node $ parseJSON (String u)
               , Node $ parseJSON (Number u)
               , Node $ parseJSON (Bool u)
               , Node $ parseJSON Null
               ]
    where u = error "I wasn't meant to be evaluated!"

show' :: ReifiedP a -> String
show' (a :.:  b) = "Required: " ++ unpack b
show' (a :.:? b) = "Optional: " ++ unpack b
show' (a :.!= b) = show' a ++ " with default" ++ show b
show' (a :>>= f) = show' a ++ show' (f undefined)
show' (Ret a)     = "\n"
show' Mzero       = "mzero"
show' (Mplus a b) = show' a ++ "or\n" ++ show' b
show' (WithText a f v) = "Expects: " ++ a  ++ show' (f undefined)

instance FromJSON Test1 where
    parseJSON (Object v) = Test1 <$> v .: "field1"
                                   <*> v .: "field2"
    parseJSON _            = mzero
