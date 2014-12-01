{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cretheus where

import Control.Monad
import Control.Applicative
import Data.Monoid
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Text
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
mkSchema (a :.:  txt) = Required txt $ typeOf (Proxy::Proxy a)
mkSchema (a :.:? txt) = Optional txt $ typeOf (Proxy::Proxy a)
mkSchema (a :.!= def) = case mkSchema a of
                            Optional txt typ -> Default txt (show def) typ
{-mkSchema (a :>>= f) = m-}
{-mkSchema (Ret a)     = return a-}
{-mkSchema Mzero       = -}
mkSchema (Mplus a b) = mkSchema a `And` mkSchema b


typeOf :: Proxy a -> Typ
typeOf = undefined
-- Compat

(.:) ::  FromJSON a => A.Object -> Text -> ReifiedP a
(.:)  = (:.:)

(.:?) :: FromJSON b => A.Object -> Text -> ReifiedP (Maybe b)
(.:?) = (:.:?)

(.!=) :: Show a => ReifiedP (Maybe a) -> a -> ReifiedP a
(.!=) = (:.!=)


class FromJSON a where
    parseJSON :: A.Value -> ReifiedP a

instance FromJSON a => A.FromJSON a where
    parseJSON = interpretR . parseJSON


-- It'll be important to keep track of which fields may be evaluated, and
-- which may not.
data ReifiedP a where
    (:.:)  :: FromJSON a => A.Object -> Text -> ReifiedP a
    (:.:?) :: FromJSON b => A.Object -> Text -> ReifiedP (Maybe b)
    (:.!=) :: Show a => ReifiedP (Maybe a) -> a -> ReifiedP a
    (:>>=) :: ReifiedP a -> (a -> ReifiedP b) -> ReifiedP b
    Ret    :: a -> ReifiedP a
    Mzero  :: ReifiedP a
    Mplus  :: ReifiedP a -> ReifiedP a -> ReifiedP a
    WithText :: String -> (Text -> ReifiedP a) -> A.Value -> ReifiedP a


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

interpretR :: ReifiedP a -> A.Parser a
interpretR (a :.:  b) = a A..: b
interpretR (a :.:? b) = a A..:? b
interpretR (a :.!= b) = interpretR a A..!= b
interpretR (a :>>= f) = interpretR a >>= interpretR . f
interpretR (Ret a)     = return a
interpretR Mzero       = mzero
interpretR (Mplus a b) = mplus (interpretR a) (interpretR b)
interpretR (WithText a f v) = A.withText a (interpretR . f) v


----Test------------------------------------------------------------------

instance FromJSON String where
    parseJSON = WithText "String" $ pure . unpack

data Test1 = Test1 { field1 :: String
                   , field2 :: String
                   } deriving Show

t1 :: Maybe Test1
t1 = A.decode "{\"field1\": \"Field 1\", \"field2\": \"Field 2\"}"

t1' :: String
t1' = show' (parseJSON obj::ReifiedP Test1)
    where obj = A.object [ ("field1", A.String "")
                       , ("field2", A.String "")
                       ]

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
    parseJSON (A.Object v) = Test1 <$> v .: "field1"
                                   <*> v .: "field2"
    parseJSON _            = mzero
