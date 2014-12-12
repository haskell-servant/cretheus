{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Cretheus.Types.Instances
    (
    -- * Type classes
    -- ** Core JSON classes
      FromJSON(..)
      -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
    -- * Functions
    , (.:)
    , (.:?)
    , (.!=)
    ) where

import qualified Data.Aeson as A
import Control.Applicative ((<$>), pure)
import Data.Cretheus.Types.Internal
import Control.Monad.Operational (singleton)
import Data.Text (Text, unpack)
import Data.Scientific (Scientific)
import Data.Typeable
import qualified Data.Vector as V


instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON Null   = pure Nothing
    parseJSON a      = Just <$> parseJSON a

instance FromJSON String where
    parseJSON = withText "String" (pure . unpack)

instance FromJSON Int where
    parseJSON = parseIntegral "Int"

instance FromJSON Integer where
    parseJSON = withScientific "Integral" $ pure . floor

instance FromJSON Text where
    parseJSON = withText "Text" pure

instance FromJSON Bool where
    parseJSON = withBool "Bool" pure

instance FromJSON Scientific where
    parseJSON = withScientific "Scientific" pure

instance (A.ToJSON a, Typeable a, FromJSON a) => FromJSON [a] where
    parseJSON = withArray "[a]" $ mapM (parseJSON . interpretV') . V.toList

instance (A.ToJSON a, Typeable a, FromJSON a) => FromJSON (V.Vector a) where
    parseJSON = withArray "Vector a" $ V.mapM (parseJSON . interpretV')

instance FromJSON Value where
    parseJSON a = pure a

withObject :: (A.ToJSON a, Typeable a) => String -> (A.Object -> ParserM a) -> Value -> ParserM a
withObject s f v = singleton $ WithObject s f v

withText :: (A.ToJSON a, Typeable a) => String -> (Text -> ParserM a) -> Value -> ParserM a
withText s f v = singleton $ WithText s f v

withArray :: (A.ToJSON a, Typeable a) => String -> (A.Array -> ParserM a) -> Value -> ParserM a
withArray s f v = singleton $ WithArray s f v

withScientific :: (A.ToJSON a, Typeable a) => String -> (Scientific -> ParserM a) -> Value -> ParserM a
withScientific s f v = singleton $ WithScientific s f v

withBool :: (A.ToJSON a, Typeable a) => String -> (Bool -> ParserM a) -> Value -> ParserM a
withBool s f v = singleton $ WithBool s f v

parseIntegral :: (A.ToJSON a, Typeable a, Integral a) => String -> Value -> ParserM a
parseIntegral expected = withScientific expected $ pure . floor

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (Typeable a,  A.ToJSON a, FromJSON a) => A.Object -> Text -> ParserM a
x .: t  = singleton $ x :.: t
{-# INLINE (.:) #-}


-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:)' instead.
(.:?) :: (Typeable b, FromJSON b, A.ToJSON b) => A.Object -> Text -> ParserM (Maybe b)
x .:? t = singleton $ x :.:? t
{-# INLINE (.:?) #-}

-- | Helper for use in combination with '.:?' to provide default
-- values for optional JSON object fields.
--
-- This combinator is most useful if the key and value can be absent
-- from an object without affecting its validity and we know a default
-- value to assign in that case.  If the key and value are mandatory,
-- use '(.:)' instead.
--
-- Example usage:
--
-- @ v1 <- o '.:?' \"opt_field_with_dfl\" .!= \"default_val\"
-- v2 <- o '.:'  \"mandatory_field\"
-- v3 <- o '.:?' \"opt_field2\"
-- @
(.!=) :: (Typeable a, A.ToJSON a, Show a) => ParserM (Maybe a) -> a -> ParserM a
p .!= t = singleton $ p :.!= t
{-# INLINE (.!=) #-}



