{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Data.Cretheus.Types.Instances
    (
    -- * Type classes
    -- ** Core JSON classes
      FromJSON(..)
      -- * Inspecting @'Value's@
    , withText
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
import Data.Typeable


instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON Null   = pure Nothing
    parseJSON a      = Just <$> parseJSON a

instance FromJSON String where
    parseJSON = withText "String" (pure . unpack)

instance FromJSON Value where
    parseJSON a = pure a
    {-# INLINE parseJSON #-}

withText :: (Typeable a) => String -> (Text -> ParserM a) -> Value -> ParserM a
withText s f v = singleton $ WithText s f v

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (Typeable a, FromJSON a) => A.Object -> Text -> ParserM a
x .: t  = singleton $ x :.: t
{-# INLINE (.:) #-}


-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:)' instead.
(.:?) :: (Typeable b, FromJSON b) => A.Object -> Text -> ParserM (Maybe b)
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
(.!=) :: (Typeable a, Show a) => ParserM (Maybe a) -> a -> ParserM a
p .!= t = singleton $ p :.!= t
{-# INLINE (.!=) #-}


-- internal instances

instance A.ToJSON SchemaH where
    toJSON (SchemaH typ xs reqs) =
        A.object [ "type" A..= typ
                 , "properties" A..= A.object xs
                 , "required" A..= reqs
                 ]
