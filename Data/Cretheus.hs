{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Aeson hiding ( FromJSON(..), Object, (.:), (.:?), (.!=)
                         , Value(..),
                         )
import Data.Data
import Data.Maybe (fromJust)
import Data.Cretheus.Types
import Data.Cretheus.Schema.Schema

----Test------------------------------------------------------------------


data Test1 = Test1 { field1 :: Int
                   , field2 :: String
                   } deriving (Show, Typeable)

-- 'A.decode' still works...
t1 :: Maybe Test1
t1 = decode "{\"field1\": \"Field 1\", \"field2\": \"Field 2\"}"


t1'' = toSchemas (fromJust t1)


instance FromJSON Test1 where
    parseJSON (Object v) = do
        -- As long as you're not inspecting 'f1' and 'f2' things are ok
        f1 <-  v .: "field1"
        f2 <- (v .:? "field2") .!= "default-string"
        return (Test1 f1 f2)
    parseJSON _            = mzero
