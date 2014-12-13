{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cretheus.Schema.Schema ( toSchemas ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Text hiding (singleton, filter)
import qualified Data.Aeson as A

import Data.Cretheus.Types.Internal
import Data.Cretheus.Schema.Types

import Debug.Trace

