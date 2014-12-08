module Data.Cretheus.Types
    (
    -- * Core JSON types
    Value(..)
    {-, Array-}
    {-, emptyArray-}
    {-, Pair-}
    {-, Object-}
    {-, emptyObject-}
    -- * Convenience types and functions
    {-, DotNetTime(..)-}
    {-, typeMismatch-}
    -- * Type conversion
    , Parser(..)
    {-, Result(..)-}
    , FromJSON(..)
    {-, fromJSON-}
    {-, parse-}
    {-, parseEither-}
    {-, parseMaybe-}
    {-, ToJSON(..)-}
    {-, modifyFailure-}
    -- #ifdef GENERICS
    -- ** Generic JSON classes
    {-, GFromJSON(..)-}
    {-, GToJSON(..)-}
    {-, genericToJSON-}
    {-, genericParseJSON-}
    -- #endif
    -- * Inspecting @'Value's@
    {-, withObject-}
    , withText
    {-, withArray-}
    {-, withNumber-}
    {-, withScientific-}
    {-, withBool-}
    -- * Constructors and accessors
    {-, (.=)-}
    , (.:)
    , (.:?)
    , (.!=)
    {-, object-}
    -- * Generic and TH encoding configuration
    {-, Options(..)-}
    {-, SumEncoding(..)-}
    {-, camelTo-}
    {-, defaultOptions-}
    {-, defaultTaggedObject-}
) where

import Data.Cretheus.Types.Instances
import Data.Cretheus.Types.Internal
