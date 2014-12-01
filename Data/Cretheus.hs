{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Cretheus where

import Control.Monad
import Control.Applicative
import Data.Monoid
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Text

-- Compat

(.:) ::  FromJSON a => A.Object -> Text -> ReifiedP a
(.:)  = (:.:)

(.:?) :: FromJSON b => A.Object -> Text -> ReifiedP (Maybe b)
(.:?) = (:.:?)

(.!=) :: ReifiedP (Maybe a) -> a -> ReifiedP a
(.!=) = (:.!=)


class FromJSON a where
    parseJSON :: A.Value -> ReifiedP a

instance FromJSON a => A.FromJSON a where
    parseJSON = interpretR . parseJSON

data ReifiedP a where
    (:.:)  :: FromJSON a => A.Object -> Text -> ReifiedP a
    (:.:?) :: FromJSON b => A.Object -> Text -> ReifiedP (Maybe b)
    (:.!=) :: ReifiedP (Maybe a) -> a -> ReifiedP a
    (:>>=) :: ReifiedP a -> (a -> ReifiedP b) -> ReifiedP b
    Ret    :: a -> ReifiedP a
    Mzero  :: ReifiedP a
    Mplus  :: ReifiedP a -> ReifiedP a -> ReifiedP a


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


