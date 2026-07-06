{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Metar.METARResult where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Lens(makeClassy, makeClassyPrisms)
import Control.Monad(Monad(return, (>>=)))
import Data.Eq(Eq)
import Data.Eq.Deriving(deriveEq1)
import Data.Foldable(Foldable(foldr))
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Extend(Extend(duplicated))
import Data.Traversable(Traversable(traverse))
import Network.Stream(ConnError)
import Prelude(Show)
import Text.Show.Deriving(deriveShow1)

data METARResult a =
  ConnErrorResult ConnError
  | ParseErrorResult
  | METARResultValue a
  deriving (Eq, Show)

makeClassy ''METARResult
makeClassyPrisms ''METARResult
deriveEq1 ''METARResult
deriveShow1 ''METARResult

instance Functor METARResult where
  fmap _ (ConnErrorResult e) =
    ConnErrorResult e
  fmap _ ParseErrorResult =
    ParseErrorResult
  fmap f (METARResultValue a) =
    METARResultValue (f a)

instance Apply METARResult where
  ConnErrorResult e <.> _ =
    ConnErrorResult e
  ParseErrorResult <.> _ =
    ParseErrorResult
  METARResultValue f <.> METARResultValue a =
    METARResultValue (f a)
  METARResultValue _ <.> ConnErrorResult e =
    ConnErrorResult e
  METARResultValue _ <.> ParseErrorResult =
    ParseErrorResult

instance Applicative METARResult where
  pure =
    METARResultValue
  (<*>) =
    (<.>)

instance Bind METARResult where
  ConnErrorResult e >>- _ =
    ConnErrorResult e
  ParseErrorResult >>- _ =
    ParseErrorResult
  METARResultValue a >>- f =
    f a

instance Monad METARResult where
  return =
    pure
  (>>=) =
    (>>-)

instance Foldable METARResult where
  foldr f z (METARResultValue a) =
    f a z
  foldr _ z (ConnErrorResult _ ) =
    z
  foldr _ z ParseErrorResult =
    z

instance Traversable METARResult where
  traverse f (METARResultValue a) =
    METARResultValue <$> f a
  traverse _ (ConnErrorResult e) =
    pure (ConnErrorResult e)
  traverse _ ParseErrorResult =
    pure ParseErrorResult

instance Extend METARResult where
  duplicated (METARResultValue a) =
    METARResultValue (METARResultValue a)
  duplicated (ConnErrorResult e) =
    ConnErrorResult e
  duplicated ParseErrorResult =
    ParseErrorResult
