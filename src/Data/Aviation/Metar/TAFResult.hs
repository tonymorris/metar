{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Metar.TAFResult where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Lens(makeClassy, makeClassyPrisms)
import Control.Monad(Monad(return, (>>=)))
import Data.Eq(Eq)
import Data.Eq.Deriving(deriveEq1)
import Data.Foldable(Foldable(foldr))
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Extend(Extend(duplicated))
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Network.Stream(ConnError)
import Prelude(Show)
import Text.Show.Deriving(deriveShow1)

data TAFResult a =
  ConnErrorResult ConnError
  | ParseErrorResult
  | TAFResultValue a
  deriving (Eq, Show)

makeClassy ''TAFResult
makeClassyPrisms ''TAFResult
deriveEq1 ''TAFResult
deriveShow1 ''TAFResult

instance Functor TAFResult where
  fmap _ (ConnErrorResult e) =
    ConnErrorResult e
  fmap _ ParseErrorResult =
    ParseErrorResult
  fmap f (TAFResultValue a) =
    TAFResultValue (f a)

instance Apply TAFResult where
  ConnErrorResult e <.> _ =
    ConnErrorResult e
  ParseErrorResult <.> _ =
    ParseErrorResult
  TAFResultValue f <.> TAFResultValue a =
    TAFResultValue (f a)
  TAFResultValue _ <.> ConnErrorResult e =
    ConnErrorResult e
  TAFResultValue _ <.> ParseErrorResult =
    ParseErrorResult

instance Applicative TAFResult where
  pure =
    TAFResultValue
  (<*>) =
    (<.>)

instance Bind TAFResult where
  ConnErrorResult e >>- _ =
    ConnErrorResult e
  ParseErrorResult >>- _ =
    ParseErrorResult
  TAFResultValue a >>- f =
    f a

instance Monad TAFResult where
  return =
    pure
  (>>=) =
    (>>-)

instance Foldable TAFResult where
  foldr f z (TAFResultValue a) =
    f a z
  foldr _ z (ConnErrorResult _ ) =
    z
  foldr _ z ParseErrorResult =
    z

instance Traversable TAFResult where
  traverse f (TAFResultValue a) =
    TAFResultValue <$> f a
  traverse _ (ConnErrorResult e) =
    pure (ConnErrorResult e)
  traverse _ ParseErrorResult =
    pure ParseErrorResult

instance Alt TAFResult where
  TAFResultValue a <!> _ =
    TAFResultValue a
  ConnErrorResult _ <!> x =
    x
  ParseErrorResult <!> x =
    x

instance Extend TAFResult where
  duplicated (TAFResultValue a) =
    TAFResultValue (TAFResultValue a)
  duplicated (ConnErrorResult e) =
    ConnErrorResult e
  duplicated ParseErrorResult =
    ParseErrorResult

instance Semigroup (TAFResult a) where
  (<>) =
    (<!>)
