{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Metar.TAFResultT where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category((.))
import Control.Monad(Monad(return, (>>=)), ap)
import Data.Aviation.Metar.TAFResult(TAFResult(TAFResultValue, ConnErrorResult, ParseErrorResult))
import Data.Eq(Eq((==)))
import Data.Foldable(Foldable(foldr))
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Classes(Eq1, Show1, eq1, showsPrec1)
import Data.Functor.Extend(Extend(duplicated))
import Data.Ord((>))
import Data.Semigroup(Semigroup((<>)))
import Control.Lens hiding ((<.>))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Eq.Deriving(deriveEq1)
import Prelude(Show(showsPrec), showParen, showString)
import Text.Show.Deriving(deriveShow1)

newtype TAFResultT f a =
  TAFResultT
    (f (TAFResult a))

makeClassy ''TAFResultT
makeWrapped ''TAFResultT

instance (Eq a, Eq1 f) => Eq (TAFResultT f a) where
  TAFResultT x == TAFResultT y =
    eq1 x y

instance (Show a, Show1 f) => Show (TAFResultT f a) where
  showsPrec n (TAFResultT x) =
    showParen (n > 10) (showString "TafResultT " . showsPrec1 n x)

deriveEq1 ''TAFResultT
deriveShow1 ''TAFResultT

instance Functor f => Functor (TAFResultT f) where
  fmap f (TAFResultT x) =
    TAFResultT (fmap (fmap f) x)

instance Monad f => Apply (TAFResultT f) where
  (<.>) =
    ap

instance Monad f => Applicative (TAFResultT f) where
  pure =
    TAFResultT . pure . pure
  (<*>) =
    ap

instance Monad f => Bind (TAFResultT f) where
  (>>-) =
    (>>=)

instance Monad f => Monad (TAFResultT f) where
  return =
    pure
  TAFResultT x >>= f =
    TAFResultT
      (
        x >>= \x' ->
        case x' of
          TAFResultValue x'' ->
            let TAFResultT r = f x''
            in  r
          ConnErrorResult e ->
            pure (ConnErrorResult e)
          ParseErrorResult ->
            pure ParseErrorResult
      )

instance Foldable f => Foldable (TAFResultT f) where
  foldr f z (TAFResultT x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable f => Traversable (TAFResultT f) where
  traverse f (TAFResultT x) =
    TAFResultT <$> traverse (traverse f) x

instance Monad f => Alt (TAFResultT f) where
  TAFResultT x <!> TAFResultT y =
    TAFResultT
      (
        x >>= \x' ->
        case x' of
          TAFResultValue x'' ->
            pure (TAFResultValue x'')
          ConnErrorResult _ ->
            y
          ParseErrorResult ->
            y
      )

instance Extend f => Extend (TAFResultT f) where
  duplicated (TAFResultT x) =
    TAFResultT (fmap (TAFResultValue . TAFResultT) (duplicated x))

instance MonadIO f => MonadIO (TAFResultT f) where
  liftIO =
    TAFResultT . liftIO . fmap pure

instance MonadTrans TAFResultT where
  lift =
    TAFResultT . fmap pure

instance Monad f => Semigroup (TAFResultT f a) where
  (<>) =
    (<!>)
