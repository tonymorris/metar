{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aviation.Metar.METARResultT where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category((.))
import Control.Monad(Monad(return, (>>=)), ap)
import Data.Aviation.Metar.METARResult(METARResult(METARResultValue, ConnErrorResult, ParseErrorResult))
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

newtype METARResultT f a =
  METARResultT
    (f (METARResult a))

makeClassy ''METARResultT
makeWrapped ''METARResultT

instance (Eq a, Eq1 f) => Eq (METARResultT f a) where
  METARResultT x == METARResultT y =
    eq1 x y

instance (Show a, Show1 f) => Show (METARResultT f a) where
  showsPrec n (METARResultT x) =
    showParen (n > 10) (showString "TafResultT " . showsPrec1 n x)

deriveEq1 ''METARResultT
deriveShow1 ''METARResultT

instance Functor f => Functor (METARResultT f) where
  fmap f (METARResultT x) =
    METARResultT (fmap (fmap f) x)

instance Monad f => Apply (METARResultT f) where
  (<.>) =
    ap

instance Monad f => Applicative (METARResultT f) where
  pure =
    METARResultT . pure . pure
  (<*>) =
    ap

instance Monad f => Bind (METARResultT f) where
  (>>-) =
    (>>=)

instance Monad f => Monad (METARResultT f) where
  return =
    pure
  METARResultT x >>= f =
    METARResultT
      (
        x >>= \x' ->
        case x' of
          METARResultValue x'' ->
            let METARResultT r = f x''
            in  r
          ConnErrorResult e ->
            pure (ConnErrorResult e)
          ParseErrorResult ->
            pure ParseErrorResult
      )

instance Foldable f => Foldable (METARResultT f) where
  foldr f z (METARResultT x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable f => Traversable (METARResultT f) where
  traverse f (METARResultT x) =
    METARResultT <$> traverse (traverse f) x

instance Monad f => Alt (METARResultT f) where
  METARResultT x <!> METARResultT y =
    METARResultT
      (
        x >>= \x' ->
        case x' of
          METARResultValue x'' ->
            pure (METARResultValue x'')
          ConnErrorResult _ ->
            y
          ParseErrorResult ->
            y
      )

instance Extend f => Extend (METARResultT f) where
  duplicated (METARResultT x) =
    METARResultT (fmap (METARResultValue . METARResultT) (duplicated x))

instance MonadIO f => MonadIO (METARResultT f) where
  liftIO =
    METARResultT . liftIO . fmap pure

instance MonadTrans METARResultT where
  lift =
    METARResultT . fmap pure

instance Monad f => Semigroup (METARResultT f a) where
  (<>) =
    (<!>)
