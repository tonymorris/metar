{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- | Monad transformer over 'METARResult'.
module Data.Aviation.Metar.METARResultT where

import Control.Lens hiding ((<.>))
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Aviation.Metar.METARResult (METARResult (METARResultFailure, METARResultValue))
import Data.Eq.Deriving (deriveEq1)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Classes (Eq1, Show1, eq1, showsPrec1)
import Data.Functor.Extend (Extend (duplicated))
import Text.Show.Deriving (deriveShow1)

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Data.Aviation.Metar.METARError
-- >>> import Data.Aviation.Metar.METARResult
-- >>> import Data.Aviation.Metar.METARResultT
-- >>> import Data.Functor.Identity (Identity(Identity, runIdentity))
-- >>> import Data.Functor.Alt ((<!>))
-- >>> import Data.Functor.Apply ((<.>))
-- >>> import Data.Functor.Bind ((>>-))
-- >>> import Data.Functor.Extend (duplicated)
-- >>> import Data.List.NonEmpty (NonEmpty((:|)))
-- >>> import Network.Stream
{- FOURMOLU_ENABLE -}

-- | Wraps @f (METARResult a)@ so error propagation happens automatically inside @f@.
newtype METARResultT f a
  = METARResultT
      (f (METARResult a))

makeClassy ''METARResultT
makeWrapped ''METARResultT

-- | Equality on the wrapped @f@.
--
-- >>> METARResultT (Identity (METARResultValue (3 :: Int))) == METARResultT (Identity (METARResultValue 3))
-- True
-- >>> METARResultT (Identity (METARResultValue (3 :: Int))) == METARResultT (Identity (METARResultFailure (ParseErrorAt "src" "why" :| [])))
-- False
instance (Eq a, Eq1 f) => Eq (METARResultT f a) where
  METARResultT x == METARResultT y =
    eq1 x y

-- | Show delegates to the underlying @f (METARResult a)@.
--
-- >>> show (METARResultT (Identity (METARResultValue (3 :: Int))))
-- "TafResultT Identity (METARResultValue 3)"
instance (Show a, Show1 f) => Show (METARResultT f a) where
  showsPrec n (METARResultT x) =
    showParen (n > 10) (showString "TafResultT " . showsPrec1 n x)

deriveEq1 ''METARResultT
deriveShow1 ''METARResultT

-- | Maps under both layers.
--
-- >>> let METARResultT x = fmap (+ 1) (METARResultT (Identity (METARResultValue (3 :: Int)))) in runIdentity x
-- METARResultValue 4
instance (Functor f) => Functor (METARResultT f) where
  fmap f (METARResultT x) =
    METARResultT (fmap (fmap f) x)

-- | 'Apply' derived from 'ap' — sequences two effectful lookups.
--
-- >>> let a = METARResultT (Identity (METARResultValue ((+ 1) :: Int -> Int)))
-- >>> let b = METARResultT (Identity (METARResultValue (3 :: Int)))
-- >>> let METARResultT x = a <.> b in runIdentity x
-- METARResultValue 4
instance (Monad f) => Apply (METARResultT f) where
  (<.>) =
    ap

-- | 'pure' injects a value through both layers.
--
-- >>> let METARResultT x = (pure 5 :: METARResultT Identity Int) in runIdentity x
-- METARResultValue 5
instance (Monad f) => Applicative (METARResultT f) where
  pure =
    METARResultT . pure . pure
  (<*>) =
    ap

-- | 'Bind' equals '>>=' on the underlying monad.
--
-- >>> let m = METARResultT (Identity (METARResultValue (3 :: Int)))
-- >>> let METARResultT x = m >>- (\v -> pure (v + 10)) in runIdentity x
-- METARResultValue 13
instance (Monad f) => Bind (METARResultT f) where
  (>>-) =
    (>>=)

-- | Short-circuiting bind: 'METARResultFailure' propagates unchanged.
--
-- >>> let ok = METARResultT (Identity (METARResultValue (3 :: Int)))
-- >>> let METARResultT x = ok >>= (\v -> pure (v * 2)) in runIdentity x
-- METARResultValue 6
-- >>> let bad = METARResultT (Identity (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int))
-- >>> let METARResultT x = bad >>= (\v -> pure (v * 2)) in runIdentity x
-- METARResultFailure (ParseErrorAt "src" "why" :| [])
instance (Monad f) => Monad (METARResultT f) where
  return =
    pure
  METARResultT x >>= f =
    METARResultT
      ( x >>= \case
          METARResultValue x'' ->
            let METARResultT r = f x''
             in r
          METARResultFailure es ->
            pure (METARResultFailure es)
      )

-- | Fold sees the underlying value if there is one.
--
-- >>> foldr (+) 0 (METARResultT (Identity (METARResultValue (5 :: Int))))
-- 5
-- >>> foldr (+) 0 (METARResultT (Identity (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int)))
-- 0
instance (Foldable f) => Foldable (METARResultT f) where
  foldr f z (METARResultT x) =
    foldr (flip (foldr f)) z x

-- | Traverse commutes the effect and the transformer.
--
-- >>> traverse Just (METARResultT (Identity (METARResultValue (5 :: Int))))
-- Just (TafResultT (Identity (METARResultValue 5)))
instance (Traversable f) => Traversable (METARResultT f) where
  traverse f (METARResultT x) =
    METARResultT <$> traverse (traverse f) x

-- | Left-biased choice that /accumulates/ failures: try the left; on failure,
-- run the right; if the right also fails, concatenate their error lists.
-- The first success wins.
--
-- >>> let ok = METARResultT (Identity (METARResultValue (1 :: Int)))
-- >>> let alt = METARResultT (Identity (METARResultValue (2 :: Int)))
-- >>> let METARResultT x = ok <!> alt in runIdentity x
-- METARResultValue 1
-- >>> let bad = METARResultT (Identity (METARResultFailure (ParseErrorAt "a" "1" :| []) :: METARResult Int))
-- >>> let METARResultT x = bad <!> alt in runIdentity x
-- METARResultValue 2
-- >>> let bad2 = METARResultT (Identity (METARResultFailure (ParseErrorAt "b" "2" :| []) :: METARResult Int))
-- >>> let METARResultT x = bad <!> bad2 in runIdentity x
-- METARResultFailure (ParseErrorAt "a" "1" :| [ParseErrorAt "b" "2"])
instance (Monad f) => Alt (METARResultT f) where
  METARResultT x <!> METARResultT y =
    METARResultT
      ( x >>= \case
          METARResultValue a ->
            pure (METARResultValue a)
          METARResultFailure es ->
            y >>= \case
              METARResultValue a ->
                pure (METARResultValue a)
              METARResultFailure es' ->
                pure (METARResultFailure (es <> es'))
      )

-- | 'duplicated' nests the value inside a fresh 'METARResultValue' inside the transformer.
--
-- >>> let m = METARResultT (Identity (METARResultValue (7 :: Int)))
-- >>> let METARResultT (Identity outer) = duplicated m in case outer of METARResultValue (METARResultT (Identity inner)) -> inner; _ -> METARResultFailure (ParseErrorAt "impossible" "" :| [])
-- METARResultValue 7
instance (Extend f) => Extend (METARResultT f) where
  duplicated (METARResultT x) =
    METARResultT (fmap (METARResultValue . METARResultT) (duplicated x))

-- | 'liftIO' lifts an 'IO' action into the transformer.
--
-- >>> let METARResultT act = (liftIO (pure (5 :: Int)) :: METARResultT IO Int) in act
-- METARResultValue 5
instance (MonadIO f) => MonadIO (METARResultT f) where
  liftIO =
    METARResultT . liftIO . fmap pure

-- | 'lift' wraps an @f a@ so it becomes a successful @METARResultT f a@.
--
-- >>> let METARResultT x = (lift (Identity (5 :: Int)) :: METARResultT Identity Int) in runIdentity x
-- METARResultValue 5
instance MonadTrans METARResultT where
  lift =
    METARResultT . fmap pure

-- | 'Semigroup' is 'Alt': accumulates failures, first success wins.
--
-- >>> let bad = METARResultT (Identity (METARResultFailure (ParseErrorAt "a" "1" :| []) :: METARResult Int))
-- >>> let ok  = METARResultT (Identity (METARResultValue (9 :: Int)))
-- >>> let METARResultT x = bad <> ok in runIdentity x
-- METARResultValue 9
instance (Monad f) => Semigroup (METARResultT f a) where
  (<>) =
    (<!>)
