{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | METAR observation result.
module Data.Aviation.Metar.METARResult where

import Control.Lens (makeClassy, makeClassyPrisms)
import Data.Aviation.Metar.METARError (METARError)
import Data.Eq.Deriving (deriveEq1)
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Extend (Extend (duplicated))
import Data.List.NonEmpty (NonEmpty)
import Text.Show.Deriving (deriveShow1)

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Data.Aviation.Metar.METARError
-- >>> import Data.Aviation.Metar.METARResult
-- >>> import Data.Functor.Apply ((<.>))
-- >>> import Data.Functor.Bind ((>>-))
-- >>> import Data.Functor.Extend (duplicated)
-- >>> import Data.List.NonEmpty (NonEmpty((:|)))
-- >>> import Network.Stream
{- FOURMOLU_ENABLE -}

-- | Either a METAR value or a non-empty list of everything that went wrong
-- while trying to obtain one.
--
-- >>> METARResultValue "METAR YSSY 280600Z" :: METARResult String
-- METARResultValue "METAR YSSY 280600Z"
-- >>> METARResultFailure (ParseErrorAt "NOAA" "HTTP 404" :| []) :: METARResult String
-- METARResultFailure (ParseErrorAt "NOAA" "HTTP 404" :| [])
data METARResult a
  = METARResultFailure (NonEmpty METARError)
  | METARResultValue a
  deriving (Eq, Show)

makeClassy ''METARResult
makeClassyPrisms ''METARResult
deriveEq1 ''METARResult
deriveShow1 ''METARResult

-- | 'fmap' rewraps 'METARResultValue', preserving the failure constructor.
--
-- >>> fmap (+ 1) (METARResultValue 3 :: METARResult Int)
-- METARResultValue 4
-- >>> fmap (+ 1) (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int)
-- METARResultFailure (ParseErrorAt "src" "why" :| [])
instance Functor METARResult where
  fmap _ (METARResultFailure es) =
    METARResultFailure es
  fmap f (METARResultValue a) =
    METARResultValue (f a)

-- | '<.>' fails fast on the left; two failures do not accumulate here (only
-- 'Data.Functor.Alt.<!>' accumulates).
--
-- >>> (METARResultValue (+ 1) :: METARResult (Int -> Int)) <.> METARResultValue 3
-- METARResultValue 4
-- >>> (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult (Int -> Int)) <.> METARResultValue 3
-- METARResultFailure (ParseErrorAt "src" "why" :| [])
-- >>> (METARResultValue (+ 1) :: METARResult (Int -> Int)) <.> METARResultFailure (ParseErrorAt "src" "why" :| [])
-- METARResultFailure (ParseErrorAt "src" "why" :| [])
instance Apply METARResult where
  METARResultFailure es <.> _ =
    METARResultFailure es
  METARResultValue f <.> METARResultValue a =
    METARResultValue (f a)
  METARResultValue _ <.> METARResultFailure es =
    METARResultFailure es

-- | 'pure' wraps a value with 'METARResultValue'.
--
-- >>> pure 3 :: METARResult Int
-- METARResultValue 3
instance Applicative METARResult where
  pure =
    METARResultValue
  (<*>) =
    (<.>)

-- | Kleisli bind: threads a value forward and short-circuits on failure.
--
-- >>> (METARResultValue 3 :: METARResult Int) >>- (\x -> METARResultValue (x + 1))
-- METARResultValue 4
-- >>> (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int) >>- (\x -> METARResultValue (x + 1))
-- METARResultFailure (ParseErrorAt "src" "why" :| [])
instance Bind METARResult where
  METARResultFailure es >>- _ =
    METARResultFailure es
  METARResultValue a >>- f =
    f a

-- | 'Monad' inherits from 'Bind' and 'Applicative'.
--
-- >>> (METARResultValue 3 :: METARResult Int) >>= (\x -> METARResultValue (x + 10))
-- METARResultValue 13
instance Monad METARResult where
  return =
    pure
  (>>=) =
    (>>-)

-- | Folding contributes the value once for 'METARResultValue', zero times for failure.
--
-- >>> foldr (+) 0 (METARResultValue 5 :: METARResult Int)
-- 5
-- >>> foldr (+) 0 (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int)
-- 0
instance Foldable METARResult where
  foldr f z (METARResultValue a) =
    f a z
  foldr _ z (METARResultFailure _) =
    z

-- | 'traverse' commutes 'METARResult' with an inner effect.
--
-- >>> traverse Just (METARResultValue 3 :: METARResult Int)
-- Just (METARResultValue 3)
-- >>> traverse Just (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int)
-- Just (METARResultFailure (ParseErrorAt "src" "why" :| []))
instance Traversable METARResult where
  traverse f (METARResultValue a) =
    METARResultValue <$> f a
  traverse _ (METARResultFailure es) =
    pure (METARResultFailure es)

-- | 'duplicated' nests a value inside a second layer.
--
-- >>> duplicated (METARResultValue 3 :: METARResult Int)
-- METARResultValue (METARResultValue 3)
-- >>> duplicated (METARResultFailure (ParseErrorAt "src" "why" :| []) :: METARResult Int)
-- METARResultFailure (ParseErrorAt "src" "why" :| [])
instance Extend METARResult where
  duplicated (METARResultValue a) =
    METARResultValue (METARResultValue a)
  duplicated (METARResultFailure es) =
    METARResultFailure es
