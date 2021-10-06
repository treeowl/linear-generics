{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures #-}
#endif

#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

{-# LANGUAGE Trustworthy #-}

module Generics.Deriving.Traversable (
  -- * Generic Traversable class
    GTraversable(..)

  -- * Default method
  , gtraversedefault

  -- * Internal Traversable class
  , GTraversable'(..)

  ) where

import           Control.Applicative (Const, WrappedMonad(..), ZipList, liftA2)
import qualified Data.Monoid as Monoid (First, Last, Product, Sum)
import           Data.Monoid (Dual)

import           Generics.Deriving.Base
import           Generics.Deriving.Base.Internal ((.#))
import           Generics.Deriving.Foldable
import           Generics.Deriving.Functor

-- import           Data.Coerce
import           Data.Complex (Complex)

import           Data.Ord (Down)

import           Data.Proxy (Proxy)

import           Data.Functor.Identity (Identity)

import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, WrappedMonoid)

--------------------------------------------------------------------------------
-- Generic traverse
--------------------------------------------------------------------------------

class GTraversable' t where
  gtraverse' :: Applicative f => (t b -> r) -> (a -> f b) -> t a -> f r

instance GTraversable' V1 where
  gtraverse' _ _ x = pure $ case x of {}

instance GTraversable' U1 where
  gtraverse' fin _ U1 = pure (fin U1)

instance GTraversable' Par1 where
  gtraverse' fin f (Par1 a) = fmap (fin .# Par1) $ f a

instance GTraversable' (K1 i c) where
  gtraverse' fin _ (K1 a) = pure (fin (K1 a))

instance GTraversable' f => GTraversable' (M1 i c f) where
  gtraverse' fin f (M1 a) = gtraverse' (fin .# M1) f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :+: g) where
  gtraverse' fin f (L1 a) = gtraverse' (fin . L1) f a
  gtraverse' fin f (R1 a) = gtraverse' (fin . R1) f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :*: g) where
  gtraverse' fin f (a :*: b) = liftA2 (\a' b' -> fin (a' :*: b')) (gtraverse' id f a) (gtraverse' id f b)

instance (GTraversable' f, GTraversable g) => GTraversable' (f :.: g) where
  gtraverse' fin f (Comp1 x) = gtraverse' (fin .# Comp1) (gtraverse f) x

instance GTraversable' UAddr where
  gtraverse' fin _ (UAddr a) = pure (fin $ UAddr a)

instance GTraversable' UChar where
  gtraverse' fin _ (UChar c) = pure (fin $ UChar c)

instance GTraversable' UDouble where
  gtraverse' fin _ (UDouble d) = pure (fin $ UDouble d)

instance GTraversable' UFloat where
  gtraverse' fin _ (UFloat f) = pure (fin $ UFloat f)

instance GTraversable' UInt where
  gtraverse' fin _ (UInt i) = pure (fin $ UInt i)

instance GTraversable' UWord where
  gtraverse' fin _ (UWord w) = pure (fin $ UWord w)

class (GFunctor t, GFoldable t) => GTraversable t where
  gtraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
#if __GLASGOW_HASKELL__ >= 701
  default gtraverse :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                    => (a -> f b) -> t a -> f (t b)
  gtraverse = gtraversedefault
#endif

  gsequenceA :: Applicative f => t (f a) -> f (t a)
  gsequenceA = gtraverse id

  gmapM :: Monad m => (a -> m b) -> t a -> m (t b)
  gmapM f = unwrapMonad . gtraverse (WrapMonad . f)

  gsequence :: Monad m => t (m a) -> m (t a)
  gsequence = gmapM id

gtraversedefault :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                 => (a -> f b) -> t a -> f (t b)
gtraversedefault f x = gtraverse' to1 f (from1 x)

-- Base types instances
instance GTraversable ((,) a) where
  gtraverse = gtraversedefault

instance GTraversable [] where
  gtraverse = gtraversedefault

instance GTraversable (Arg a) where
  gtraverse = gtraversedefault

instance GTraversable Complex where
  gtraverse = gtraversedefault

instance GTraversable (Const m) where
  gtraverse = gtraversedefault

instance GTraversable Down where
  gtraverse = gtraversedefault

instance GTraversable Dual where
  gtraverse = gtraversedefault

instance GTraversable (Either a) where
  gtraverse = gtraversedefault

instance GTraversable Monoid.First where
  gtraverse = gtraversedefault

instance GTraversable (Semigroup.First) where
  gtraverse = gtraversedefault

instance GTraversable Identity where
  gtraverse = gtraversedefault

instance GTraversable Monoid.Last where
  gtraverse = gtraversedefault

instance GTraversable Semigroup.Last where
  gtraverse = gtraversedefault

instance GTraversable Max where
  gtraverse = gtraversedefault

instance GTraversable Maybe where
  gtraverse = gtraversedefault

instance GTraversable Min where
  gtraverse = gtraversedefault

instance GTraversable NonEmpty where
  gtraverse = gtraversedefault

instance GTraversable Monoid.Product where
  gtraverse = gtraversedefault

instance (GTraversable f, GTraversable g) => GTraversable (Functor.Product f g) where
  gtraverse = gtraversedefault

instance GTraversable Proxy where
  gtraverse = gtraversedefault

instance GTraversable Monoid.Sum where
  gtraverse = gtraversedefault

instance (GTraversable f, GTraversable g) => GTraversable (Functor.Sum f g) where
  gtraverse = gtraversedefault

instance GTraversable WrappedMonoid where
  gtraverse = gtraversedefault

instance GTraversable ZipList where
  gtraverse = gtraversedefault
