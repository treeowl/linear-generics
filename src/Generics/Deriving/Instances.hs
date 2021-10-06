{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

#define GENERICALLY(t) deriving via GHCGenerically(t) instance Generic (t)
#define GENERICALLY1(t) deriving via GHCGenerically1(t) instance Generic1 (t)

module Generics.Deriving.Instances (
  -- Instances only
  ) where

import Control.Applicative
import Data.Complex (Complex)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import qualified Data.Monoid as M
import Data.Proxy (Proxy)
import Data.Version (Version)
import System.Exit (ExitCode)
import Data.Ord (Down)
import Control.Arrow (Kleisli(..))
import Generics.Deriving.Base.Internal
import qualified GHC.Generics as GHCG
import qualified Data.Functor.Sum as FSum
import qualified Data.Functor.Product as FProd

GENERICALLY ((a, b))
GENERICALLY ((a, b, c))
GENERICALLY ((a, b, c, d))
GENERICALLY ((a, b, c, d, e))
GENERICALLY ((a, b, c, d, e, f))
GENERICALLY ((a, b, c, d, e, f, g))

GENERICALLY1 ((,) a)
GENERICALLY1 ((,,) a b)
GENERICALLY1 ((,,,) a b c)
GENERICALLY1 ((,,,,) a b c d)
GENERICALLY1 ((,,,,,) a b c d e)
GENERICALLY1 ((,,,,,,) a b c d e f)

-- TODO: once the Template Haskell stuff is fully operational,
-- use it to derive these when necessary.
#if MIN_VERSION_base(4,16,0)
GENERICALLY ((a, b, c, d, e, f, g, h))
GENERICALLY ((a, b, c, d, e, f, g, h, i))
GENERICALLY ((a, b, c, d, e, f, g, h, i, j))
GENERICALLY ((a, b, c, d, e, f, g, h, i, j, k))
GENERICALLY ((a, b, c, d, e, f, g, h, i, j, k, l))
GENERICALLY ((a, b, c, d, e, f, g, h, i, j, k, l, m))
GENERICALLY ((a, b, c, d, e, f, g, h, i, j, k, l, m, n))
GENERICALLY ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))

GENERICALLY1 (((,,,,,,,) a b c d e f g))
GENERICALLY1 (((,,,,,,,,) a b c d e f g h))
GENERICALLY1 (((,,,,,,,,,) a b c d e f g h i))
GENERICALLY1 (((,,,,,,,,,,) a b c d e f g h i j))
GENERICALLY1 (((,,,,,,,,,,,) a b c d e f g h i j k))
GENERICALLY1 (((,,,,,,,,,,,,) a b c d e f g h i j k l))
GENERICALLY1 (((,,,,,,,,,,,,,) a b c d e f g h i j k l m))
GENERICALLY1 (((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n))
#endif

GENERICALLY (Identity a)
GENERICALLY1 (Identity)

GENERICALLY (Kleisli m a b)

-- TODO:  instance Generic1 (Kleisli m a)

GENERICALLY (Down a)
GENERICALLY1 (Down)

-----

GENERICALLY (ExitCode)
GENERICALLY (Version)
GENERICALLY1 (f :+: g)
GENERICALLY1 (f :*: g)
GENERICALLY1 (K1 i c)

-- TODO: instance Generic1 (f :.: g)


GENERICALLY1 (M1 i c f)
GENERICALLY1 (Par1)
GENERICALLY1 (U1)
GENERICALLY (V1 p)
GENERICALLY1 (V1)
GENERICALLY (UAddr p)
GENERICALLY1 (UAddr)
GENERICALLY (UChar p)
GENERICALLY1 (UChar)
GENERICALLY (UDouble p)
GENERICALLY1 (UDouble)
GENERICALLY (UFloat p)
GENERICALLY1 (UFloat)
GENERICALLY (UInt p)
GENERICALLY1 (UInt)
GENERICALLY (UWord p)
GENERICALLY1 (UWord)
GENERICALLY (Complex a)
GENERICALLY1 (Complex)
GENERICALLY (Proxy a)
GENERICALLY1 (Proxy)

-----

--------------------------------------------------------------------------------
-- Representations for base types
--------------------------------------------------------------------------------


GENERICALLY (S.All)
GENERICALLY (S.Any)

GENERICALLY (S.Min a)
GENERICALLY1 (S.Min)

GENERICALLY (S.Max a)
GENERICALLY1 (S.Max)

GENERICALLY (M.Alt f a)
GENERICALLY1 (M.Alt f)

GENERICALLY (S.Arg a b)
GENERICALLY1 (S.Arg a)

-----

GENERICALLY ((f GHCG.:.: g) a)

-----

GENERICALLY (Fixity)
GENERICALLY (Associativity)
GENERICALLY (DecidedStrictness)
GENERICALLY (SourceStrictness)
GENERICALLY (SourceUnpackedness)

-----

GENERICALLY (Const a b)
GENERICALLY1 (Const a)

-----

GENERICALLY (S.Dual a)
GENERICALLY1 (S.Dual)

GENERICALLY (S.Endo a)

GENERICALLY (S.First a)
GENERICALLY1 (S.First)

GENERICALLY (M.First a)
GENERICALLY1 (M.First)

-----

GENERICALLY (S.Last a)
GENERICALLY1 (S.Last)

GENERICALLY (M.Last a)
GENERICALLY1 (M.Last)

-----

GENERICALLY (S.Product a)
GENERICALLY1 (S.Product)

GENERICALLY (S.Sum a)
GENERICALLY1 (S.Sum)

GENERICALLY (S.WrappedMonoid a)
GENERICALLY1 (S.WrappedMonoid)

-----

GENERICALLY (WrappedArrow a b c)
GENERICALLY1 (WrappedArrow a b)


GENERICALLY (WrappedMonad m a)
GENERICALLY1 (WrappedMonad m)

GENERICALLY (ZipList a)
GENERICALLY1 (ZipList)

-----

GENERICALLY (U1 p)

GENERICALLY (Par1 p)
GENERICALLY (K1 i c p)
GENERICALLY (M1 i c f p)
GENERICALLY ((f :+: g) p)
GENERICALLY ((f :*: g) p)
GENERICALLY ((f :.: g) p)

-----

GENERICALLY ([a])
GENERICALLY1 ([])
GENERICALLY (NonEmpty a)
GENERICALLY1 (NonEmpty)
GENERICALLY (Either a b)
GENERICALLY1 (Either a)
GENERICALLY (Maybe a)
GENERICALLY1 (Maybe)

-----

GENERICALLY (FSum.Sum f g a)
GENERICALLY1 (FSum.Sum f g)

GENERICALLY (FProd.Product f g a)
GENERICALLY1 (FProd.Product f g)

-----

GENERICALLY (Bool)
-- TODO: Char, Double, Int, Float
GENERICALLY (Ordering)
GENERICALLY (())
