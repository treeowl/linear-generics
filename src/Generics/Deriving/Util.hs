{-# language GADTs #-}
{-# language LinearTypes #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}
{-# language Unsafe #-}

-- | Unsafe and semi-safe utility functions used various places.
module Generics.Deriving.Util
  ( toLinear
  , (.#)
  , (#.)
  ) where
import GHC.Exts (TYPE, RuntimeRep)
import Unsafe.Coerce (unsafeEqualityProof, UnsafeEquality (..))
import Data.Coerce (Coercible, coerce)

-- Stolen from linear-base

toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p q.
     (a %p-> b) %1-> (a %q-> b)
toLinear = case unsafeEqualityProof @p @q of
  UnsafeRefl -> \f -> f

-- Stolen from profunctors

infixr 9 #.
(#.) :: Coercible b c => p b c -> (a -> b) -> a -> c
(#.) _ = coerce

infixl 8 .#
(.#) :: Coercible a b => (b -> c) -> p a b -> a -> c
f .# _ = coerce f
