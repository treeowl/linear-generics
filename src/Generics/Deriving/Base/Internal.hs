{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Deriving.Base.Internal
  ( Generic (..)
  , Generic1 (..)
  , (:.:)(..)
  , module GHCGenerics
  ) where
import GHC.Generics as GHCGenerics hiding (Generic (..), Generic1 (..), (:.:)(..), Rec1)
import qualified GHC.Generics as G
import Data.Kind (Type)

class Generic a where
  type family Rep a :: Type -> Type

  to :: forall {p} m. Rep a p %m-> a
  from :: forall {p} m. a %m-> Rep a p

class Generic1 f where
  type family Rep1 f :: k -> Type

  to1 :: forall p m. Rep1 f p %m-> f p
  from1 :: forall p m. f p %m-> Rep1 f p

newtype (f :.: g) x = Comp1 { unComp1 :: f (g x) }
  deriving (Eq, Ord, Show, Read, G.Generic, G.Generic1)
