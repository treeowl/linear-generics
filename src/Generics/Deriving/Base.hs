{-# LANGUAGE CPP #-}

{-# LANGUAGE Trustworthy #-}

module Generics.Deriving.Base (module Generics.Deriving.Base.Internal) where

import Generics.Deriving.Base.Internal
  hiding (GHCGenerically (..), GHCGenerically1 (..), (.#), (#.))
import Generics.Deriving.Instances ()
