{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.LinearAlgebra.MatrixFree
  (
    LinearMap(..),
    fromL
  )

where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static


data LinearMap (m :: Nat) (n :: Nat) where
  -- | Construct a LinearMap from a forward and adjoint function
  LinearMap :: (KnownNat m, KnownNat n) =>
                    (R n -> R m) -- ^ Forward fucntion
                 -> (R m -> R n) -- ^ Adjoint function
                 -> LinearMap m n

fromL :: (KnownNat m, KnownNat n) => L m n -> LinearMap m n
fromL a = LinearMap (a #>) (tr a #>)

-- I'll need to think about how perform this map
-- toL :: (KnownNat m, KnownNat n) => LinearMap m n -> L m n
-- toL (LinearMap f _) = matrix $ concatMap (extract . f) (toColumns eye)
