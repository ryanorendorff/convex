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
                    (R n -> R m) 
                 -> (R m -> R n) 
                 -> LinearMap m n

fromL :: (KnownNat m, KnownNat n) => L m n -> LinearMap m n
fromL a = LinearMap (a #>) (tr a #>)

-- TODO: Implement this!
-- I'll need to think about how perform this map
--   src/Numeric/LinearAlgebra/MatrixFree.hs:32:43: error:
--     • Couldn't match type ‘Data.Vector.Storable.Vector’ with ‘[]’
--         arising from a functional dependency between:
--           constraint ‘Sized ℝ (R m) []’ arising from a use of ‘extract’
--           instance ‘Sized ℝ (R n1) Data.Vector.Storable.Vector’
--             at <no location info>
--     • In the first argument of ‘(.)’, namely ‘extract’
--       In the first argument of ‘concatMap’, namely ‘(extract . f)’
--       In the second argument of ‘($)’, namely
--         ‘concatMap (extract . f) (toColumns eye)’
-- 
-- toL :: (KnownNat m, KnownNat n) => LinearMap m n -> L m n
-- toL (LinearMap f _) = matrix $ concatMap (extract . f) (toColumns eye)
