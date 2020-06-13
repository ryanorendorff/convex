{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module ControlSystems.DynamicSystems.Conversions.Static (
  c2d
) where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

-- Convert continuous time linear dynamical system to discrete time using a
-- given sampling period.
c2d :: forall m n. (KnownNat m, KnownNat n) => Double -> L n n -> L n m -> (L n n, L n m)
c2d t a b = splitCols ab
  where
    ab = fst (splitRows exp_block)
    exp_block = expm (konst t * block)
    block = a ||| b
              ===
               0
