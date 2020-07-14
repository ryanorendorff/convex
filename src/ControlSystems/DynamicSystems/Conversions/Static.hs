{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- The unused variables are left as documentation
{-# OPTIONS -Wno-unused-local-binds #-}

{-|
Module      : ControlSystems.DynamicSystems.Conversion
Description : Converts continuous time systems to discrete time, using static vectors.
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental
-}
module ControlSystems.DynamicSystems.Conversions.Static (
  c2d
) where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

-- | Convert a continuous time linear differential system to a discrete form,
-- using state space models, using type level naturals.
--
-- Specifically note this implementation against
-- `ControlSystems.DynamicSystems.Conversions.c2d`, which takes many more
-- code to handle the complexities of creating a block matrix of the right
-- size and then splitting it apart again.
c2d :: forall m n. (KnownNat m, KnownNat n) =>
       Double         -- ^ The sampling time
    -> L n n          -- ^ The state transition matrix, often called A
    -> L n m          -- ^ The state input matrix, often called B
    -> (L n n, L n m) -- ^ The discretized A & B system
c2d t a b = discreteSystem
  where
    block = a ||| b  -- [ A B ]
              ===    -- [ 0 0 ]
               0

    -- Calculate the discrete time block matrix, which is the following
    -- ⌈ A_d B_d ⌉
    -- ⌊  0   I  ⌋
    exp_block = expm (konst t * block)

    -- Finally we can extract the correct submatrices assuming we pull out
    -- the right pieces. Note that we do not need to pass the indices here
    -- directly; the type system ensures that we pull out the correct A_d
    -- and B_d parts of the exp_block matrix.
    discreteSystem@(a_d, b_d) = splitCols $ fst (splitRows exp_block)
