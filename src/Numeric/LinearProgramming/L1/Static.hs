{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- TODO: This module is in the wrong module (these are not linear programs)
{-|
Module      : Numeric.LinearProgramming.L1.Static
Description : Solve l1 programs with static vectors/matrices.
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental
-}
module Numeric.LinearProgramming.L1.Static (
  l1Solve,
  l1SolveGT,
  l1SolveO,
  lInfSolveO,
  l1SolveU
)

where

import           Data.Maybe (fromJust)
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearProgramming.L1 as L1

-- Internal only. Could lead to runtime exceptions except in this case we trust
-- the output from the solvers is the correct dimensions.
mkL :: KnownNat n => LA.Vector ℝ -> R n
mkL = fromJust . create


-- | Solution in the L_1 norm, with L_1 regularization, of a linear system @Ax=b@.
--
-- @argmin_x  λ||x||_1 + ||Ax-b||_1@
l1Solve :: (KnownNat m, KnownNat n) =>
           Double -- ^ λ
        -> L m n  -- ^ A
        -> R m    -- ^ b
        -> R n    -- ^ x
l1Solve λ (extract -> a) (extract -> b) = mkL $ L1.l1Solve λ a b


-- | Solution in the L_1 norm, with L_1 regularization, of a system of linear inequalities @Ax>=b@.
--
-- @argmin_x  λ||x||_1 + ||step(b-Ax)||_1@
l1SolveGT :: (KnownNat m, KnownNat n) =>
             Double -- ^ λ
          -> L m n  -- ^ A
          -> R m    -- ^ b
          -> R n    -- ^ x
l1SolveGT λ (extract -> a) (extract -> b) = mkL $ L1.l1SolveGT λ a b


-- | L_1 solution of overconstrained system Ax=b.
--
-- @argmin_x ||Ax-b||_1@
l1SolveO :: (KnownNat m, KnownNat n) =>
            L m n -- ^ A
         -> R m   -- ^ b
         -> R n   -- ^ x
l1SolveO (extract -> a) (extract -> b) = mkL $ L1.l1SolveO a b


-- | L_inf solution of overconstrained system Ax=b.
--
-- @argmin_x ||Ax-b||_inf@
lInfSolveO :: (KnownNat m, KnownNat n) =>
              L m n -- ^ A
           -> R m   -- ^ b
           -> R n   -- ^ x
lInfSolveO (extract -> a) (extract -> b) = mkL $ L1.lInfSolveO a b


-- | L1 solution of underconstrained linear system Ax=b.
--
-- @argmin_x ||x||_1 such that Ax=b@
l1SolveU :: (KnownNat m, KnownNat n) =>
            L m n -- ^ A
         -> R m   -- ^ b
         -> R n   -- ^ x
l1SolveU (extract -> a) (extract -> b) = mkL $ L1.l1SolveU a b
