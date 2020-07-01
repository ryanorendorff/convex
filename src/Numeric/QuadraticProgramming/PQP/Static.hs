{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-|
Module      : Data.Fin
Description : Data type for set of natural numbers <= n
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental

Solve a QP with positive definite hessian using the method known as Parallel
Quadratic Programming (PQP).

Reference: Brand, Matthew, et al. "A parallel quadratic programming algorithm for model predictive control." IFAC Proceedings Volumes 44.1 (2011): 1031-1039.

Link: https://www.merl.com/publications/docs/TR2011-056.pdf
-}
module Numeric.QuadraticProgramming.PQP.Static
    ( pqp
    , dualToPrimalQPPoint
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           Prelude                 hiding ( (<>) )


-- | Solves the following quadratic program
--
-- \[
-- \textrm{min}_x\ \frac{1}{2} x^TAx + c^Tx \\
-- \textrm{s.t}\ B x <= d
-- \]
--
-- where \(A\) is positive definite and the primal problem above is feasible.
--
-- by solving the dual program with the following simple iterative update step.
--
-- \[
-- y_i \leftarrow y_i \left[\frac{h_i^- + (Q^-y)_i}{h_i^+ + (Q^+y)_i}\right]
-- \]
--
-- where
--
-- - \(i\) references the ith element of a vector.
-- - \(Q^+\) is the positive elements of the hessian of the dual quadratic
--   program (ie \(Q^+ = \textrm{max}(Q, 0) + diag(r), r \succeq 0\)).
--   Similar statement for \(Q^-\) but with an absolute value.
--   The \(r\) vector guarantees that the dual quadratic program is positive
--   semidefinite after the thresholding, bounding the solution away from
--   zero and infinity.
-- - \(h^+\) is the positive elements of the dual linear cost component
--   (\(h^+ = \textrm{max}(Q, 0)\))
pqp
    :: (KnownNat m, KnownNat n)
    => L n n -- ^ Hessian of the quadratic program cost, A
    -> R n   -- ^ Linear component of the quadratic program cost
    -> L m n -- ^ Constraint matrix B
    -> R m   -- ^ Constraint vector d
    -> [R m] -- ^ Dual iterations

-- Initial guess must be > 0. Absent a better heuristic we use 1.
pqp a c b d = iterate update (konst 1)
  where
    -- Lift max functions from operating on doubles to operating on vectors
    -- and matrices
    -- TODO: extract out into top level definition
    max0v  = dvmap (max 0)
    max0m  = dmmap (max 0)

    -- Convert to dual QP
    q      = b <> (inv a) <> (tr b)
    h      = d + (b <> (inv a)) #> c

    -- Need any r >= 0 in order to make the dual update step positive
    -- semidefinite.
    r      = max0m (-q) #> konst 1

    hPlus = max0v h
    hMinus = max0v (-h)

    qPlus  = max0m q + diag r
    qMinus = max0m (-q) + diag r

    -- Normally this is written out for each element index individually,
    -- which makes this problem embarassingly parallel. However, we are not
    -- concerned about the parallelizability but that the algorithm is
    -- implemented correctly. There is a part that is not parallel (Q⁺y,
    -- Q⁻y), but ideally the matrix-vector multiply is fast.
    update y = y * ((hMinus + (qMinus #> y)) / (hPlus + (qPlus #> y)))

-- | Convert a point in the dual space to the primal space using the formula
--
-- \[
-- x = A^{-1} (c + B^T y)
-- \]
dualToPrimalQPPoint
    :: (KnownNat m, KnownNat n)
    => L n n -- ^ Hessian of the quadratic program cost, A
    -> R n   -- ^ cost c
    -> L m n -- ^ Constraint matrix B
    -> R m   -- ^ Dual value y
    -> R n   -- ^ Primal value
dualToPrimalQPPoint a c b y = (-(inv a #> (c + (tr b) #> y)))

-- TODO: Define other direction (I suppose not as useful in this case)
