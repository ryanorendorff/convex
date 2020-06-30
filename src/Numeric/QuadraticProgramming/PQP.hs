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

Solves the following quadratic program

\[
\textrm{min}_x\ \frac{1}{2} x^TAx + c^Tx \\
\textrm{s.t}\ B x <= d
\]

where \[A\] is positive definite and the primal problem above is feasible.

by solving the dual program with the following simple iterative update step.

\[
y_i \leftarrow y_i \left[\frac{h_i^- + (Q^-y)_i}{h_i^+ + (Q^+y)_i}\right]
\]

where

- \[i\] references the ith element of a vector.
- \[Q^+\] is the positive elements of the hessian of the dual quadratic
  program (ie \[Q^+ = \textrm{max}(Q, 0) + diag(r) \forall r \succeq 0\]).
  Similar statement for \[Q^-\] but with an absolute value. The \[r\] vector
  guarantees that the dual quadratic program is positive semidefinite after
  the thresholding, bounding the solution away from zero and infinity.
- \[h^+\] is the positive elements of the dual linear cost component
  (\[h^+ = \textrm{max}(Q, 0)\])

-}
module Numeric.QuadraticProgramming.PQP
  (pqp
  )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra         as LA
import           Data.Fin
import           Data.Maybe
import qualified Data.Vector.Storable          as V
import           Data.Tuple.HT                  ( fst3 )


-- TODO: This is pseudocode. Make real.
pqp
    :: (KnownNat m, KnownNat n)
    => (L n n) -- ^ Hessian of the quadratic program cost
    -> (R n)   -- ^ Linear component of the quadratic program cost
    -> (L m n) -- ^ Constraint matrix B
    -> (R n)   -- ^ Constraint vector d
    -> [R n] -- ^ Dual iterations
pqp a c b d = iterate update (konst 1)
  where
    q = b <> (inv a) <> (tr b)
    h = d + b <> (inv a) #> c
    r = (max-q  0) #> konst 1

    hPlus = max h 0
    hMinus = max (-h) 0

    qPlus = max q 0 + diag r
    qMinus = max (-q) 0 + diag r

    -- Initial guess must be > 0. We choose 1.
    -- Normally this is written out for each element index individually,
    -- which makes this problem embarassingly parallel. However, we are not
    -- concerned about the parallelizability but that the algorithm is
    -- implemented correctly. There is a part that is not parallel (Q^+y,
    -- Q^-y), but ideally the matrix-vector multiply is fast.
    update y = y * ((hMinus + (qMinus * y)) /
                    (hPlus + (qPlus * y)))

-- u = -Q^-1 (h + B^T y)
-- TODO: This is pseudocode. Make real.
dualToPrimalQP
    :: (KnownNat m, KnownNat n)
    => (L n n) -- ^ Hessian of the quadratic program cost
    -> (L m n) -- ^ Constraint matrix B
    -> (R n)   -- ^ Dual cost h
    -> (R n)   -- ^ Dual optimal value y*
    -> [R n]   -- ^ Dual iterations
