module ControlSystems.DynamicSystems.Conversions (
  c2d
)

where

import Numeric.LinearAlgebra

type ℝ = Double

-- | Convert a continuous time linear differential system to a discrete form,
-- using state space models.
c2d :: Double   -- ^ The sampling time
    -> Matrix ℝ -- ^ The state transition matrix, often called A
    -> Matrix ℝ -- ^ The state input matrix, often called B
    -> (Matrix ℝ, Matrix ℝ) -- ^ The discretized linear differential system.
c2d t a b = (a_d, b_d)
  where
    -- First we have to extract the sizes of all the matrices at run time.
    (m_a, n_a) = (rows a, cols a)
    (_  , n_b) = (rows b, cols b)

    -- Then manually make the correct block. This must be a square matrix
    -- because the matrix exponential function (`expm`) expects to only have
    -- square inputs (given that it is a series expansion of matrix to higher
    -- powers). We need to form this matrix
    -- ⌈ A B ⌉
    -- ⌊ 0 0 ⌋
    block = a ||| b
              ===
              konst 0 (m_a, n_a + n_b) -- I left in a bug here! ;-D

    -- Calculate the discrete time block matrix, which is the following
    -- ⌈ A_d B_d ⌉
    -- ⌊  0   I  ⌋
    exp_block = expm (scale t block)

    -- Finally we can extract the correct submatrices assuming we pull out the
    -- right pieces.
    a_d = subMatrix (0, 0) (m_a, n_a) exp_block
    b_d = subMatrix (0, n_a) (m_a, n_b) exp_block
