{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-
This allows us to simplify the constraints we need to specify up front for
KnownNat constraints. For example, the following function

f :: forall n . (KnownNat n, KnownNat (n+2)) => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy (n+2))

can be simplified to

f :: forall n . KnownNat n => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy (n+2))
-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- Allows us to equate terms like (a + b) + c and a + (b + c) as the same thing.
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Main where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as LA
import           Data.Proxy
import           Numeric.LinearAlgebra.MatrixFree
import qualified Numeric.LinearProgramming.L1 as L1
import qualified Numeric.LinearProgramming.L1.Static as L1S
import Data.Maybe (fromJust)
import ControlSystems.DynamicSystems.Conversions.Static
import System.Random
import qualified Data.Vector.Storable as V


f :: forall n . KnownNat n => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy (n+2))

l1_example :: (R 1000, LA.Vector ℝ, R 1000, LA.Vector ℝ)
l1_example = (signal_static, x, x_static, lsq_sol)
  where
    m = 200
    n = 1000
    s = 30 -- Sparsity, number of non-zero elements in the signal

    -- Random state transition matrix
    a = LA.reshape n $ LA.randomVector 0 LA.Gaussian (n*m)

    -- Create a signal with `s` elements being either -1 or 1.
    g = mkStdGen 0
    indx = take s $ randomRs (0, n - 1) g :: [Int]
    r = take s $ fmap ([-1, 1] !!) $ randomRs (0, 1) g
    signal = V.replicate n 0 V.// zip indx r
    b = a LA.#> signal

    -- Make dependently typed versions of the signal.
    a_static = fromJust $ create a :: L 200 1000
    signal_static = fromJust $ create signal :: R 1000
    b_static = a_static #> signal_static

    λ = 1
    x = L1.l1Solve λ a b
    x_static = L1S.l1Solve λ a_static b_static

    lsq_sol = a LA.<\> b

(signal, x, x_static, lsq_sol) = l1_example

main :: IO ()
main = do
  putStrLn "Hello, World!"
  print $ f (Proxy @3)
