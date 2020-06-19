{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.Proximal.Static
    ( fista
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra         as LA
import           Data.Proxy
import qualified Numeric.LinearProgramming.L1  as L1
import           Data.Maybe
import           System.Random
import qualified Data.Vector.Storable          as V
import           Data.Proxy
import           Data.Tuple.HT                  ( fst3 )


------------------------------------------------------------------------
--   Get data from vector/matrix at known location (Fin simulation)  --
------------------------------------------------------------------------

-- TODO: move this to another module
data Coord (n :: Nat) = Coord

atV
    :: forall n k
     . (KnownNat n, KnownNat k, k+1<=n)
    => R n
    -> Coord k
    -> Double
atV v _ = extract v LA.! pos
    where pos = fromIntegral . natVal $ (undefined :: Proxy k)

atM
    :: forall m n i j
     . (KnownNat m, KnownNat n, KnownNat i, KnownNat j, i+1<=m, j+1 <=n)
    => L m n
    -> Coord i
    -> Coord j
    -> Double
atM m _ _ = extract m `LA.atIndex` (pi, pj)
  where
    pi = fromIntegral . natVal $ (undefined :: Proxy i)
    pj = fromIntegral . natVal $ (undefined :: Proxy j)

foldR :: KnownNat n => (b -> Double -> b) -> b -> R n -> b
foldR f initial x = V.foldl' f initial (extract x)

foldR1 :: KnownNat n => (Double -> Double -> Double) -> R n -> Double
foldR1 f x = V.foldl1' f (extract x)


------------------------------------------------------------------
--  Lipschitz calculations on actual matrixes by max eigenvalue --
------------------------------------------------------------------

-- TODO: Implement solution for extracting k largest eigenvalues directly.

-- Note that these are not particularly efficient as each eigenvalue must be extracted and then searched through to find the maximum.
lsq_lipschitz :: (KnownNat m, KnownNat n) => L m n -> Double
lsq_lipschitz a = foldR1 max . eigenvalues $ mTm a

quadratic_lipschitz :: (KnownNat n) => Sym n -> Double
quadratic_lipschitz = foldR1 max . eigenvalues


---------------------------------------------------------
--                  Proximal Functions                 --
---------------------------------------------------------
indicator_box :: Double -> Double -> (forall n . KnownNat n => R n -> Double)
indicator_box l u =
    foldR (\a b -> a + (if (b > u) || (b < l) then 1 / 0 else 0)) 0

prox_indicator_box :: Double -> Double -> (forall n . KnownNat n => R n -> R n)
prox_indicator_box l u = dvmap bound
  where
    bound x = case (x < l, x > u) of
        (True, _   ) -> l
        (_   , True) -> u
        _            -> x


-- | Fast Iterative Soft Thresholding Algorithm
-- Used to solve problems of the form F(x) = f(x) + g(x), where
-- f is a smooth convex function with grad F being L-Lipschitz continuous and
-- g is a (potentially non-smooth) convex function. Common problems solved
-- with FISTA include min ||Ax - b|| st. x \in C or min ||Ax - b|| + ||x||_1
fista
    :: (KnownNat n)
    => (R n -> R n)    -- ^ gradient of f
    -> (R n -> R n)    -- ^ proximal operator of g
    -> Double          -- ^ Lipschitz constant
    -> Maybe (R n)     -- ^ Initial guess at a solution
    -> [R n]           -- ^ Output cost function and optimal solution over time
fista grad_f prox_g lipschitz (maybe (konst 0) id -> x0) = map fst3
    $ iterate go (x0, x0, 1)
  where
        -- Sometimes update is called p_L
    gradient_step = \x -> x - (konst (1 / lipschitz)) * grad_f x
    prox_step     = \x -> prox_g x
    update        = prox_step . gradient_step

    -- Algorithm here
    go (x, y, t) =
        let
            x_update = update y
            t_update = (1 + sqrt (1 + 4 * t ^^ 2)) / 2
            y_update =
                x_update + (konst $ (t - 1) / (t_update)) * (x_update - x)
        in
            (x_update, y_update, t_update)

residuals :: forall n . KnownNat n => [R n] -> [Double]
residuals x = zipWith (\x xn -> norm_2 (x - xn)) x (tail x)

-----------------------------------------------------------------
--                        Cost Functions                       --
-----------------------------------------------------------------

-- | Takes the most smooth convex function `f` and the convex (potentially non-smooth) function `g` and calculates the cost of a given input.
fista_cost :: KnownNat n => (R n -> Double) -> (R n -> Double) -> R n -> Double
fista_cost f g x = f x + g x


l1_cost :: (KnownNat n) => Double -> R n -> Double
l1_cost lambda = (lambda *) . norm_1

l1_prox :: (KnownNat n) => Double -> R n -> R n
l1_prox lambda = dvmap f where f xi = signum xi * max (abs xi - lambda) 0
