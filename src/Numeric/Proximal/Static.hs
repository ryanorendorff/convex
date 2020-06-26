{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.Proximal.Static
    ( Coord(..)
    , atM
    , atV
    , fista
    , fistaCost
    , foldR
    , foldR1
    , indicatorBox
    , l1Cost
    , l1Prox
    , lsqLipschitz
    , proxIndicatorBox
    , quadraticLipschitz
    , residuals
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra         as LA
import           Data.Proxy
import           Data.Maybe
import qualified Data.Vector.Storable          as V
import           Data.Tuple.HT                  ( fst3 )


------------------------------------------------------------------------
--   Get data from vector/matrix at known location (Fin simulation)  --
------------------------------------------------------------------------

-- TODO: move element selection to another module

-- TODO:
-- Currently GHC thinks that the k + 1 <= n (and similar constraints) are
-- redundant because there is no constraint that specifies that Coord must
-- be within the length of the vector. Put another way, the constraint is
-- specified but used on the value level instead of the type level, so GHC
-- doesn't understand why the constraint exists. Ideally Coord can be
-- replaced with something line Fin but that does not use peano numbers to
-- encode this property.

-- | Type level natural meant as an index into a vector or matrix
data Coord (n :: Nat) = Coord

-- | Get a value from a vector at a given coordinate/index with guarantee
-- that the element index is within the length of the vector at compile
-- time.
atV
    :: forall n k
     . (KnownNat n, KnownNat k, k + 1 <= n)
    => R n
    -> Coord k
    -> Double
atV v _ = extract v LA.! pos
    where pos = fromIntegral . natVal $ (undefined :: Proxy k)

-- | Get element from matrix at a given coordinate/index with guarantee that
-- the element index is within the shape of the matrix at compile time.
atM
    :: forall m n i j
     . (KnownNat m, KnownNat n, KnownNat i, KnownNat j, i + 1 <= m, j + 1 <= n)
    => L m n
    -> Coord i
    -> Coord j
    -> Double
atM m _ _ = extract m `LA.atIndex` (i, j)
  where
    i = fromIntegral . natVal $ (undefined :: Proxy i)
    j = fromIntegral . natVal $ (undefined :: Proxy j)

-- | Fold a vector.
foldR :: KnownNat n => (b -> Double -> b) -> b -> R n -> b
foldR f initial x = V.foldl' f initial (extract x)

-- | Fold a vector without an initial element.
foldR1 :: KnownNat n => (Double -> Double -> Double) -> R n -> Double
foldR1 f x = V.foldl1' f (extract x)


------------------------------------------------------------------
--  Lipschitz calculations on actual matrixes by max eigenvalue --
------------------------------------------------------------------

-- TODO: Implement solution for extracting k largest eigenvalues directly.

-- Note that these are not particularly efficient as each eigenvalue must be extracted and then searched through to find the maximum.
lsqLipschitz :: (KnownNat m, KnownNat n) => L m n -> Double
lsqLipschitz a = foldR1 max . eigenvalues $ mTm a

quadraticLipschitz :: (KnownNat n) => Sym n -> Double
quadraticLipschitz = foldR1 max . eigenvalues


---------------------------------------------------------
--                  Proximal Functions                 --
---------------------------------------------------------

-- | Returns the cost function for the box indicator function.
-- I(l, u) = {0 if l < xᵢ < u ∀ xᵢ ∈ x
--            ∞ otherwise}
indicatorBox :: Double -> Double -> (forall n . KnownNat n => R n -> Double)
indicatorBox l u =
    foldR (\a b -> a + (if (b > u) || (b < l) then 1 / 0 else 0)) 0

-- | Proximal operator for the box indicator function
proxIndicatorBox :: Double -> Double -> (forall n . KnownNat n => R n -> R n)
proxIndicatorBox l u = dvmap bound
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
fista grad_f prox_g lipschitz (fromMaybe (konst 0) -> x0) = map fst3
    $ iterate go (x0, x0, 1)
  where
        -- Sometimes update is called p_L
    gradient_step = \x -> x - konst (1 / lipschitz) * grad_f x
    prox_step     = prox_g
    update        = prox_step . gradient_step

    -- Algorithm here
    go (x, y, t) =
        let
            x_update = update y
            t_update = (1 + sqrt (1 + 4 * t ^^ (2 :: Int))) / 2
            y_update =
                x_update + konst ((t - 1) / t_update) * (x_update - x)
        in
            (x_update, y_update, t_update)

-- | Residuals for a list of vectors.
residuals :: forall n . KnownNat n => [R n] -> [Double]
residuals x = zipWith (\y yn -> norm_2 (y - yn)) x (tail x)

-----------------------------------------------------------------
--                        Cost Functions                       --
-----------------------------------------------------------------

-- | Takes the most smooth convex function `f` and the convex (potentially
-- non-smooth) function `g` and calculates the cost of a given input.
fistaCost :: (R n -> Double) -> (R n -> Double) -> R n -> Double
fistaCost f g x = f x + g x


-- | Cost function for an ℓ₁ term
l1Cost :: (KnownNat n) => Double -> R n -> Double
l1Cost lambda = (lambda *) . norm_1

-- | Proximal equation a the ℓ₁ term
l1Prox :: (KnownNat n) => Double -> R n -> R n
l1Prox lambda = dvmap f where f xi = signum xi * max (abs xi - lambda) 0
