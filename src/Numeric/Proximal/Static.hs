{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.Proximal.Static
    ( Fin(..)
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

-- | Set of natural numbers < n ([0, ..., n - 1]). These are constructed
-- using typelits, which has some convenient machinery and does not require
-- the very inefficient Peano encoding.
--
-- One gotcha to this encoding is that the error messages are cryptic; the
-- errors only mention that the type level 'False does not equal the type
-- level 'True, instead of saying that the size constraint (k + 1 <= n) is
-- being violated.
data Fin (n :: Nat) where
  Fin :: (KnownNat k, k + 1 <= n) => Proxy k -> Fin n

instance (KnownNat n) => Show (Fin n) where
  show f = show (fromFin f :: Integer)

instance (KnownNat n) => Eq (Fin n) where
  (==) a b = (fromFin a :: Integer) == fromFin b

instance (KnownNat n) => Ord (Fin n) where
  compare a b = (fromFin a :: Integer) `compare` fromFin b

-- Fin 0 has no inhabitants! It is the same as Void
instance (KnownNat n, 1 <= n) => Bounded (Fin n) where
  minBound = Fin (Proxy :: Proxy 0)
  maxBound = Fin (Proxy :: Proxy (n - 1))

fromFin :: (Integral i) => Fin n -> i
fromFin (Fin p) = fromIntegral $ natVal p

-- | Get a value from a vector at a given coordinate/index with guarantee
-- that the element index is within the length of the vector at compile
-- time.
atV
    :: forall n
     . (KnownNat n)
    => R n
    -> Fin n
    -> Double
atV v p = extract v LA.! (fromFin p)

-- | Get element from matrix at a given coordinate/index with guarantee that
-- the element index is within the shape of the matrix at compile time.
atM
    :: forall m n
     . (KnownNat m, KnownNat n)
    => L m n
    -> Fin m
    -> Fin n
    -> Double
atM mat m n = extract mat `LA.atIndex` (fromFin m, fromFin n)

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
