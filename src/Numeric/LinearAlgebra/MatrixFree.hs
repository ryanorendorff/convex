{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-|
Module      : Numeric.LinearAlgebra.MatrixFree
Description : Represent matrices as a pair of functions for forward and adjoint.
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental
-}
module Numeric.LinearAlgebra.MatrixFree
    ( LinearMap(..)
    , (##>)
    , (*#)
    , (+#)
    , (<#>)
    , avgRepeatedFree
    , exactDimsFree
    , eyeFree
    , removeAvg
    , fromL
    , innerDimsFree
    , toL
    , konst
    , diag
    , forward
    , adjoint
    )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Type.Equality             ( (:~:)(Refl) )
import           GHC.TypeLits
import           Numeric.LinearAlgebra          ( Transposable(..) )
import           Numeric.LinearAlgebra.Static hiding (konst, diag)
import qualified Numeric.LinearAlgebra.Static  as LS
import qualified Data.Vector.Storable          as V


-- | A linear operator/map between two vector spaces
data LinearMap (m :: Nat) (n :: Nat) where
  LinearMap :: (KnownNat m, KnownNat n) =>
               (R n -> R m) -- ^ Forward Function
            -> (R m -> R n) -- ^ Adjoint FunctionLS
            -> LinearMap m n

-- | Extract the foward function from a `LinearMap`
forward :: LinearMap m n -> (R n -> R m)
forward (LinearMap f _) = f

-- | Extract the adjoint function from a `LinearMap`
adjoint :: LinearMap m n -> (R m -> R n)
adjoint (LinearMap _ a) = a

instance Transposable (LinearMap n m) (LinearMap m n) where
   -- We do not have different functions for conjugate transposes in the
   -- LinearMap constructor (yet?)
    tr  (LinearMap f a) = LinearMap a f
    tr' = tr


---------------------------------------------------------------------
--        Conversions between matrices and matrix free forms       --
---------------------------------------------------------------------

-- | Convert a static matrix into the matrix free data type. This merely
-- wraps the matrix in a function (eta abstraction); it does not determine
-- the function associated with a given matrix.
fromL :: (KnownNat m, KnownNat n) =>
         L m n -- ^ Static matrix to convert to matrix free data type
      -> LinearMap m n
fromL a = LinearMap (a #>) (tr a #>)

-- | Convert a matrix free linear map into a static matrix.
--
-- This methods is very inefficient as it traverses several components
-- linearly several times.
toL :: (KnownNat m, KnownNat n) => LinearMap m n -> L m n
toL (LinearMap f _) =
    matrix $ concatMap (V.toList . extract . f) (toColumns eye)


---------------------------------------------------------------------------
--                           Example LinearMaps                          --
---------------------------------------------------------------------------

-- | The identity matrix in matrix free form.
eyeFree :: (KnownNat n) => LinearMap n n
eyeFree = LinearMap id id


-- | Take the average of a vector.
avg :: forall n. (KnownNat n) => R n -> Double
avg v = v <.> 1 / n' where n' = fromIntegral . natVal $ (Proxy :: Proxy n)


-- | mean(v) * 1, where 1 is the ones vector. of length v.
avgRepeatedFree :: (KnownNat n) => LinearMap n n
avgRepeatedFree = LinearMap f f where f v = LS.konst (avg v)


-- | Remove the average value of a vector from each element of the vector.
removeAvg :: (KnownNat n) => LinearMap n n
removeAvg = eyeFree +# ((-1) *# avgRepeatedFree)


-- | The constant matrix, which sums the vector and sets every element of
-- the resulting vector to that sum
konst :: (KnownNat m, KnownNat n) => Double -> LinearMap m n
konst k = LinearMap sumAndRepeat sumAndRepeat
  where
    sumAndRepeat :: (KnownNat p, KnownNat q) => R p -> R q
    sumAndRepeat v = LS.konst (v <.> LS.konst k)

-- | Convert a vector into a diagonal matrix
diag :: (KnownNat n) => R n -> LinearMap n n
diag r = LinearMap f f
  where
    f v = zipWithVector (*) r v

-- TODO: Define fft in matrix free form using FFT and IFFT

---------------------------------------------------------------------
--                Operators for composing LinearMaps               --
---------------------------------------------------------------------

-- | Apply matrix free LinearMap to a vector
(##>) :: LinearMap n m -> R m -> R n
(##>) (LinearMap f _) = f

infixr 8 ##>


-- | Matrix multiply of two `LinearMap`s
(<#>) :: LinearMap m n
      -> LinearMap n p
      -> LinearMap m p
(<#>) (LinearMap f_a a_a) (LinearMap f_b a_b) =
    LinearMap (f_a . f_b) (a_b . a_a)

infixr 8 <#>


-- | Multiply a LinearMap matrix by a constant real number
(*#) :: (KnownNat m, KnownNat n) => Double -> LinearMap m n -> LinearMap m n
(*#) s (LinearMap f a) = LinearMap (\v -> LS.konst s * f v) (\v -> LS.konst s * a v)

infixr 8 *#


-- | Add two `LinearMap`s together if they have the same shape.
(+#) :: (KnownNat m, KnownNat n) =>
        LinearMap m n
     -> LinearMap m n
     -> LinearMap m n
(+#) (LinearMap f_a a_a) (LinearMap f_b a_b) =
    LinearMap (\v -> f_b v + f_a v) (\v -> a_b v + a_a v)

infixr 7 +#


-----------------------------------------------------------------------------
--                    Runtime Proofs on LinearMap shape                    --
-----------------------------------------------------------------------------

-- | Useful for constraining two dependently typed LinearMap matrices to
-- match each other in dimensions when they are unknown at compile-time.
exactDimsFree :: forall m n j k.
                 (KnownNat n, KnownNat m, KnownNat j, KnownNat k) =>
                 LinearMap m n
              -> Maybe (LinearMap j k)
exactDimsFree (LinearMap f b) = do
    Refl <- sameNat (Proxy :: Proxy m) (Proxy :: Proxy j)
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy k)
    return $ LinearMap f b

-- | Useful for constraining that two dependently typed LinearMap matrices
-- match in their inner dimensions (enabling matrix multiplication) each
-- other in dimensions when they are unknown at compile-time.
innerDimsFree :: forall m n p q. (KnownNat n, KnownNat p) =>
                 LinearMap m n   -- ^ The left LinearMap in a matrix multiply
              -> LinearMap p q   -- ^ The right LinearMap in a matrix multiply
              -> Maybe (n :~: p) -- ^ Proof that the inner dimensions are equal
innerDimsFree (LinearMap _ _) (LinearMap _ _) = do
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy p)
    return (Refl)
