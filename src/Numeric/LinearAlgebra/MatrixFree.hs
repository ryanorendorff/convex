{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.LinearAlgebra.MatrixFree
    ( LinearMap(..)
    , (##>)
    , (*#)
    , (+#)
    , (<#>)
    , avgRepeatedFree
    , exactDimsFree
    , eyeFree
    , fromL
    , innerDimsFree
    , toL
    )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Type.Equality             ( (:~:)(Refl) )
import           GHC.TypeLits
import           Numeric.LinearAlgebra          ( Transposable )
import           Numeric.LinearAlgebra.Static
import qualified Data.Vector.Storable          as V


-- | A linear operator/map between two vector spaces
data LinearMap (m :: Nat) (n :: Nat) where
  LinearMap ::(KnownNat m, KnownNat n) =>
                    (R n -> R m) -- ^ Forward Function
                 -> (R m -> R n) -- ^ Adjoint Function
                 -> LinearMap m n

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


-- | The identity matrix in matrix free form.
eyeFree :: (KnownNat n) => LinearMap n n
eyeFree = LinearMap id id


-- | Take the average of a vector.
avg :: forall n. (KnownNat n) => R n -> Double
avg v = v <.> 1 / n' where n' = fromIntegral . natVal $ (Proxy :: Proxy n)


-- | mean(v) * 1, where 1 is the ones vector. of length v.
avgRepeatedFree :: (KnownNat n) => LinearMap n n
avgRepeatedFree = LinearMap f f where f v = konst (avg v)


-- | Remove the average value of a vector from each element of the vector.
removeAvg :: (KnownNat n) => LinearMap n n
removeAvg = eyeFree +# ((-1) *# avgRepeatedFree)

-- TODO: Define fft in matrix free form using FFT and IFFT

trLM :: (KnownNat n, KnownNat m) => LinearMap n m -> LinearMap m n
trLM (LinearMap f a) = LinearMap a f

instance  (KnownNat n, KnownNat m) =>
          Transposable (LinearMap n m) (LinearMap m n) where
    tr = trLM


-- Apply matrix free function to a vector
(##>) :: (KnownNat n, KnownNat m) => LinearMap n m -> R m -> R n
(##>) (LinearMap f a) = f


infixr 8 ##>

(<#>)
    :: (KnownNat m, KnownNat n, KnownNat p)
    => LinearMap m n
    -> LinearMap n p
    -> LinearMap m p
(<#>) (LinearMap f_a a_a) (LinearMap f_b a_b) =
    LinearMap (f_a . f_b) (a_b . a_a)

infixr 8 <#>


(*#) :: (KnownNat m, KnownNat n) => Double -> LinearMap m n -> LinearMap m n
(*#) s (LinearMap f a) = LinearMap (\v -> konst s * f v) (\v -> konst s * a v)
infixr 8 *#


(+#)
    :: (KnownNat m, KnownNat n)
    => LinearMap m n
    -> LinearMap m n
    -> LinearMap m n
(+#) (LinearMap f_a a_a) (LinearMap f_b a_b) =
    LinearMap (\v -> f_b v + f_a v) (\v -> a_b v + a_a v)

infixr 7 +#


exactDimsFree
    :: forall m n j k.
       (KnownNat n, KnownNat m, KnownNat j, KnownNat k) =>
       LinearMap m n
    -> Maybe (LinearMap j k)
exactDimsFree m@(LinearMap f b) = do
    Refl <- sameNat (Proxy :: Proxy m) (Proxy :: Proxy j)
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy k)
    return $ LinearMap f b

innerDimsFree
    :: forall m n p q.
       (KnownNat m, KnownNat n, KnownNat p, KnownNat q) =>
       LinearMap m n
    -> LinearMap p q
    -> Maybe (LinearMap m n, LinearMap p q, n :~: p)
innerDimsFree a@(LinearMap fa ba) b@(LinearMap fb bb) = do
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy p)
    return (LinearMap fa ba, LinearMap fb bb, Refl)
