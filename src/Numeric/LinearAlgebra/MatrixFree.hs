{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.LinearAlgebra.MatrixFree
    ( LinearMap(..)
    , fromL
    , eyeFree
    , avgRepeatedFree
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           Numeric.LinearAlgebra          ( Transposable )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Type.Equality             ( (:~:)(Refl) )


data LinearMap (m :: Nat) (n :: Nat) where
  -- | Construct a LinearMap from a forward and adjoint function
  LinearMap ::(KnownNat m, KnownNat n) =>
                    (R n -> R m) -- Forward Function
                 -> (R m -> R n) -- Adjoint Function
                 -> LinearMap m n

fromL :: (KnownNat m, KnownNat n) => L m n -> LinearMap m n
fromL a = LinearMap (a #>) (tr a #>)

-- TODO: Implement this!
-- I'll need to think about how perform this map
--   src/Numeric/LinearAlgebra/MatrixFree.hs:32:43: error:
--     • Couldn't match type ‘Data.Vector.Storable.Vector’ with ‘[]’
--         arising from a functional dependency between:
--           constraint ‘Sized ℝ (R m) []’ arising from a use of ‘extract’
--           instance ‘Sized ℝ (R n1) Data.Vector.Storable.Vector’
--             at <no location info>
--     • In the first argument of ‘(.)’, namely ‘extract’
--       In the first argument of ‘concatMap’, namely ‘(extract . f)’
--       In the second argument of ‘($)’, namely
--         ‘concatMap (extract . f) (toColumns eye)’
--
-- toL :: (KnownNat m, KnownNat n) => LinearMap m n -> L m n
-- toL (LinearMap f _) = matrix $ concatMap (extract . f) (toColumns eye)


eyeFree :: (KnownNat n) => LinearMap n n
eyeFree = LinearMap id id


avg :: forall n . (KnownNat n) => R n -> Double
avg v = v <.> 1 / n' where n' = fromIntegral . natVal $ (Proxy :: Proxy n)


avgRepeatedFree :: (KnownNat n) => LinearMap n n
avgRepeatedFree = LinearMap f f where f v = konst (avg v)


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

removeAvg :: (KnownNat n) => LinearMap n n
removeAvg = eyeFree +# ((-1) *# avgRepeatedFree)


exactDims'
    :: forall n m j k
     . (KnownNat n, KnownNat m, KnownNat j, KnownNat k)
    => LinearMap m n
    -> Maybe (LinearMap j k)
exactDims' m@(LinearMap f b) = do
    Refl <- sameNat (Proxy :: Proxy m) (Proxy :: Proxy j)
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy k)
    return $ LinearMap f b

innerDims
    :: forall n m p q
     . (KnownNat m, KnownNat n, KnownNat p, KnownNat q)
    => LinearMap m n
    -> LinearMap p q
    -> Maybe (LinearMap m n, LinearMap p q, n :~: p)
innerDims a@(LinearMap fa ba) b@(LinearMap fb bb) = do
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy p)
    return ((LinearMap fa ba), (LinearMap fb bb), Refl)