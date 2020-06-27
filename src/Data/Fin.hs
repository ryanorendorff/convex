{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}


module Data.Fin
  ( Fin(..)
  , fromFin
  )
where

import           Data.Proxy
import           GHC.TypeLits

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

-- | Convert a Fin into an Integral value. Always safe to do.
fromFin :: (Integral i) => Fin n -> i
fromFin (Fin p) = fromIntegral $ natVal p
