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

This module introduces `Fin`, which is a type that describes natural numbers
less than it's index. For example, `Fin 3` can be inhabited by the numbers
`[0, 1, 2]`.

There are many ways to implement this data type, the most common being peano
numbers. However this  implementation aims to do two things in particular.

- Be efficient in the representation. Peano numbers take up $O(n)$ space
  where $n$ is the actual number itself.
- Work with the existing type level TypeLits framework and the GHC
  extensions that make working with them easier ("GHC.TypeLits.Normalise"
  and "GHC.TyleIts.KnowNat.Solver").

The other approaches are great too! And more theoretically sound. Here is a
list of similar modules on Hackage.

- fin
- finite-typelits
- type-natural
- peano
- nat
- PeanoWitnesses
- type-combinators

-}
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
  -- | Constructing a `Fin` is done with a `Proxy` object which stores the
  -- type level natural that is within the bounds of `Fin n`.
  -- `Fin (Proxy :: Proxy 3) :: Fin 4`
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
