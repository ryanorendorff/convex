{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-|
Module      : Numeric.UnitaryRational
Description : Data type that is constrained to be between 0 and 1 using types.
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental
-}
module Numeric.UnitaryRational
    ( UnitaryRational(..)
    , unitaryToPair
    , unitaryToFractional
    )
where

import           GHC.TypeLits
import           Data.Proxy

-- | A type level rational that is between zero and one. This is not a quotient
-- type; two values that are the same under reduction (ex: 1/2 and 2/4) are not
-- the same here. This could be done with a smart constructor; to guarantee
-- this property at the type level we would need something like quotient types.
data UnitaryRational (n :: Nat) (m :: Nat) where
    UnitaryRational ::(KnownNat n, KnownNat m, 1 <= m, n <= m) => UnitaryRational n m


instance (KnownNat n, KnownNat m) => Show (UnitaryRational n m) where
    show _ = "UnitaryRational " ++ show n' ++ " " ++ show m'
      where
        n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
        m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int


-- | Converts a UnitaryRational to a numerator and denominator.
unitaryToPair
    :: forall n m
     . (KnownNat n, KnownNat m)
    => UnitaryRational n m -- ^ The UnitaryRational to convert
    -> (Integer, Integer)  -- ^ Pair of (numerator, denominator)
unitaryToPair _ = (num, denom)
  where
    num   = natVal $ (undefined :: Proxy n) :: Integer
    denom = natVal $ (undefined :: Proxy m) :: Integer


-- | Convert a UnitaryRational to a floating point number.
unitaryToFractional
    :: forall n m o
     . (KnownNat n, KnownNat m, Fractional o)
    => UnitaryRational n m  -- ^ The UnitaryRational to convert
    -> o                    -- ^ The Fractional representation of the input
unitaryToFractional x = (fromIntegral num) / (fromIntegral denom)
    where (num, denom) = unitaryToPair x
