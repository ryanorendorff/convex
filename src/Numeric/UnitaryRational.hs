{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.UnitaryRational
    ( UnitaryRational(..)
    , unitaryToPair
    , unitaryToFractional
    )
where

import           GHC.TypeLits
import           Data.Proxy

-- A type level rational that is between zero and one. This is not a quotient
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


unitaryToPair
    :: forall n m
     . (KnownNat n, KnownNat m)
    => UnitaryRational n m
    -> (Integer, Integer)
unitaryToPair _ = (num, denom)
  where
    num   = natVal $ (undefined :: Proxy n) :: Integer
    denom = natVal $ (undefined :: Proxy m) :: Integer


unitaryToFractional
    :: forall n m o
     . (KnownNat n, KnownNat m, Fractional o)
    => UnitaryRational n m
    -> o
unitaryToFractional x = (fromIntegral num) / (fromIntegral denom)
    where (num, denom) = unitaryToPair x
