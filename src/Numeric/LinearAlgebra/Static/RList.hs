{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-|
Module      : Numeric.LinearAlgebra.Static.RList
Description : List of static vectors (`R`s)
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental
-}
module Numeric.LinearAlgebra.Static.RList
    ( RList(..)
    , Sum
    , exRList
    , concat
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import Prelude hiding (concat)


-- TODO: Make a fold function, then this can be abstracted.
-- | Sum a type level list of naturals
type family Sum (xs :: [Nat]) where
    Sum '[] = 0
    Sum (n ': ns) = n + Sum ns

-- | A non-empty list of `R` vectors of different sizes.
-- The list is non-empty since we cannot create a vector of size 0 in a
-- reasonable way when using Data.Vector as the backend (as HMatrix).
data RList (ns :: [Nat]) where
    ROne :: (KnownNat n) => R n -> RList '[n]
    (:::) :: (KnownNat n) => R n -> RList ns -> RList (n ': ns)

infixr 6 :::

-- | An example of how to construct an RList.
exRList :: RList '[2, 3]
exRList = konst 1 ::: ROne (konst 2)

-- There is no null vector. Also the HMatrix library does the wrong thing
-- with this example.
--
-- λ> (konst 4 :: R 2) # (konst 0 :: R 0)
-- (vector [4.0,4.0,0.0] :: R 2)

-- | Concatenate an RList of vectors
--
-- Thank you Christiaan Baaij for figuring out how to do concat! This is
-- basically his solution to the problem, with a modification to work with
-- the non-empty version of RList. See the following gist for more details:
-- https://gist.github.com/ryanorendorff/d05c378b71829e3c0c33de462cb9a973
concat :: RList ns -> R (Sum ns)
concat rs = case concatWorker rs of
  RWorker rsW -> rsW

data RWorker n where
  RWorker :: KnownNat n => R n -> RWorker n

concatWorker :: RList ns -> RWorker (Sum ns)
concatWorker (ROne r) = RWorker r
concatWorker (r ::: rs) = case concatWorker rs of
  RWorker rsW -> RWorker (r # rsW)
