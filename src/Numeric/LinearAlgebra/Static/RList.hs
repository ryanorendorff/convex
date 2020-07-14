{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
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
    , LList(..)
    , Sum
    , exRList
    , concat
    , blockDiag
    , mapRListSquare
    , applyLList
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


-- | A non-empty list of `L` matrices of different sizes.
-- The list is non-empty since we cannot create a matrix of size 0 in a
-- reasonable way when using Data.Vector as the backend (as HMatrix).
data LList (ms :: [Nat]) (ns :: [Nat]) where
    LOne :: (KnownNat m, KnownNat n) => L m n -> LList '[m] '[n]
    (:-:) :: (KnownNat m, KnownNat n) => L m n -> LList ms ns
                                      -> LList (m ': ms) (n ': ns)


-- | Concatenate a LList of matrices as a block diagonal.
blockDiag :: LList ms ns -> L (Sum ms) (Sum ns)
blockDiag ls = case blockDiagWorker ls of
  LWorker lsW -> lsW

data LWorker m n where
  LWorker :: (KnownNat m, KnownNat n) => L m n -> LWorker m n

blockDiagWorker :: LList ms ns -> LWorker (Sum ms) (Sum ns)
blockDiagWorker (LOne l) = LWorker l
blockDiagWorker (l :-: ls) = case blockDiagWorker ls of
  LWorker lsW -> LWorker (l ||| konst 0 === konst 0 ||| lsW)

-- | Map some function that preserves the dimension of the vector (a square
-- matrix) over a list of vectors.
mapRListSquare :: (forall n. KnownNat n => R n -> R n)
               -> RList ns -> RList ns
mapRListSquare f (ROne r) = ROne (f r)
mapRListSquare f (r ::: rs) = f r ::: mapRListSquare f rs

-- I have not proved that the LList and RList are the same length, so GHC
-- complains that I have non-exhaustive patterns that cannot be constructed.
-- TODO: Add comment on how this would work in Agda to show how this type
-- of error would be avoided.
-- | Apply a list of static matrices over a list of static vectors
applyLList :: LList ms ns -> RList ns -> RList ms
applyLList (LOne _) (_ ::: _) = error "This case is impossible"
applyLList (_ :-: _) (ROne _) = error "This case is impossible"
applyLList (LOne l) (ROne r) = ROne (l #> r)
applyLList (l :-: ls) (r ::: rs) = (l #> r) ::: applyLList ls rs

-- This function is more complicated, may need to reflect out the position
-- we are in the list in order to write this function.
-- splitRList :: R (Sum ns) -> RList ns
-- splitRList vec = let (r, rs) = split vec in r ::: (splitRList rs)
