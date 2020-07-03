{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Numeric.LinearAlgebra.Static.Util
Description : Functions to manipulate vectors and matrices
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental
-}
module Numeric.LinearAlgebra.Static.Util
    ( atM
    , atV
    , foldR
    , foldR1
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra         as LA
import           Data.Fin
import qualified Data.Vector.Storable          as V

---------------------------------------------------------------
-- Get data from vector/matrix with compile time bound check --
---------------------------------------------------------------

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
