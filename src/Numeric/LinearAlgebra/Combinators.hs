{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.LinearAlgebra.Combinators
    ( blockMatrix
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static


-- | Generate a block matrix from static matrices.
blockMatrix
    :: (KnownNat m, KnownNat n, KnownNat p, KnownNat q)
    => L m n -- ^ Upper left matrix
    -> L m p -- ^ Upper right matrix
    -> L q n -- ^ Lower left matrix
    -> L q p -- ^ Lower right matrix
    -> L (m + q) (n + p) -- ^ Resulting block matrix
blockMatrix a b c d = a ||| b === c ||| d
