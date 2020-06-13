{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}

{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.LinearAlgebra.Combinators
    ( blockMatrix
    )
where

import           Numeric.LinearAlgebra.Static


blockMatrix
    :: forall m n p q
     . (KnownNat m, KnownNat n, KnownNat p, KnownNat q)
    => L m n
    -> L m p
    -> L q n
    -> L q p
    -> L (m + q) (n + p)
blockMatrix a b c d = a ||| b === c ||| d
