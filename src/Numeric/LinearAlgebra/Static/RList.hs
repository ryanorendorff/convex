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
    -- , concat
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

data RList (ns :: [Nat]) where
    RNil :: RList '[]
    (:::) :: (KnownNat n) => R n -> RList ns -> RList (n ': ns)

infixr 6 :::

-- | An example of how to construct an RList
exRList :: RList '[2, 3]
exRList = konst 1 ::: konst 2 ::: RNil

-- This does not work because the sum is not expanded
exRList2 :: R (Sum '[2, 3])
exRList2 = (konst 1 :: R 3) # (konst 2 :: R 2)

-- There is no null vector. Also the HMatrix library does the wrong thing
-- with this example.
--
-- λ> (konst 4 :: R 2) # (konst 0 :: R 0)
-- (vector [4.0,4.0,0.0] :: R 2)
-- This does not work but I think it is because I have lost the size of the
-- vector, so the size of r and rs are not known to the compiler.
concat :: RList ns -> R (Sum ns)
concat (r ::: RNil) = r
concat (r ::: rs) = r # (concat rs)
