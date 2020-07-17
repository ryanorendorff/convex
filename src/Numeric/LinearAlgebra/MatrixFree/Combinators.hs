{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-|
Module      : Numeric.LinearAlgebra.MatrixFree.Static
Description : Compose matrix-free linear maps
Copyright   : (c) Ryan Orendorff, 2020
License     : BSD3
Stability   : experimental

Enables the user to compose matrix-free `LinearMap` objects in the same way
that one would compose regular matrices.
-}
module Numeric.LinearAlgebra.MatrixFree.Combinators
    ( (===)
    , (|||)
    , blockMatrix
    , blockDiag
    , LinearMapList(..)
    , applyLinearMapList
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra          ( Transposable(..) )
import           Numeric.LinearAlgebra.Static   ( (#)
                                                , split
                                                , R
                                                )
import           Numeric.LinearAlgebra.MatrixFree
                                                ( LinearMap(..), (##>))
import           Numeric.LinearAlgebra.Static.RList (Sum, RList(..))

-- | Vertically concatenate two `LinearMap`s \(\begin{bmatrix}A\\B\end{bmatrix}\)
(===) :: LinearMap m n -> LinearMap p n -> LinearMap (m + p) n
(===) (LinearMap f_top t_top) (LinearMap f_bot t_bot) = LinearMap f t
  where
    f v = f_top v # f_bot v
    t v = let (v1, v2) = split v in t_top v1 + t_bot v2

infixl 2 ===


-- | Horizontally concatenate two `LinearMap`s \(\begin{bmatrix}A&B\end{bmatrix}\)
(|||) :: LinearMap m n -> LinearMap m p -> LinearMap m (n + p)
(|||) (LinearMap f_top t_top) (LinearMap f_bot t_bot) = LinearMap f t
  where
    f v = let (v1, v2) = split v in f_top v1 + f_bot v2
    t v = t_top v # t_bot v

infixl 3 |||

-- | Generate a block matrix from four `LinearMap`s
-- \[
-- \begin{bmatrix}
--   A & B \\
--   C & D
-- \end{bmatrix}
-- \]
blockMatrix
    :: LinearMap m n             -- ^ Upper left matrix
    -> LinearMap m p             -- ^ Upper right matrix
    -> LinearMap q n             -- ^ Lower left matrix
    -> LinearMap q p             -- ^ Lower right matrix
    -> LinearMap (m + q) (n + p) -- ^ Resulting block matrix
blockMatrix a b c d = a ||| b === c ||| d


-- | A non-empty list of "LinearMap" matrices of different sizes.
data LinearMapList (ms :: [Nat]) (ns :: [Nat]) where
    LOne :: (KnownNat m, KnownNat n) => LinearMap m n -> LinearMapList '[m] '[n]
    (:-:) :: (KnownNat m, KnownNat n) => LinearMap m n -> LinearMapList ms ns
                                      -> LinearMapList (m ': ms) (n ': ns)


-- | Concatenate a "LinearMapList" of "LinearMap"s as a block diagonal.
-- Note that this has poor performance; it runs `split` on the
-- input vector O(n) times, where n is the number of entries in the list.
blockDiag :: LinearMapList ms ns -> LinearMap (Sum ms) (Sum ns)
blockDiag ls = case blockDiagWorker ls of
  LWorker lsW -> lsW

data LWorker m n where
  LWorker :: (KnownNat m, KnownNat n) => LinearMap m n -> LWorker m n

blockDiagWorker :: LinearMapList ms ns -> LWorker (Sum ms) (Sum ns)
blockDiagWorker (LOne l) = LWorker l
blockDiagWorker (l :-: ls) = case blockDiagWorker ls of
  LWorker lsW -> LWorker (diag2 l lsW)

-- Combine two `LinearMap`s on a diagonal without having to use
-- konst 0, which would just be a waste of processing time (sum a list and
-- multiply it by zero)
diag2 :: (KnownNat m, KnownNat n, KnownNat p, KnownNat q)
      => LinearMap m n
      -> LinearMap p q
      -> LinearMap (m + p) (n + q)
diag2 la lb = LinearMap (f la lb) (f (tr la) (tr lb))
  where
    f :: (KnownNat m', KnownNat n', KnownNat p', KnownNat q')
      => LinearMap m' n' -> LinearMap p' q' -> R (n' + q') -> R (m' + p')
    f l1 l2 v = let (v1, v2) = split v in
      (l1 ##> v1) # (l2 ##> v2)


-- | Apply a list of "LinearMap"s over a list of static vectors
applyLinearMapList :: LinearMapList ms ns -> RList ns -> RList ms
applyLinearMapList (LOne _) (_ ::: _) = error "This case is impossible"
applyLinearMapList (_ :-: _) (ROne _) = error "This case is impossible"
applyLinearMapList (LOne l) (ROne r) = ROne (l ##> r)
applyLinearMapList (l :-: ls) (r ::: rs) = (l ##> r) ::: applyLinearMapList ls rs
