{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Numeric.LinearAlgebra.MatrixFree.Combinators
    ( (===)
    , (|||)
    , blockMatrix
    )
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static   ( (#)
                                                , split
                                                )
import           Numeric.LinearAlgebra.MatrixFree
                                                ( LinearMap(..) )

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

-- Can this function be written? Maybe require the impredicate work SPJ
-- talked about recently (shot in the dark there).
-- blockDiag
--     :: [(forall m n. LinearMap m n)] -> ???
