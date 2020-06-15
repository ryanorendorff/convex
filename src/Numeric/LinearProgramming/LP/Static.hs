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



module Numeric.LinearProgramming.LP.Static (
  affineScaling
)
where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra.Static as S
import qualified Numeric.LinearAlgebra as LA
import Numeric.UnitaryRational

-- https://en.wikipedia.org/wiki/Karmarkar%27s_algorithm
affineScaling :: (KnownNat m, KnownNat n, KnownNat p, KnownNat q, 1 <= n, 1 <= m) => L m n -> R m -> R n -> R n -> UnitaryRational p q -> [Maybe (R n)]
affineScaling a b c x0 gamma = iterate (>>= go) (Just x0)
    where
        gamma' = unitaryToFractional gamma

        min_vh :: (KnownNat n) => R n -> R n -> Double
        min_vh v h = minimum . map fst . filter ((<0) . snd) $ vals_with_h
            where
                vals = zipWithVector (\vi hi -> -vi/hi) v h
                vals_with_h = zip (LA.toList $ extract vals) (LA.toList $ extract h)

        go xk = let
            vk = b - a #> xk
            dm_negsq = diag $ dvmap (**(-2)) vk
            hx = inv ((tr a) S.<> dm_negsq S.<> a) #> c
            hv = ((konst (-1)) * a #> hx)
            alpha = gamma' * min_vh vk hv
            xkp1 = xk + (konst alpha) * hx
            in 
                if ((>=0) . maximum . LA.toList . extract $ hv)
                then Nothing
                else Just xkp1

-- Example
-- -------
--
-- p = vector [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0] :: R 11
-- 
-- -- min c^Tx
-- c = vec2 1 1
-- 
-- -- s.t Ax <= b
-- a = ((konst 2) * (col p)) ||| (1 :: L 11 1)
-- b = p**2 + 1
-- 
-- lpSol = affineScaling a b c (konst 0) (UnitaryRational :: UnitaryRational 50 100)
-- 
-- mapM_ (disp 10 . fromJust) . take 20 $ lpSol
