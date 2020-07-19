module LinearMap where

open import Data.List using (List; sum) renaming ([] to []ᴸ; _∷_ to _∷ᴸ_)
open import Data.Nat using (ℕ; _+_; zero; suc)
open import Data.Vec using (Vec; _++_) renaming ([] to []ⱽ; _∷_ to _∷ⱽ_)
open import Data.Product as Prod using (∃; ∃₂; _×_; _,_)

open import Function using (id)

open import VectorList using (VectorList; splitToVectorList; []ⱽᴸ; _∷ⱽᴸ_)

-- We could make the natural numbers implicit but then they do not
-- appear in the type signature directly and would have to be made explicit
-- constantly.
data LinearMap (A : Set) (m n : ℕ) : Set where
    LM : (Vec A n → Vec A m) -- Forward function
      → (Vec A m → Vec A n) -- Adjoint function
      → LinearMap A m n

idₗₘ : {A : Set} → {n : ℕ} → LinearMap A n n
idₗₘ {A} {n} = LM id id
