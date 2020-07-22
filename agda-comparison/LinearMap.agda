module LinearMap where

open import Data.List using (List; sum) renaming ([] to []ᴸ; _∷_ to _∷ᴸ_)
open import Data.Nat using (ℕ; zero; suc)
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

_ᵀ : ∀ {A : Set} {m n} → LinearMap A m n → LinearMap A n m
LM f a ᵀ = LM a f

_·_ : ∀ {A : Set} {m n} → LinearMap A m n → Vec A n → Vec A m
LM f a · x = f x

-- I can't define this yet because it would need the fact that A has
-- the _+_ binary function.
-- _+_ : {A : Set} → {m n : ℕ} → LinearMap A m n
--                              → LinearMap A n n
--                              → LinearMap A m n
-- _+_ M₁ M₂ = LM (λ v → M₁ · v + M₂ · v) (λ v → M₁ ᵀ · v + M₂ ᵀ · v)

_*_ : ∀ {A : Set} {m n p} → LinearMap A m n
                          → LinearMap A n p
                          → LinearMap A m p
_*_ M₁ M₂ = LM (λ v → M₁ · M₂ · v) (λ v → M₂ ᵀ · M₁ ᵀ · v)

-- infix 7 _+_
infix 8 _*_
infixr 8 _·_
infixl 9 _ᵀ


-- Some matrix free operators. It may be hard to define the ones that need
-- real numbers.
idₗₘ : ∀ {A : Set} {n : ℕ} → LinearMap A n n
idₗₘ {A} {n} = LM id id
