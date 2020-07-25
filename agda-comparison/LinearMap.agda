module LinearMap where

open import Data.List using (List; sum) renaming ([] to []ᴸ; _∷_ to _∷ᴸ_)
open import Data.Nat using (ℕ)
open import Data.Vec using (Vec; _++_) renaming ([] to []ⱽ; _∷_ to _∷ⱽ_)
open import Data.Product as Prod using (∃; ∃₂; _×_; _,_)

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning


open import Function using (id)

open import VectorList using (VectorList; splitToVectorList; []ⱽᴸ; _∷ⱽᴸ_)

variable
  m n p q : ℕ
  A : Set
  a b c : A

record Field {ℓ} (A : Set ℓ) : Set ℓ where

  infixl 7 _+_
  infixl 8 _*_
  infixl 9 -_
  infixl 10 _⁻¹

  field
    _+_ : A → A → A
    _*_ : A → A → A

    zero : A
    one  : A
    -_   : A → A -- + inverse
    _⁻¹  : A → A -- * inverse

    +-assoc   : (a b c : A) → a + (b + c) ≡ (a + b) + c
    +-comm    : (a b : A)   → a + b ≡ b + a
    +-0       : (a : A)     → a + zero ≡ a
    +-inv     : (a : A)     → - a + a ≡ zero
    *-assoc   : (a b c : A) → a * (b * c) ≡ (a * b) * c
    *-comm    : (a b : A)   → a * b ≡ b * a
    *-1       : (a : A)     → a * one ≡ a
    *-inv     : (a : A)     → a ⁻¹ * a ≡ one
    *-distr-+ : (a b c : A) → a * (b + c) ≡ a * b + a * c


_+ⱽ_ : ⦃ f : Field A ⦄ → Vec A n → Vec A n → Vec A n
_+ⱽ_ []ⱽ []ⱽ = []ⱽ
_+ⱽ_ (x₁ ∷ⱽ xs₁) (x₂ ∷ⱽ xs₂) = x₁ + x₂ ∷ⱽ (xs₁ +ⱽ xs₂)
  where open Field {{...}}

infix 7 _+ⱽ_


-------------------------------------------------------------------------------
--                             Proofs on Vectors                             --
-------------------------------------------------------------------------------

+ⱽ-comm : ⦃ f : Field A ⦄ → (v₁ v₂ : Vec A n) → v₁ +ⱽ v₂ ≡ v₂ +ⱽ v₁
+ⱽ-comm []ⱽ []ⱽ = refl
+ⱽ-comm (x₁ ∷ⱽ vs₁) (x₂ ∷ⱽ vs₂) = begin
  x₁ + x₂ ∷ⱽ vs₁ +ⱽ vs₂
  ≡⟨ cong ((x₁ + x₂) ∷ⱽ_) (+ⱽ-comm vs₁ vs₂) ⟩
  x₁ + x₂ ∷ⱽ vs₂ +ⱽ vs₁
  ≡⟨ cong (_∷ⱽ vs₂ +ⱽ vs₁) (+-comm x₁ x₂) ⟩
  x₂ + x₁ ∷ⱽ vs₂ +ⱽ vs₁
  ∎
  where open Field {{...}}


-------------------------------------------------------------------------------
--                     LinearMap constructors and values                     --
-------------------------------------------------------------------------------

data LinearMap (A : Set) (m n : ℕ) : Set where
    LM : (Vec A n → Vec A m) -- Forward function
      → (Vec A m → Vec A n) -- Adjoint function
      → LinearMap A m n

_ᵀ : ∀ {A : Set} → LinearMap A m n → LinearMap A n m
LM f a ᵀ = LM a f

_·_ : ∀ {A : Set} → LinearMap A m n → Vec A n → Vec A m
LM f a · x = f x

_+_ : {A : Set} ⦃ f : Field A ⦄ → {m n : ℕ} →
      LinearMap A m n → LinearMap A m n → LinearMap A m n
M₁ + M₂ = LM (λ v → M₁ · v +ⱽ M₂ · v) (λ v → M₁ ᵀ · v +ⱽ M₂ ᵀ · v)
  where open Field {{...}}

_*_ : LinearMap A m n → LinearMap A n p → LinearMap A m p
M₁ * M₂ = LM (λ v → M₁ · M₂ · v) (λ v → M₂ ᵀ · M₁ ᵀ · v)


infix 7 _+_
infix 8 _*_
infixr 8 _·_
infixl 9 _ᵀ


-- Some matrix free operators. It may be hard to define the ones that need
-- real numbers.
idₗₘ : ∀ {A : Set} → LinearMap A n n
idₗₘ = LM id id
