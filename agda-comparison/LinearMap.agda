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

record Field (A : Set) : Set where

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


_+ⱽ_ : {A : Set} → {f : Field A} → {n : ℕ} → Vec A n → Vec A n → Vec A n
_+ⱽ_ []ⱽ []ⱽ = []ⱽ
_+ⱽ_ {A} {f} (x₁ ∷ⱽ xs₁) (x₂ ∷ⱽ xs₂) = x₁ + x₂ ∷ⱽ ((_+ⱽ_ {A} {f}) xs₁ xs₂)
  where open Field f

infix 7 _+ⱽ_


-------------------------------------------------------------------------------
--                             Proofs on Vectors                             --
-------------------------------------------------------------------------------

+ⱽ-comm : {A : Set} → {f : Field A} → {n : ℕ} → (v₁ v₂ : Vec A n) →
          (_+ⱽ_ {A} {f}) v₁ v₂ ≡ (_+ⱽ_ {A} {f}) v₂ v₁
+ⱽ-comm []ⱽ []ⱽ = refl
+ⱽ-comm {A} {f} (x₁ ∷ⱽ vs₁) (x₂ ∷ⱽ vs₂) = begin
  x₁ + x₂ ∷ⱽ vs₁ +ⱽ vs₂
  ≡⟨ cong ((x₁ + x₂) ∷ⱽ_) (+ⱽ-comm {A} {f} vs₁ vs₂) ⟩
  x₁ + x₂ ∷ⱽ vs₂ +ⱽ vs₁
  ≡⟨ cong (_∷ⱽ (_+ⱽ_ {A} {f} vs₂ vs₁)) (+-comm x₁ x₂) ⟩
  x₂ + x₁ ∷ⱽ vs₂ +ⱽ vs₁
  ∎
  where open Field f


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


_+_ : {A : Set} → {f : Field A} → {m n : ℕ} →
      LinearMap A m n → LinearMap A m n → LinearMap A m n
_+_ {A} {f} M₁ M₂ = LM (λ v → M₁ · v +ᶠ M₂ · v) (λ v → M₁ ᵀ · v +ᶠ M₂ ᵀ · v)
  where _+ᶠ_ = _+ⱽ_ {A} {f}
        infix 6 _+ᶠ_

_*_ : LinearMap A m n → LinearMap A n p → LinearMap A m p
_*_ M₁ M₂ = LM (λ v → M₁ · M₂ · v) (λ v → M₂ ᵀ · M₁ ᵀ · v)

infix 7 _+_
infix 8 _*_
infixr 8 _·_
infixl 9 _ᵀ


-- Some matrix free operators. It may be hard to define the ones that need
-- real numbers.
idₗₘ : ∀ {A : Set} → LinearMap A n n
idₗₘ = LM id id
