module LinearMap where

open import Data.Nat using (ℕ)
open import Data.Vec using (Vec; foldr; zipWith; map) renaming ([] to []ⱽ; _∷_ to _∷ⱽ_)

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


-- Binary operators on vectors ------------------------------------------------

_+ⱽ_ : ⦃ F : Field A ⦄ → Vec A n → Vec A n → Vec A n
_+ⱽ_ []ⱽ []ⱽ = []ⱽ
_+ⱽ_ (x₁ ∷ⱽ xs₁) (x₂ ∷ⱽ xs₂) = x₁ + x₂ ∷ⱽ (xs₁ +ⱽ xs₂)
  where open Field {{...}}

-- Vector Hadamard product
_∘ⱽ_ : {A : Set} ⦃ F : Field A ⦄ → Vec A n → Vec A n → Vec A n
_∘ⱽ_ = zipWith _*_
  where open Field {{...}}

-- Multiply vector by a constant
_*ᶜ_ : {A : Set} ⦃ F : Field A ⦄ → A → Vec A n → Vec A n
c *ᶜ v = map (c *_) v
  where open Field {{...}}

infixl  7 _+ⱽ_
infixl  8 _∘ⱽ_
infixl 10 _*ᶜ_


sum : {A : Set} ⦃ F : Field A ⦄ → Vec A n → A
sum = foldr _ _+_ zero
  where open Field {{...}}

-- Inner product
⟨_,_⟩ : {A : Set} ⦃ F : Field A ⦄ → Vec A n → Vec A n → A
⟨ v₁ , v₂ ⟩ =  sum (v₁ ∘ⱽ v₂)


-------------------------------------------------------------------------------
--                             Proofs on Vectors                             --
-------------------------------------------------------------------------------

zipWith-comm : (f : A → A → A) → (f-comm : (a b : A) → f a b ≡ f b a)
             → (x y : Vec A n) → zipWith f x y ≡ zipWith f y x
zipWith-comm f f-comm []ⱽ []ⱽ = refl
zipWith-comm f f-comm (x ∷ⱽ xs) (y ∷ⱽ ys) rewrite
    zipWith-comm f f-comm xs ys
  | f-comm x y = refl

+ⱽ-comm : ⦃ F : Field A ⦄ → (v₁ v₂ : Vec A n) → v₁ +ⱽ v₂ ≡ v₂ +ⱽ v₁
+ⱽ-comm []ⱽ []ⱽ = refl
+ⱽ-comm (x₁ ∷ⱽ vs₁) (x₂ ∷ⱽ vs₂) = begin
  x₁ + x₂ ∷ⱽ vs₁ +ⱽ vs₂
  ≡⟨ cong ((x₁ + x₂) ∷ⱽ_) (+ⱽ-comm vs₁ vs₂) ⟩
  x₁ + x₂ ∷ⱽ vs₂ +ⱽ vs₁
  ≡⟨ cong (_∷ⱽ vs₂ +ⱽ vs₁) (+-comm x₁ x₂) ⟩
  x₂ + x₁ ∷ⱽ vs₂ +ⱽ vs₁
  ∎
  where open Field {{...}}

⟨⟩-comm : ⦃ F : Field A ⦄ → (v₁ v₂ : Vec A n)
        → ⟨ v₁ , v₂ ⟩ ≡ ⟨ v₂ , v₁ ⟩
⟨⟩-comm []ⱽ []ⱽ = refl
⟨⟩-comm {{F}} (x₁ ∷ⱽ v₁) (x₂ ∷ⱽ v₂) rewrite
    ⟨⟩-comm v₁ v₂
  | Field.*-comm F x₁ x₂
  = refl

-------------------------------------------------------------------------------
--                           LinearMap constructor                           --
-------------------------------------------------------------------------------

record LinearMap (A : Set) ⦃ F : Field A ⦄ (m n : ℕ) : Set where
  field
    f : (Vec A m → Vec A n)

    -- Additivity
    f[u+v]≡f[u]+f[v] : (u v : Vec A m) → f (u +ⱽ v) ≡ f u +ⱽ f v

    -- Homogeneity
    f[c*v]≡c*f[v] : (c : A) → (v : Vec A m) → f (c *ᶜ v) ≡ c *ᶜ (f v)


_∙ₗₘ_ : {A : Set} ⦃ F : Field A ⦄ → LinearMap A m n → Vec A m → Vec A n
_∙ₗₘ_ LM v = LinearMap.f LM v

-- Choose 20 since function application is assumed higher than almost anything
infixr 20 _∙ₗₘ_


-- Example LinearMap values ---------------------------------------------------

idₗₘ : ∀ {A n} ⦃ f : Field A ⦄ → LinearMap A n n
idₗₘ = record
  { f = id
  ; f[u+v]≡f[u]+f[v] = λ u v → refl
  ; f[c*v]≡c*f[v] = λ c v → refl
  }


-------------------------------------------------------------------------------
--                          M constructor and values                         --
-------------------------------------------------------------------------------

-- This data type is incomplete. We need to add the following
--
-- 1. Define the forward and adjoint functions to be linear maps, as in they
--    respect the following properties:
--
--    a. Additivity : f (u + v) ≡ f u + f v ∀ u, v ∈ V(F)
--    b. Homogeneity : ∀ c ∈ F, f (c * u) ≡ c * f u, u ∈ V(F)
--
-- 2. We want the transpose operation to actually be the transpose. This is to
--    say that we want the following equation to hold.
--
--    ⟨ x ᵀ, A * y ⟩ ≡ ⟨ yᵀ * , A ᵀ * x ⟩
--
--    This equation is just the transpose rule: (xᵀAy)ᵀ ≡ (yᵀAᵀx)
data M_∶_×_ (A : Set) (m n : ℕ) : Set where
  ⟦_,_⟧ : (Vec A n → Vec A m) -- Forward function
        → (Vec A m → Vec A n) -- Adjoint function
        → M A ∶ m × n

_ᵀ : ∀ {A : Set} → M A ∶ m × n → M A ∶ n × m
⟦ f , a ⟧ ᵀ = ⟦ a , f ⟧

_·_ : ∀ {A : Set} → M A ∶ m × n → Vec A n → Vec A m
⟦ f , a ⟧ · x = f x

_+_ : {A : Set} ⦃ F : Field A ⦄ → {m n : ℕ} →
      M A ∶ m × n → M A ∶ m × n → M A ∶ m × n
M₁ + M₂ = ⟦ (λ v → M₁ · v +ⱽ M₂ · v)
          , (λ v → M₁ ᵀ · v +ⱽ M₂ ᵀ · v) ⟧
  where open Field {{...}}

_*_ : M A ∶ m × n → M A ∶ n × p → M A ∶ m × p
M₁ * M₂ = ⟦ (λ v → M₁ · M₂ · v)
          , (λ v → M₂ ᵀ · M₁ ᵀ · v) ⟧


infix 7 _+_
infix 8 _*_
infixr 8 _·_
infixl 9 _ᵀ


-- Matrix Free Operators ------------------------------------------------------

I : M A ∶ n × n
I = ⟦ id , id ⟧


-------------------------------------------------------------------------------
--                            Proofs on LinearMaps                           --
-------------------------------------------------------------------------------

idᵀᵀ : (B : M A ∶ m × n) → B ᵀ ᵀ ≡ B
idᵀᵀ ⟦ _ , _ ⟧ = refl

ᵀ-distr-* : (L : M A ∶ m × n) (R : M A ∶ n × p)
          → (L * R) ᵀ ≡ (R ᵀ * L ᵀ)
ᵀ-distr-* L R rewrite idᵀᵀ L | idᵀᵀ R = refl

ᵀ-distr-+ : {A : Set} ⦃ F : Field A ⦄
          → (L : M A ∶ m × n) (R : M A ∶ m × n)
          → (L + R) ᵀ ≡ (L ᵀ + R ᵀ)
ᵀ-distr-+ L R rewrite idᵀᵀ L | idᵀᵀ R = refl

I-idempotent : {A : Set} {n : ℕ} → (I {A} {n}) * I ≡ I
I-idempotent = refl

-- This is the start of a redefinition for M; see above
id-transpose : ⦃ F : Field A ⦄ → (x y : Vec A n) → ⟨ x , id y ⟩ ≡ ⟨ y , id x ⟩
id-transpose {{F}} x y rewrite
    zipWith-comm (Field._*_ F) (Field.*-comm F) x y
  = refl
