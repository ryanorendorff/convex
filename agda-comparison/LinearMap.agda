module LinearMap where

open import Level using (Level)
open import Data.Nat using (ℕ; suc; zero)

open import Data.Empty

open import Data.Vec using (Vec; foldr; zipWith; map) renaming ([] to []ⱽ; _∷_ to _∷ⱽ_)

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

open import Function using (id)

open import VectorList using (VectorList; splitToVectorList; []ⱽᴸ; _∷ⱽᴸ_)

private
  variable
    m n p q : ℕ
    A : Set
    a b c : A
    ℓ : Level


-- TODO: Can this be replaced with something like the List⁺ definition so that
-- the proofs from Vec can be transferred? This definition is convenient because
-- the size is correct (it is not n - 1).
data Vec⁺ (A : Set ℓ) : ℕ → Set ℓ where
  [_] : A → Vec⁺ A 1
  _∷_ : ∀ {n} (x : A) (xs : Vec⁺ A n) → Vec⁺ A (suc n)

-- Want to prove that is it not possible to construct an empty vector
emptyVecImpossible : {A : Set ℓ} → Vec⁺ A 0 → ⊥
emptyVecImpossible = λ ()

Vec⁺→Vec : {A : Set ℓ} → Vec⁺ A n → Vec A n
Vec⁺→Vec [ v ] = v ∷ⱽ []ⱽ
Vec⁺→Vec (v ∷ vs⁺) = v ∷ⱽ Vec⁺→Vec vs⁺

Vec⁺→n≢0 : {A : Set ℓ} → Vec⁺ A n → n ≢ 0
Vec⁺→n≢0 {A} {suc n} v = suc≢0
  where
    suc≢0 : {n : ℕ} → suc n ≢ 0
    suc≢0 {zero} ()
    suc≢0 {suc n} = λ ()

-- Vec→Vec⁺ : ∀ {ℓ} → {A : Set ℓ} {n : ℕ} → (n ≢ 0) → Vec A n → Vec⁺ A n
-- Vec→Vec⁺ {ℓ} {A} {0} p []ⱽ = {!p !}
-- Vec→Vec⁺ {ℓ} {A} {suc n} p (x ∷ⱽ x₁) = {!!}

record Field {ℓ} (A : Set ℓ) : Set ℓ where

  infixl 7 _+_
  infixl 8 _*_
  infixl 9 -_
  infixl 10 _⁻¹

  field
    _+_ : A → A → A
    _*_ : A → A → A

    0ᶠ : A
    1ᶠ  : A
    -_   : A → A -- + inverse
    _⁻¹  : A → A -- * inverse

    +-assoc   : (a b c : A) → a + (b + c) ≡ (a + b) + c
    +-comm    : (a b : A)   → a + b ≡ b + a
    +-0       : (a : A)     → a + 0ᶠ ≡ a
    +-inv     : (a : A)     → - a + a ≡ 0ᶠ
    *-assoc   : (a b c : A) → a * (b * c) ≡ (a * b) * c
    *-comm    : (a b : A)   → a * b ≡ b * a
    *-1       : (a : A)     → a * 1ᶠ ≡ a
    *-inv     : (a : A)     → a ⁻¹ * a ≡ 1ᶠ -- Missing a ≠ 0 in clause
    *-distr-+ : (a b c : A) → a * (b + c) ≡ a * b + a * c


{-
If we want the reals, we need a few things

Linearly ordered set on F, ∀ x, y, z ∈ F
  Totality : x ≤ y or y ≤ x
  Transitivity : if x ≤ y and y ≤ z then x ≤ z
  Anti-symmetry : if x ≤ y and y ≤ x, then x ≡ y

Linearly ordered field (F, +, *, ≤) if
  x ≤ y then z + x ≤ z + y
  0 ≤ x and 0 ≤ y then 0 ≤ x*y

Complete ordered field (F, +, *, ≤) if
  (F, +, *, ≤) is a linearly ordered field
  Completeness : every non-empty subset of F, bounded above, has a supremum in F

Archimedian property:
  (F, +, *, ≤) is a complete ordered field
  r, s ∈ F. r > 0 ∃ n ∈ ℕ. s < r + ... + r (n times)

  Roughly speaking, it is the property of having no infinitely large or
  infinitely small elements

  Axiom of Archimedies is a formulation of this property for ordered fields:
    ∀ ε ∈ F. ε > 0 → ∃ n ∈ ℕ . 1/n < ε

Cauchy sequences representation of the reals and the Dedekind representation
of the reals are both Archimedian completely ordered fields and hence
isomorphic to the reals. Note there is a proof that all Archimedian complete
ordered fields are isomorphic.

References:
[1]: https://www.cs.swan.ac.uk/~csetzer/articlesFromOthers/chiMing/chiMingChuangExtractionOfProgramsForExactRealNumberComputation.pdf
[2]: https://en.wikipedia.org/wiki/Axiomatic_theory_of_real_numbers
-}


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
sum = foldr _ _+_ 0ᶠ
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

∘ⱽ-distr-+ⱽ : ∀ {A n} ⦃ F : Field A ⦄ → (a u v : Vec A n)
            → a ∘ⱽ (u +ⱽ v) ≡ a ∘ⱽ u +ⱽ a ∘ⱽ v
∘ⱽ-distr-+ⱽ []ⱽ []ⱽ []ⱽ = refl
∘ⱽ-distr-+ⱽ ⦃ F ⦄ (a ∷ⱽ as) (u ∷ⱽ us) (v ∷ⱽ vs) rewrite
    ∘ⱽ-distr-+ⱽ as us vs
  | Field.*-distr-+ F a u v
  = refl

-- Homogeneity of degree 1 for linear maps
∘ⱽ*ᶜ≡*ᶜ∘ⱽ : ∀ {A n} ⦃ F : Field A ⦄ → (c : A) (u v : Vec A n)
          → u ∘ⱽ c *ᶜ v ≡ c *ᶜ (u ∘ⱽ v)
∘ⱽ*ᶜ≡*ᶜ∘ⱽ c []ⱽ []ⱽ = refl
∘ⱽ*ᶜ≡*ᶜ∘ⱽ ⦃ F ⦄ c (u ∷ⱽ us) (v ∷ⱽ vs) rewrite
    ∘ⱽ*ᶜ≡*ᶜ∘ⱽ c us vs
  | Field.*-assoc F u c v
  | Field.*-comm F u c
  | sym (Field.*-assoc F c u v)
  = refl

⟨⟩-comm : ⦃ F : Field A ⦄ → (v₁ v₂ : Vec A n)
        → ⟨ v₁ , v₂ ⟩ ≡ ⟨ v₂ , v₁ ⟩
⟨⟩-comm []ⱽ []ⱽ = refl
⟨⟩-comm ⦃ F ⦄ (x₁ ∷ⱽ v₁) (x₂ ∷ⱽ v₂) rewrite
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

diagₗₘ : ∀ {A n} ⦃ f : Field A ⦄ → Vec A n → LinearMap A n n
diagₗₘ d = record
  { f = d ∘ⱽ_
  ; f[u+v]≡f[u]+f[v] = ∘ⱽ-distr-+ⱽ d
  ; f[c*v]≡c*f[v] = λ c v → ∘ⱽ*ᶜ≡*ᶜ∘ⱽ c d v
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
infixr 20 _·_
infixl 25 _ᵀ


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
