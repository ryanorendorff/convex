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

  infixl 6 _+_
  infixl 7 _*_
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

-- Match the fixity of Haskell
infixl  6 _+ⱽ_
infixl  7 _∘ⱽ_
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

+ⱽ-assoc : ⦃ F : Field A ⦄ → (v₁ v₂ v₃ : Vec A n)
         → v₁ +ⱽ v₂ +ⱽ v₃ ≡ v₁ +ⱽ (v₂ +ⱽ v₃)
+ⱽ-assoc []ⱽ []ⱽ []ⱽ = refl
+ⱽ-assoc ⦃ F ⦄ (v₁ ∷ⱽ vs₁) (v₂ ∷ⱽ vs₂) (v₃ ∷ⱽ vs₃) rewrite
    +ⱽ-assoc vs₁ vs₂ vs₃
  | Field.+-assoc F v₁ v₂ v₃
    = refl

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

*ᶜ-distr-+ⱽ : {A : Set} ⦃ F : Field A ⦄
            → (c : A) (u v : Vec A n)
            → c *ᶜ (u +ⱽ v) ≡ c *ᶜ u +ⱽ c *ᶜ v
*ᶜ-distr-+ⱽ c []ⱽ []ⱽ = refl
*ᶜ-distr-+ⱽ ⦃ F ⦄ c (u ∷ⱽ us) (v ∷ⱽ vs) rewrite
    *ᶜ-distr-+ⱽ c us vs
  | Field.*-distr-+ F c u v
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


_·ˡᵐ_ : {A : Set} ⦃ F : Field A ⦄ → LinearMap A m n → Vec A m → Vec A n
_·ˡᵐ_ LM = LinearMap.f LM

-- Choose 20 since function application is assumed higher than almost anything
infixr 20 _·ˡᵐ_


_+ˡᵐ_ : {A : Set} ⦃ F : Field A ⦄
      → LinearMap A m n → LinearMap A m n → LinearMap A m n
g +ˡᵐ h = record
  { f = λ v → g ·ˡᵐ v +ⱽ h ·ˡᵐ v
  ; f[u+v]≡f[u]+f[v] = additivity g h
  ; f[c*v]≡c*f[v] = homogeneity g h
  }
  where open Field {{...}}
        additivity : {A : Set} ⦃ F : Field A ⦄
                   → (g h : LinearMap A m n)→ (u v : Vec A m)
                   → g ·ˡᵐ (u +ⱽ v) +ⱽ h ·ˡᵐ (u +ⱽ v) ≡
                      g ·ˡᵐ u +ⱽ h ·ˡᵐ u +ⱽ (g ·ˡᵐ v +ⱽ h ·ˡᵐ v)
        additivity g h u v rewrite
            LinearMap.f[u+v]≡f[u]+f[v] g u v
          | LinearMap.f[u+v]≡f[u]+f[v] h u v
          | (+ⱽ-assoc (g ·ˡᵐ u) (g ·ˡᵐ v) (h ·ˡᵐ u +ⱽ h ·ˡᵐ v))
          | sym (+ⱽ-assoc (g ·ˡᵐ v) (h ·ˡᵐ u) (h ·ˡᵐ v))
          | +ⱽ-comm (g ·ˡᵐ v) (h ·ˡᵐ u)
          | (+ⱽ-assoc (h ·ˡᵐ u) (g ·ˡᵐ v) (h ·ˡᵐ v))
          | sym (+ⱽ-assoc (g ·ˡᵐ u) (h ·ˡᵐ u) (g ·ˡᵐ v +ⱽ h ·ˡᵐ v))
          = refl

        homogeneity : {A : Set} ⦃ F : Field A ⦄
                    → (g h : LinearMap A m n) → (c : A) (v : Vec A m)
                    → g ·ˡᵐ (c *ᶜ v) +ⱽ h ·ˡᵐ (c *ᶜ v) ≡ c *ᶜ (g ·ˡᵐ v +ⱽ h ·ˡᵐ v)
        homogeneity g h c v rewrite
            LinearMap.f[c*v]≡c*f[v] g c v
          | LinearMap.f[c*v]≡c*f[v] h c v
          | sym (*ᶜ-distr-+ⱽ c (g ·ˡᵐ v) (h ·ˡᵐ v))
          = refl

_*ˡᵐ_ : {A : Set} ⦃ F : Field A ⦄
      → LinearMap A n p → LinearMap A m n → LinearMap A m p
g *ˡᵐ h = record
  { f = λ v → g ·ˡᵐ (h ·ˡᵐ v)
  ; f[u+v]≡f[u]+f[v] = additivity g h
  ; f[c*v]≡c*f[v] = homogeneity g h
  }
  where open Field {{...}}
        additivity : {A : Set} ⦃ F : Field A ⦄
                   → (g : LinearMap A n p)
                   → (h : LinearMap A m n)
                   → (u v : Vec A m)
                   → g ·ˡᵐ (h ·ˡᵐ (u +ⱽ v)) ≡ g ·ˡᵐ (h ·ˡᵐ u) +ⱽ g ·ˡᵐ (h ·ˡᵐ v)
        additivity g h u v rewrite
            LinearMap.f[u+v]≡f[u]+f[v] h u v
          | LinearMap.f[u+v]≡f[u]+f[v] g (LinearMap.f h u) (LinearMap.f h v)
          = refl

        homogeneity : {A : Set} ⦃ F : Field A ⦄
                    → (g : LinearMap A n p)
                    → (h : LinearMap A m n)
                    → (c : A) (v : Vec A m)
                    → g ·ˡᵐ (h ·ˡᵐ (c *ᶜ v)) ≡ c *ᶜ g ·ˡᵐ (h ·ˡᵐ v)
        homogeneity g h c v rewrite
            LinearMap.f[c*v]≡c*f[v] h c v
          | LinearMap.f[c*v]≡c*f[v] g c (h ·ˡᵐ v)
          = refl

infixl 6 _+ˡᵐ_
infixl 7 _*ˡᵐ_

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

data M_∶_×_ (A : Set) ⦃ F : Field A ⦄ (m n : ℕ) : Set where
  ⟦_,_,_⟧ : (M : LinearMap A n m )
          → (Mᵀ : LinearMap A m n )
          → (p : (x : Vec A m) → (y : Vec A n)
               → ⟨ x , M ·ˡᵐ y ⟩ ≡ ⟨ y , Mᵀ ·ˡᵐ x ⟩ )
          → M A ∶ m × n

extractLinearMap : {A : Set} ⦃ F : Field A ⦄ → M A ∶ m × n → LinearMap A n m
extractLinearMap ⟦ M , Mᵀ , p ⟧ = M

_ᵀ : {A : Set} ⦃ F : Field A ⦄ → M A ∶ m × n → M A ∶ n × m
⟦ f , a , p ⟧ ᵀ = ⟦ a , f , (λ x y → sym (p y x)) ⟧

_·_ : ∀ {A : Set} ⦃ F : Field A ⦄ → M A ∶ m × n → Vec A n → Vec A m
⟦ f , a , _ ⟧ · x = f ·ˡᵐ x

·-distr-+ⱽ : {A : Set} ⦃ F : Field A ⦄
           → (M : M A ∶ m × n) → (u v : Vec A n)
           → M · (u +ⱽ v) ≡ M · u +ⱽ M · v
·-distr-+ⱽ ⟦ M , _ , _ ⟧ u v = LinearMap.f[u+v]≡f[u]+f[v] M u v

·-comm-*ᶜ : {A : Set} ⦃ F : Field A ⦄
          → (M : M A ∶ m × n) → (c : A) (v : Vec A n)
          → M · (c *ᶜ v) ≡ c *ᶜ (M · v)
·-comm-*ᶜ ⟦ M , _ , _ ⟧ c v = LinearMap.f[c*v]≡c*f[v] M c v


_+_ : {A : Set} ⦃ F : Field A ⦄ → {m n : ℕ} →
      M A ∶ m × n → M A ∶ m × n → M A ∶ m × n
⟦ M₁ , M₁ᵀ , p₁ ⟧ + ⟦ M₂ , M₂ᵀ , p₂ ⟧ =
  ⟦ M₁ +ˡᵐ M₂
  , M₁ᵀ +ˡᵐ M₂ᵀ
  , {!!}
  ⟧
  where open Field {{...}}

-- _*_ : {A : Set} ⦃ F : Field A ⦄ → M A ∶ m × n → M A ∶ n × p → M A ∶ m × p
-- M₁ * M₂ = ⟦ record
--             { f = (λ v → M₁ · M₂ · v)
--             ; f[u+v]≡f[u]+f[v] = λ u v → {!!}
--             ; f[c*v]≡c*f[v] = {!!}
--             }
--           , {!!}
--           -- (λ v → M₂ ᵀ · M₁ ᵀ · v)
--           , {!!}
--           ⟧



infix 6 _+_
-- infix 7 _*_
infixr 20 _·_
infixl 25 _ᵀ


-- Matrix Free Operators ------------------------------------------------------

I : {A : Set} ⦃ F : Field A ⦄ → M A ∶ n × n
I = ⟦ idₗₘ , idₗₘ , id-transpose  ⟧
  where
    id-transpose : ⦃ F : Field A ⦄ → (x y : Vec A n)
                 → ⟨ x , id y ⟩ ≡ ⟨ y , id x ⟩
    id-transpose {{F}} x y rewrite
        zipWith-comm (Field._*_ F) (Field.*-comm F) x y
      = refl


-------------------------------------------------------------------------------
--                            Proofs on LinearMaps                           --
-------------------------------------------------------------------------------

idᵀᵀ : {A : Set} ⦃ F : Field A ⦄ → (B : M A ∶ m × n) → B ᵀ ᵀ ≡ B
idᵀᵀ ⟦ M , Mᵀ , p ⟧ = {!!}

-- ᵀ-distr-* : (L : M A ∶ m × n) (R : M A ∶ n × p)
--           → (L * R) ᵀ ≡ (R ᵀ * L ᵀ)
-- ᵀ-distr-* L R rewrite idᵀᵀ L | idᵀᵀ R = refl
--
-- ᵀ-distr-+ : {A : Set} ⦃ F : Field A ⦄
--           → (L : M A ∶ m × n) (R : M A ∶ m × n)
--           → (L + R) ᵀ ≡ (L ᵀ + R ᵀ)
-- ᵀ-distr-+ L R rewrite idᵀᵀ L | idᵀᵀ R = refl
--
-- I-idempotent : {A : Set} {n : ℕ} → (I {A} {n}) * I ≡ I
-- I-idempotent = refl
