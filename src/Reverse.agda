--------------------------------------------------------------------------------
--
--
--
--------------------------------------------------------------------------------

module Reverse where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; sym)

infixr 5 _∷_ _++_

--------------------------------------------------------------------------------
--

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

[_] : ∀ {A} → A → List A
[ x ] = x ∷ []

_++_ : ∀ {A} → List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys

reverse : ∀ {A} → List A → List A
reverse []       = []
reverse (x ∷ xs) = reverse xs ++ [ x ]

--------------------------------------------------------------------------------
-- monoid

left-id : ∀ {A} (xs : List A) → [] ++ xs ≡ xs
left-id _ = refl

right-id : ∀ {A} (xs : List A) → xs ++ [] ≡ xs
right-id []       = refl
right-id (x ∷ xs) = cong (_∷_ x) (right-id xs)

assoc : ∀ {A} (xs ys zs : List A) → xs ++ ys ++ zs ≡ (xs ++ ys) ++ zs
assoc []       ys zs = refl
assoc (x ∷ xs) ys zs = cong (_∷_ x) (assoc xs ys zs)

--------------------------------------------------------------------------------
--

revcat : ∀ {A} → List A → List A → List A
revcat xs ys = reverse xs ++ ys

base : ∀ {A} (ys : List A) → revcat [] ys ≡ ys
base ys =
  begin
    revcat [] ys
      ≡⟨ refl ⟩
    reverse [] ++ ys
      ≡⟨ refl ⟩
    [] ++ ys
      ≡⟨ refl ⟩
    ys
  ∎
  where open Relation.Binary.PropositionalEquality.≡-Reasoning

induc : ∀ {A} (x : A) (xs ys : List A) → revcat (x ∷ xs) ys ≡ revcat xs (x ∷ ys)
induc x xs ys =
  begin
    revcat (x ∷ xs) ys
      ≡⟨ refl ⟩
    reverse (x ∷ xs) ++ ys
      ≡⟨ refl ⟩
    (reverse xs ++ [ x ]) ++ ys
      ≡⟨ sym (assoc (reverse xs) [ x ] ys) ⟩
    reverse xs ++ [ x ] ++ ys
      ≡⟨ refl ⟩
    reverse xs ++ (x ∷ ys)
      ≡⟨ refl ⟩
    revcat xs (x ∷ ys)
  ∎
  where open Relation.Binary.PropositionalEquality.≡-Reasoning

--------------------------------------------------------------------------------
--

reverse' : ∀ {A} → List A → List A
reverse' xs = reverse'' xs []
  where
    reverse'' : ∀ {A} → List A → List A → List A
    reverse'' []       ys = ys
    reverse'' (x ∷ xs) ys = reverse'' xs (x ∷ ys)
