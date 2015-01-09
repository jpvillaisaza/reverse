module Reverse.Data.List where

import Data.List

infixr 5 _∷_ _++_

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
