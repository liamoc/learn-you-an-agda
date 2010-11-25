module Printf where

open import Data.List hiding(_++_)
open import Data.String
open import Data.Unit
open import Data.Empty
open import Data.Char 
open import Data.Product
open import Data.Nat.Show renaming (show to showNat)
open import Data.Nat
open import Function using (_∘_)

data ValidFormat : Set₁ where
   argument : (A : Set) → (A → String) → ValidFormat
   literal  : Char      → ValidFormat

data Format : Set₁ where
   valid   : List ValidFormat → Format
   invalid : Format

parse : String → Format
parse s = parse′ [] (toList s)
  where 
    parse′ : List ValidFormat → List Char → Format
    parse′ l ('%' ∷ 's' ∷ fmt) = parse′ (argument String (λ x → x) ∷ l) fmt
    parse′ l ('%' ∷ 'c' ∷ fmt) = parse′ (argument Char (λ x → fromList [ x ]) ∷ l) fmt
    parse′ l ('%' ∷ 'd' ∷ fmt) = parse′ (argument ℕ showNat ∷ l) fmt
    parse′ l ('%' ∷ '%' ∷ fmt) = parse′ (literal '%' ∷ l) fmt
    parse′ l ('%' ∷ c   ∷ fmt) = invalid
    parse′ l (c         ∷ fmt) = parse′ (literal c ∷ l) fmt
    parse′ l []                = valid (reverse l)

Args : Format → Set
Args invalid                    = ⊥ → String 
Args (valid (argument t _ ∷ r)) = t → (Args (valid r))
Args (valid (literal _ ∷ r))    = Args (valid r)
Args (valid [])                 = String 

FormatArgs : String → Set
FormatArgs f = Args (parse f)

sprintf : (f : String) → FormatArgs f
sprintf = sprintf′ "" ∘ parse
  where
    sprintf′ : String → (f : Format) → Args f
    sprintf′ _     invalid                    = λ b → "" --who cares, impossible
    sprintf′ accum (valid [])                 = accum
    sprintf′ accum (valid (argument _ s ∷ l)) = λ t →  (sprintf′ (accum ++ s t) (valid l))
    sprintf′ accum (valid (literal c ∷ l))    = sprintf′ (accum ++ fromList [ c ]) (valid l)


