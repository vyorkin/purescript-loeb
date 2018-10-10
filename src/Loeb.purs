module Loeb (loeb, moeb, fix) where

import Prelude

import Data.Lazy (Lazy, defer, force)

-- | Multi-Loeb.
moeb ∷ ∀ a b c. (((a → Lazy b) → Lazy b) → c → a) → c → a
moeb f x = let go = f (\g → defer \_ → force $ g go) x in go

-- | Loeb.
loeb ∷ ∀ f a. Functor f ⇒ f (f (Lazy a) → Lazy a) → f (Lazy a)
loeb = moeb map

-- | Fixed point of a function.
fix ∷ ∀ a. (Lazy a → Lazy a) → Lazy a
fix = moeb identity
