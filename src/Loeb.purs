module Loeb (loeb, moeb, noeb, toeb, foeb, fix) where

import Prelude

import Data.Array (foldMap)
import Data.Foldable (class Foldable)
import Data.Lazy (Lazy, defer, force)
import Data.Traversable (class Traversable, traverse)

-- | Multi-Loeb.
moeb ∷ ∀ a b c. (((a → Lazy b) → Lazy b) → c → a) → c → a
moeb f x = let go = f (\g → defer \_ → force $ g go) x in go

-- | Loeb.
loeb ∷ ∀ f a. Functor f ⇒ f (f (Lazy a) → Lazy a) → f (Lazy a)
loeb = moeb map

-- | Fixed point of a function.
fix ∷ ∀ a. (Lazy a → Lazy a) → Lazy a
fix = moeb identity

noeb ∷ ∀ a. Lazy (Lazy a → Lazy a) → Lazy a
noeb = moeb (=<<)

toeb ∷ ∀ f a. Traversable f ⇒ f (Lazy (f a) → Lazy a) → Lazy (f a)
toeb = moeb traverse

foeb ∷ ∀ m f. Foldable f ⇒ Monoid m ⇒ f (Lazy m → Lazy m) → Lazy m
foeb = moeb foldMap
