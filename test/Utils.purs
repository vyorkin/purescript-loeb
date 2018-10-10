module Test.Utils (lazy, at, addAt, sum', constl) where

import Prelude

import Data.Array (foldl, unsafeIndex)
import Data.Lazy (Lazy, defer, force)
import Partial.Unsafe (unsafePartial)

infixl 8 unsafeIndex as !!?

at ∷ ∀ a. Int → Array (Lazy a) → Lazy a
at ix xs = unsafePartial (xs !!? ix)

addAt ∷ ∀ a. Semiring a ⇒ a → Int → Array (Lazy a) → Lazy a
addAt x ix xs = lazy $ force (at ix xs) + x

sum' ∷ ∀ a. Semiring a ⇒ Array Int → Array (Lazy a) → Lazy a
sum' ixs xs = foldl (\acc ix → acc + at ix xs) zero ixs

lazy ∷ ∀ a. a → Lazy a
lazy x = defer \_ → x

constl ∷ ∀ a b. a → b → Lazy a
constl = const <<< lazy
