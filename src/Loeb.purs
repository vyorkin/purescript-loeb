module Loeb (loeb) where

import Prelude

import Data.Lazy (Lazy, defer)

loeb ∷ ∀ f a. Functor f ⇒ f (f (Lazy a) → a) → f (Lazy a)
loeb x = go where go = (\y → defer (\_ → y go)) <$> x
