module Loeb (loeb, moeb) where

import Prelude

import Data.Lazy (Lazy, defer)

loeb ∷ ∀ f a. Functor f ⇒ f (f (Lazy a) → a) → f (Lazy a)
loeb x = go where go = (\y → defer (\_ → y go)) <$> x

moeb ∷ Unit
moeb = unit

-- moeb ∷ ∀ a b c. (((a → b) → b) → c → a) → c → a
-- moeb f x = go where go = f (($) moeb go) x
