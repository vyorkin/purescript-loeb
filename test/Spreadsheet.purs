module Test.Spreadsheet where

import Prelude

import Data.Array (foldl, unsafeIndex, (..))
import Data.Lazy (Lazy, force)
import Effect (Effect)
import Loeb (loeb)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assertEqual)

vat ∷ Int → Array (Lazy Number) → Number
vat ix xs = at ix xs / 10.0

sum' ∷ Array Int → Array (Lazy Number) → Number
sum' ixs xs = foldl (\acc ix → acc + at ix xs) 0.0 ixs

spreadsheet ∷ Array (Array (Lazy Number) → Number)
spreadsheet =
--  Prices   | VAT      | Effective prices + total
  [ const 1.0, vat 0,     sum' (0..1),         const 0.0, const 0.0
  , const 3.0, vat 5,     sum' (5..6),         const 0.0, const 0.0
  , const 5.0, vat 10,    sum' (10..11),       const 0.0, const 0.0
  , const 2.0, vat 15,    sum' (15..16),       const 0.0, const 0.0
  , const 0.0, const 0.0, sum' [2, 7, 12, 17], const 0.0, const 0.0
  ]

expected ∷ Array Number
expected =
  [ 1.0,  0.1,  1.1,  0.0,  0.0
  , 3.0,  0.3,  3.3,  0.0,  0.0
  , 5.0,  0.5,  5.5,  0.0,  0.0
  , 2.0,  0.2,  2.2,  0.0,  0.0
  , 0.0,  0.0,  12.100000000000001,  0.0,  0.0
  ]

main ∷ Effect Unit
main = do
  assertEqual { actual: force <$> loeb spreadsheet
              , expected
              }

infixl 8 unsafeIndex as !!?

at ∷ ∀ a. Int → Array (Lazy a) → a
at ix xs = force $ unsafePartial (xs !!? ix)

add1 ∷ Int → Array (Lazy Int) → Int
add1 ix xs = at ix xs + 1
