module Test.Spreadsheet where

import Prelude

import Data.Array ((..))
import Data.Lazy (Lazy, force)
import Effect (Effect)
import Loeb (loeb)
import Math (round)
import Test.Assert (assertEqual)
import Test.Utils (addAt, at, constl, lazy, sum')

vat ∷ Int → Array (Lazy Number) → Lazy Number
vat ix xs = lazy $ round $ force (at ix xs) / 10.0

zero' ∷ ∀ a. a → Lazy Number
zero' = constl 0.0

spreadsheet ∷ Array (Array (Lazy Number) → Lazy Number)
spreadsheet =
  [ constl 1.0, addAt 1.0 0,  sum' (0..1),         zero', zero'
  , constl 3.0, addAt 2.0 5,  sum' (5..6),         zero', zero'
  , constl 5.0, addAt 2.0 10, sum' (10..11),       zero', zero'
  , constl 2.0, addAt 3.0 15, sum' (15..16),       zero', zero'
  , zero',      zero',        sum' [2, 7, 12, 17], zero', zero'
  ]

expected ∷ Array Number
expected =
  [ 1.0,  2.0,  3.0,  0.0,  0.0
  , 3.0,  5.0,  8.0,  0.0,  0.0
  , 5.0,  7.0,  12.0, 0.0,  0.0
  , 2.0,  5.0,  7.0,  0.0,  0.0
  , 0.0,  0.0,  30.0, 0.0,  0.0
  ]

main ∷ Effect Unit
main = do
  assertEqual { actual: force <$> loeb spreadsheet
              , expected
              }
