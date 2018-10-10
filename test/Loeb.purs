module Test.Loeb where

import Prelude

import Data.Array (length)
import Data.Lazy (Lazy, force)
import Effect (Effect)
import Loeb (loeb)
import Test.Assert (assertEqual)
import Test.Utils (at, constl, lazy)

fs ∷ Array (Array (Lazy Int) → Lazy Int)
fs = [ \xs → lazy $ force (at 1 xs) + 1
     , \xs → lazy $ force (at 3 xs) + 1
     , \xs → lazy $ force (at 0 xs) + 1
     , constl 1
     , \xs → at 0 xs
     , \xs → lazy $ length xs
     ]

main ∷ Effect Unit
main = do
  assertEqual { actual: force <$> loeb fs
              , expected: [3, 2, 4, 1, 3, 6]
              }
