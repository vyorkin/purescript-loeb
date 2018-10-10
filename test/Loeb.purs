module Test.Loeb where

import Prelude

import Data.Array (foldl, (!!), (..))
import Data.Lazy (Lazy, force)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Loeb (loeb)
import Test.Assert (assertEqual)

arr :: Array (Array (Lazy Int) → Int)
arr =
  [ const 8
  , const 1
  , const 5
  , add1 0
  , add1 3
  , sum' (0..4)
  , sum' (0..5)
  ]
  where
    add1 n x = fromMaybe 0 (add 1 <<< force <$> x !! n)
    sum' ixs xs = foldl (\acc ix -> fromMaybe 0 ((+) acc <<< force <$> xs !! ix)) 0 ixs

main ∷ Effect Unit
main = do
  assertEqual { actual: force <$> loeb arr
              , expected: [8, 1, 5, 9, 10, 33, 66]
              }
