module Test.Main where

import Prelude

import Effect (Effect)
import Test.Loeb as Test.Loeb
import Test.Moeb as Test.Moeb
import Test.Spreadsheet as Test.Spreadsheet

main ∷ Effect Unit
main = do
  Test.Loeb.main
  Test.Moeb.main
  Test.Spreadsheet.main
