module Test.Main where

import Test.PSpec.Mocha

import qualified Test.Stream  as Stream
import qualified Test.Modify  as Modify
import qualified Test.Combine as Combine

main = runMocha $ do
  Stream.test
  Modify.test
  Combine.test
