module Test.Common where

import Test.Assert.Simple

assertAbout er e a = if e * (1 - er) < a && a < e * (1 + er) then return unit else assertFailure msg
  where
    msg = "expected: about " ++ show e ++ " but got: " ++ show a

