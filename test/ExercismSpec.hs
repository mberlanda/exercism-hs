module ExercismSpec(spec) where

import Test.Hspec
import Exercism (someFunc)

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "returns 'someFunc'" $ do
      someFunc `shouldBe` "someFunc"
