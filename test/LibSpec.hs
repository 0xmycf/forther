module LibSpec (spec) where

import Lib (helloString)

import Test.Hspec

spec :: Spec
spec = describe "helloString" $ do
  it "returns 'Hello, Haskell!'" $ do
    helloString `shouldBe` "Hello, Haskell!"
