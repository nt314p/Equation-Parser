module SyntaxSpec where

-- file test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

spec :: Spec
spec = do
  describe "test"
    it "do them test" $ [] `shouldBe` []