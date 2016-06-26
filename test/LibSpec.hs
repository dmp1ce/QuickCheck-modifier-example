module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib  (
              isValid
            , MyList (Empty, List)
            )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    prop "MyList isValid" $ \x ->
      isValid (x :: MyList Int String) `shouldBe` True
--    it "works" $ do
--      True `shouldBe` True
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
