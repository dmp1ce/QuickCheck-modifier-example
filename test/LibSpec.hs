module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib  (
              isValid
            , MyList (Empty, List)
            , ValidMyList (ValidMyList)
            )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    prop "Random MyList == itself" $ \x ->
      (x :: MyList Int String) `shouldBe` x
    prop "MyList isValid" $ \(ValidMyList x) ->
      isValid (x :: MyList Int String) == True

    --prop "MyList isValid" $ \ValidMyList x ->
    --  isValid x `shouldBe` True
--    it "works" $ do
--      True `shouldBe` True
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
