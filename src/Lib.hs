{-# LANGUAGE FlexibleInstances #-}
-- | A library to do stuff.
module Lib
    (
      isValid
    , MyList (Empty, List)
    , prop_pos
    , PosList
    , ValidMyList (ValidMyList)
    ) where
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data MyList m a = Empty | List m a (MyList m a)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyList a b) where
  arbitrary = do
    x <- arbitrary
    m <- arbitrary
    aList <- arbitrary
    result <- elements [Empty, List m x aList]
    return $ result

-- Create QuickCheck modifier for valid MyList
newtype ValidMyList m a = ValidMyList (MyList m a)
  deriving ( Eq, Show )
--instance Functor (ValidMyList m) where
--  fmap f (ValidMyList l) = ValidMyList (fmap f l)
instance Arbitrary a => Arbitrary (ValidMyList Int a)
  where arbitrary = ValidMyList <$> arbitrary `suchThat` isValid

-- Function stub for determining if MyList is valid
isValid :: (MyList Int a) -> Bool
isValid Empty = True
isValid (List m _ _)
  | m > 0     = True
  | otherwise = False

-- Example QuickCheck property
newtype PosList a = PL [a] deriving Show
instance (Arbitrary a, Num a) => Arbitrary (PosList a) where
  arbitrary = fmap (PL . map abs) arbitrary

prop_pos (PL xs) = all (>= 0) xs

-- Try: verboseCheck prop_pos
-- sample (arbitrary :: Gen (PosList Int))
