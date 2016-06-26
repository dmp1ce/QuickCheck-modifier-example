-- | A library to do stuff.
module Lib
    (
      isValid
    , MyList (Empty, List)
    ) where
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
instance Functor (ValidMyList a) where
  fmap f (ValidMyList l) = ValidMyList (f l) 
--instance (Arbitrary a, Arbitrary m) => Arbitrary (ValidMyList Int a)
--  where arbitrary = fmap ValidMyList $ arbitrary `suchThat` isValidMyList

-- Function stub for determining if MyList is valid
isValid :: MyList m a -> Bool
isValid _ = True
