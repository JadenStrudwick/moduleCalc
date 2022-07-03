module Arbitrary where

import Test.QuickCheck ( Arbitrary(arbitrary) )
import Lib

instance Arbitrary Coursework where
  arbitrary = do
    name <- arbitrary
    weight <- arbitrary
    Coursework name weight <$> arbitrary

instance Arbitrary a => Arbitrary (Node a) where
    arbitrary = do
        name <- arbitrary
        weight <- arbitrary
        Node name weight <$> arbitrary

