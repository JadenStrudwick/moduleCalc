import Test.QuickCheck
import Lib 

instance Arbitrary Coursework where
    arbitrary = do
        name <- arbitrary
        weight <- arbitrary
        mark <- arbitrary
        return $ makeCoursework name weight mark

instance Arbitrary Module where
    arbitrary = do
        name <- arbitrary
        cws <- arbitrary
        weight <- arbitrary
        return $ makeModule name cws weight

instance Arbitrary Year where
    arbitrary = do
        modules <- arbitrary
        weight <- arbitrary
        return $ makeYear modules weight

main :: IO ()
main = do
    quickCheck prop_makeModule
    quickCheck prop_makeYear
    putStrLn "Done"

prop_makeModule :: String -> [Coursework] -> Float -> Bool
prop_makeModule name cws weight = mMark m == sum [ (cwWeight cw / 100) * cwMark cw | cw <- cws]
    where
        m = makeModule name cws weight

prop_makeYear :: [Module] -> Float -> Bool
prop_makeYear ms weight = yMark y == sum [ (mWeight m / 100) * mMark m | m <- ms]
    where
        y = makeYear ms weight