module Lib (Coursework, makeCoursework, Module, makeModule, Year, makeYear) where

-- | Coursework data type
data Coursework = Coursework {
    cwName      :: String,
    cwWeight    :: Float,
    cwMark      :: Float
}

instance Show Coursework where
    show (Coursework name weight mark) = " - " ++ name ++ " (" ++ show weight ++ "% weight): " ++ show mark ++ "%"

makeCoursework :: String -> Float -> Float -> Coursework
makeCoursework name weight mark = Coursework name weight mark

testCoursework :: Coursework
testCoursework = makeCoursework "Test Coursework" 50 70

-- | Module data type
data Module = Module {
    moduleName      :: String,
    courseworks     :: [Coursework],
    moduleWeight    :: Float,
    moduleMark      :: Float
}

instance Show Module where
    show (Module name courseworks weight mark) = "Module name: " ++ name ++ " (" ++ show weight ++ "% weight): " ++ " \n" ++ unlines (map show courseworks) ++ " Module percent: " ++ show mark ++ "% \n"

makeModule :: String -> [Coursework] -> Float -> Module
makeModule name courseworks weight = Module name courseworks weight (sum [ (cwWeight cw / 100) * cwMark cw | cw <- courseworks])

testModule :: Module
testModule = makeModule "Test Module" [testCoursework, testCoursework] 0.25

-- | Year data type
data Year = Year {
    modules     :: [Module],
    yearWeight  :: Float,
    yearMark    :: Float
}

instance Show Year where
    show (Year modules weight mark) = unlines (map show modules) ++ "Year percent: (" ++ show weight ++ "% weight): " ++ show mark ++ "% \n"

makeYear :: [Module] -> Float -> Year
makeYear modules weight = Year modules weight (sum [moduleMark m | m <- modules] / (fromIntegral (length modules)))

testYear :: Year
testYear = makeYear [testModule, testModule, testModule, testModule] 0.10
