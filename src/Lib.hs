module Lib where

-- | Coursework data type
data Coursework = Coursework {
    cwName      :: String,
    cwWeight    :: Float,
    cwMark      :: Float
}

instance Show Coursework where
    show c = " - " ++ cwName c ++ " (" ++ show (cwWeight c) ++ "% weight): " ++ show (cwMark c) ++ "%"

makeCoursework :: String -> Float -> Float -> Coursework
makeCoursework = Coursework

-- | Module data type
data Module = Module {
    moduleName      :: String,
    courseworks     :: [Coursework],
    moduleWeight    :: Float,
    moduleMark      :: Float
}

instance Show Module where
    show m = "Module name: " ++ moduleName m ++ " (" ++ show (moduleWeight m) ++ "% weight): " ++ " \n" ++ unlines (map show (courseworks m) )++ " Module percent: " ++ show (moduleMark m) ++ "% \n"

makeModule :: String -> [Coursework] -> Float -> Module
makeModule name cws weight = Module name cws weight (sum [ (cwWeight cw / 100) * cwMark cw | cw <- cws])

-- | Year data type
data Year = Year {
    modules     :: [Module],
    yearWeight  :: Float,
    yearMark    :: Float
}

instance Show Year where
    show y = unlines (map show (modules y)) ++ "Year percent: (" ++ show (yearWeight y) ++ "% weight): " ++ show (yearMark y) ++ "% \n"

makeYear :: [Module] -> Float -> Year
makeYear ms weight = Year ms weight (sum [moduleMark m | m <- ms] / fromIntegral (length ms))