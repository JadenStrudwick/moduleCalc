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
    mName      :: String,
    mCws       :: [Coursework],
    mWeight    :: Float,
    mMark      :: Float
}

instance Show Module where
    show m = "Module name: " ++ mName m ++ " (" ++ show (mWeight m) ++ "% weight): " ++ " \n" ++ unlines (map show (mCws m) )++ " Module percent: " ++ show (mMark m) ++ "% \n"

-- | Smart constructor for Module data type
-- Empty coursework list creates a module with 0 weight and 0 mark
makeModule :: String -> [Coursework] -> Float -> Module
makeModule name [] weight = Module name [] 0 0
makeModule name cws weight = Module name cws weight (sum [ (cwWeight cw / 100) * cwMark cw | cw <- cws])

-- | Year data type
data Year = Year {
    yMs         :: [Module],
    yWeight     :: Float,
    yMark       :: Float
}

instance Show Year where
    show y = unlines (map show (yMs y)) ++ "Year percent (" ++ show (yWeight y) ++ "% weight): " ++ show (yMark y) ++ "% \n"

-- | Smart constructor for Year data type
-- Empty module list creates a year with 0 weight and 0 mark
makeYear :: [Module] -> Float -> Year
makeYear [] weight = Year [] 0 0
makeYear ms weight = Year ms weight (sum [ mMark m | m <- ms] / fromIntegral (length ms))