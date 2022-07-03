module Lib where

import Data.Maybe

-- | Coursework data type
data Coursework = Coursework {
    cwName      :: Maybe String,
    cwWeight    :: Maybe Float,
    cwMark      :: Maybe Float
}

makeCoursework :: String -> Float -> Float -> Coursework
makeCoursework name weight mark = Coursework {
    cwName      = Just name,
    cwWeight    = Just weight,
    cwMark      = Just mark
}

-- | Module data type
data Module = Module {
    mName      :: Maybe String,
    mCws       :: [Coursework],
    mWeight    :: Maybe Float,
    mMark      :: Maybe Float
}

-- | Smart constructor for Module data type
-- Empty coursework list creates a module with 0 weight and 0 mark
makeModule :: String -> [Coursework] -> Float -> Module
makeModule name [] weight = Module {
    mName      = Just name,
    mCws       = [],
    mWeight    = Just 0,
    mMark      = Just 0
}
makeModule name cws weight = Module {
    mName      = Just name,
    mCws       = cws,
    mWeight    = Just weight,
    mMark      = Just $ sum [ ( fromMaybe 0 (cwWeight cw) / 100) * fromMaybe 0 (cwMark cw) | cw <- cws ]
}

-- | Year data type
data Year = Year {
    yMs         :: [Module],
    yWeight     :: Maybe Float,
    yMark       :: Maybe Float
}

-- | Smart constructor for Year data type
-- Empty module list creates a year with 0 weight and 0 mark
makeYear :: [Module] -> Float -> Year
makeYear [] weight = Year {
    yMs         = [],
    yWeight     = Just 0,
    yMark       = Just 0
}
makeYear ms weight = Year {
    yMs         = ms,
    yWeight     = Just weight,
    yMark       = Just $ sum [ ( fromMaybe 0 (mWeight m) / 100) * fromMaybe 0 (mMark m) | m <- ms ]
}
