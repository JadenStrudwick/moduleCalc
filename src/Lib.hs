module Lib where

type NameWeight = (Maybe String, Maybe Float)

data Coursework = Coursework {
    cName        :: Maybe String,
    cWeight      :: Maybe Float,
    cMark        :: Maybe Float
}
    deriving (Show, Eq)

data Node a = Node {
    nName        :: Maybe String,
    nWeight      :: Maybe Float,
    nCont        :: [a]
}
    deriving (Show, Eq)

type Module = Node Coursework
type Year   = Node Module
type Degree = [Year]

-- This should be good enough and then have a calculate button

sumMaybes :: (Num a) => [Maybe a] -> Maybe a
sumMaybes = fmap sum . sequence

calculateCourseworkMark :: Coursework -> Maybe Float
calculateCourseworkMark c = do
    weightPercent <- cWeight c
    let weight = weightPercent / 100
    mark   <- cMark c
    return $ mark * weight

calculateModuleMark :: Module -> Maybe Float
calculateModuleMark (Node _ _ []) = Nothing
calculateModuleMark m = do
    weightPercent <- nWeight m
    let weight = weightPercent / 100
    cwsMarks <- mapM calculateCourseworkMark (nCont m)
    return $ sum cwsMarks * weight

calculateYearMark :: Year -> Maybe Float
calculateYearMark (Node _ _ []) = Nothing
calculateYearMark y = do
    weightPercent <- nWeight y
    let weight = weightPercent / 100
    ms <- mapM calculateModuleMark (nCont y)
    return $ sum ms * weight

calculateDegreeMark :: Degree -> Maybe Float
calculateDegreeMark [] = Nothing
calculateDegreeMark ys = sumMaybes $ fmap calculateYearMark ys