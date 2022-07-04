module Lib where

-- | Data types 
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

-- | Smart constructors
makeValidWeightMark :: Maybe Float -> Maybe Float
makeValidWeightMark Nothing = Nothing
makeValidWeightMark (Just x)
    | x < 0 || x > 100 = Nothing
    | otherwise = Just x

mkCoursework :: Maybe String -> Maybe Float -> Maybe Float -> Coursework
mkCoursework name weight mark = Coursework name (makeValidWeightMark weight) (makeValidWeightMark mark)

mkNode :: Maybe String -> Maybe Float -> [a] -> Node a
mkNode name weight = Node name (makeValidWeightMark weight)

-- | Helper functions
class Uni a where
    calculateMark :: a -> Maybe Float

instance Uni Coursework where
    calculateMark c =  do
        weightPercent <- cWeight c
        let weight = weightPercent / 100
        mark   <- cMark c
        return $ mark * weight

instance (Uni a) => Uni (Node a) where
    calculateMark (Node _ _ []) = Nothing
    calculateMark m = do
        weightPercent <- nWeight m
        let weight = weightPercent / 100
        cwsMarks <- mapM calculateMark (nCont m)
        return $ sum cwsMarks * weight

instance (Uni a) => Uni [a] where
    calculateMark [] = Nothing
    calculateMark ys = sumMaybes $ fmap calculateMark ys
        where sumMaybes = fmap sum . sequence
