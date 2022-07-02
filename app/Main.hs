module Main where

import Lib
    ( makeCoursework, makeModule, makeYear, Coursework, Module, Year )
import Control.Monad (replicateM)

genCoursework :: IO Coursework
genCoursework = do
    putStrLn "Enter coursework name: "
    name <- getLine
    putStrLn "Enter coursework weight: "
    weight <- getLine
    putStrLn "Enter coursework mark: "
    makeCoursework name (read weight) . read <$> getLine

genModule :: IO Module
genModule = do
    putStrLn "Enter module name: "
    name <- getLine
    putStrLn "Enter module weight: "
    weight <- getLine
    putStrLn "Enter number of courseworks: "
    numCourseworks <- getLine
    courseworks <- replicateM (read numCourseworks) genCoursework
    return (makeModule name courseworks (read weight))

genYear :: IO Year
genYear = do
    putStrLn "Enter weight of year: "
    weight <- getLine
    putStrLn "Enter number of modules: "
    numModules <- getLine
    modules <- replicateM (read numModules) genModule
    return (makeYear modules (read weight))

main :: IO ()
main = genYear >>= print
