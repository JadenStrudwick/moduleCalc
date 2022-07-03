module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Lib

main :: IO ()
main = do
    startGUI defaultConfig {
        jsPort = Just 8080
    } setup

setup :: Window -> UI ()
setup window = do
    _ <- return window # set title "Module Calculator"
    mkCourseworkComponent window (makeCoursework "TestCoursework" 20 70)
    return ()

-- Courseworks also do not save their state

mkNameInput :: Maybe String -> String -> UI Element
mkNameInput (Just name) hint = UI.input # set (attr "placeholder") hint # set value name
mkNameInput Nothing hint = UI.input # set (attr "placeholder") hint # set value ""

mkNumberInput :: Maybe Float -> String -> UI Element
mkNumberInput (Just number) hint = UI.input # set (attr "placeholder") hint # set value (show number) # set (attr "type") "number" # set (attr "min") "0" # set (attr "max") "100"
mkNumberInput Nothing hint = UI.input # set (attr "placeholder") hint # set value ""

mkDivBoxAndAddToWindow :: Window -> [Element] -> UI Element
mkDivBoxAndAddToWindow window elements = do 
    div <- UI.div #+ (element <$> elements)
    _ <- getBody window #+ [element div]
    return div

mkCourseworkComponent :: Window -> Coursework -> UI Element
mkCourseworkComponent window c = do
    name <- mkNameInput (cwName c) "Coursework name"
    weight <- mkNumberInput (cwWeight c) "Coursework weight"
    mark <- mkNumberInput (cwMark c) "Coursework mark"
    mkDivBoxAndAddToWindow window [name, weight, mark]

mkModuleComponent :: Window -> Module -> UI Element
mkModuleComponent window m = do
    name <- mkNameInput (mName m) "Module name"
    weight <- mkNumberInput (mWeight m) "Module weight"
    mkDivBoxAndAddToWindow window [name, weight]

