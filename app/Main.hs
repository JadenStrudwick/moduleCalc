module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Widgets

import CLI ( cliMain )
import Control.Monad (void)

main :: IO ()
main = do
    startGUI defaultConfig {
        jsPort = Just 8080 
    } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Module Calculator"
    mkReactiveTextField window "test"
    return ()

mkReactiveTextField :: Window -> String -> UI Element
mkReactiveTextField window labelString = do

    label <- UI.p # set text labelString                -- Create paragraph label
    input <- UI.input # set value ""                    -- Create input field
    div <- UI.div #+ [element label, element input]     -- Create div container
    getBody window #+ [element div]                     -- Add div to body

    on UI.keyup input $ \keyCode -> do                  -- When input field is changed
        if keyCode == 13 then do                        -- If enter key is pressed
            val <- get value input                      -- Get value of input field
            div' <- mkReactiveTextField window val      -- Create new div
            delete div                                  -- Delete old div
            return div'                                 -- Return new div
        else 
            return div                                  -- Return div container 

    return div                                          -- Return div container

