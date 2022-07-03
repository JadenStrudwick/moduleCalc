module ReactiveTextField where

mkReactiveTextField :: Window -> String -> UI Element
mkReactiveTextField window labelString = do

    label <- UI.p # set text labelString                -- Create paragraph label
    input <- UI.input # set value ""                    -- Create input field
    divBox <- UI.div #+ [element label, element input]  -- Create div container
    _ <- getBody window #+ [element divBox]             -- Add div to body

    on UI.keyup input $ \keyCode -> do                  -- When input field is changed
        if keyCode == 13 then do                        -- If enter key is pressed
            val <- get value input                      -- Get value of input field
            divBox' <- mkReactiveTextField window val   -- Create new div
            delete divBox                               -- Delete old div
            return divBox'                              -- Return new div
        else
            return divBox                               -- Return div container 

    return divBox                                       -- Return div container