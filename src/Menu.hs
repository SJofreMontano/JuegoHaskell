module Menu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Types

-- Dibuja el menu
renderMenu :: World -> Picture
renderMenu w = 
    let textElements = pictures 
            [translate 0 (-100) $ scale 0.2 0.2 $ color white $ text "ENTER PARA EMPEZAR"
            ]
    in case backgroundSprite w of
        Just bg -> pictures [bg, textElements]
        Nothing -> textElements

-- Maneja el input del menú 
handleMenuInput :: Event -> State World ()
handleMenuInput (EventKey (SpecialKey KeyEnter) Down _ _) = 
    modify (\w -> w { scene = Playing })
handleMenuInput _ = return ()