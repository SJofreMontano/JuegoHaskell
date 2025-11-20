module Menu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Types

-- Dibuja el menu
renderMenu :: World -> Picture
renderMenu w = 
    let title = translate 0 100 $ scale 0.5 0.5 $ color cyan $ text "HASKELL SURVIVORS"
        prompt = translate 0 (-50) $ scale 0.2 0.2 $ color white $ text "ENTER PARA EMPEZAR"
    in pictures [title, prompt]

-- Maneja el input del menú 
handleMenuInput :: Event -> State World ()
handleMenuInput (EventKey (SpecialKey KeyEnter) Down _ _) = 
    modify (\w -> w { scene = Playing })
handleMenuInput _ = return ()