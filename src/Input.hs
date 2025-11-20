module Input where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Types
import Menu (handleMenuInput)
import Logic (activatePowerUp) 

--Convierte la funcion monadica en funcion para Gloss
handleInput_pure :: Event -> World -> World
handleInput_pure event oldWorld = execState (handleInput_monadic event) oldWorld


--Logica para el cambio de escena
handleInput_monadic :: Event -> State World ()
handleInput_monadic event = do
    w <- get
    case scene w of
        Menu    -> handleMenuInput event 
        Playing -> handleGameInput event 


-- Lógica monádica, actualiza el estado de las teclas
handleGameInput :: Event -> State World ()
handleGameInput event =
    case event of
        --Movimiento del jugador WASD
        (EventKey (Char c) keyState _ _) -> modify $ \w ->
            let p = player w
                (mu, md, ml, mr) = pMoveKeys p
                isDown = keyState == Down
                newMoveKeys = case c of
                    'w' -> (isDown, md, ml, mr)
                    's' -> (mu, isDown, ml, mr)
                    'a' -> (mu, md, isDown, mr)
                    'd' -> (mu, md, ml, isDown)
                    _   -> (mu, md, ml, mr)
            in w { player = p { pMoveKeys = newMoveKeys } }
        
        --Disparo del jugador flechas
        (EventKey (SpecialKey k) keyState _ _) -> 
            if k == KeySpace && keyState == Down 
            then activatePowerUp -- Llama a la función de Logic
            else modify $ \w ->
                let p = player w
                    (su, sd, sl, sr) = pShootKeys p
                    isDown = keyState == Down
                    newShootKeys = case k of
                        KeyUp    -> (isDown, sd, sl, sr)
                        KeyDown  -> (su, isDown, sl, sr)
                        KeyLeft  -> (su, sd, isDown, sr)
                        KeyRight -> (su, sd, sl, isDown)
                        _        -> (su, sd, sl, sr)
                in w { player = p { pShootKeys = newShootKeys } }
            
        _ -> return ()