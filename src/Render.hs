module Render where

import Graphics.Gloss
import Types
import Menu (renderMenu) -- Necesario para dibujar el menú
import Config (window) 
import Graphics.Gloss.Juicy (loadJuicyPNG)
--Cambio de escena
render :: World -> Picture
render w = 
    case scene w of
        Menu     -> renderMenu w 
        Playing  -> renderGame w
        GameOver -> renderGameOver w


--Render del juego cuando se esta Playing
renderGame :: World -> Picture
renderGame w = pictures (renderWall : renderHealth (player w): renderPlayer : renderEnemies ++ renderBullets ++ renderPowerUps ++ [renderPowerUpIndicator w])
  where
    -- MURO REDUCIDO
    x_wall = 1200 / 2
    y_wall = 640 / 2
    wallPath = [ (x_wall, y_wall), (-x_wall, y_wall), (-x_wall, -y_wall), (x_wall, -y_wall) ] 
    renderWall = color white (lineLoop wallPath)
    
    --JUGADOR
    (px, py) = pPos (player w)
    renderPlayer = case playerSprite w of
        Just pic -> translate px py pic
        Nothing  -> translate px py $ color cyan $ circleSolid 10
                  
    --BALAS
    renderBullets = map drawBullet (bullets w)
    drawBullet b = let (bx, by) = bPos b in translate bx by $ color yellow $ circleSolid 3

    --POWERUPS
    renderPowerUps = map drawPowerUp (powerups w)
    drawPowerUp pu = let (pux, puy) = puPos pu in translate pux puy $ color green $ circleSolid 8 

    --ENEMIGOS
    renderEnemies = map drawEnemy (enemies w)
    drawEnemy e = 
        let (ex, ey) = ePos e
            eColor   = enemyColor (eType e)
            eSize    = enemySize (eType e)
        in translate ex ey $ color eColor $ circleSolid eSize

    enemyColor :: EnemyType -> Color
    enemyColor Grunt = red
    enemyColor Tank  = yellow 

    enemySize :: EnemyType -> Float
    enemySize Grunt = 10
    enemySize Tank  = 15 


--Render PowerUp
renderPowerUpIndicator :: World -> Picture
renderPowerUpIndicator w = 
    let p = player w
        indicatorColor = if pHasPowerUp p then green else greyN 0.3
        indicatorSize  = 30                                         
        indicatorX = 0                                              
        indicatorY = -(720 / 2) + (indicatorSize / 2) + 10          
    in translate indicatorX indicatorY $ color indicatorColor $ rectangleSolid indicatorSize indicatorSize

-- En Render.hs

-- Dibuja la vida del jugador en la esquina superior izquierda
renderHealth :: Player -> Picture
renderHealth p =
    translate (-580) (380) $ -- Posición en la esquina superior izquierda
    scale 0.2 0.2 $
    color white $
    text ("Vidas: " ++ show (max 0 (pHp p)))

-- Dibuja la pantalla de Game Over
renderGameOver :: World -> Picture
renderGameOver w = pictures
    [ translate 0 100 $ scale 0.5 0.5 $ color red $ text "GAME OVER"
    , translate 0 (-50) $ scale 0.2 0.2 $ color white $ text ("Sobreviviste: " ++ show (round (time w) :: Int) ++ " segundos")
    ]
