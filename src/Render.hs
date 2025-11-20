module Render where

import Graphics.Gloss
import Types
import Menu (renderMenu) -- Necesario para dibujar el menú
import Config (window) 

--Cambio de escena
render :: World -> Picture
render w = 
    case scene w of
        Menu    -> renderMenu w 
        Playing -> renderGame w 


--Render del juego cuando se esta Playing
renderGame :: World -> Picture
renderGame w = pictures (renderWall : renderPlayer : renderEnemies ++ renderBullets ++ renderPowerUps ++ [renderPowerUpIndicator w])
  where
    -- MURO REDUCIDO
    x_wall = 1200 / 2
    y_wall = 640 / 2
    wallPath = [ (x_wall, y_wall), (-x_wall, y_wall), (-x_wall, -y_wall), (x_wall, -y_wall) ] 
    renderWall = color white (lineLoop wallPath)
    
    -- JUGADOR
    (px, py) = pPos (player w)
    renderPlayer = translate px py $ color cyan $ circleSolid 10
                  
    -- BALAS
    renderBullets = map drawBullet (bullets w)
    drawBullet b = let (bx, by) = bPos b in translate bx by $ color yellow $ circleSolid 3

    -- POWERUPS
    renderPowerUps = map drawPowerUp (powerups w)
    drawPowerUp pu = let (pux, puy) = puPos pu in translate pux puy $ color green $ circleSolid 8 

    -- ENEMIGOS
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