module Render where

import Graphics.Gloss
import Types
import Menu (renderMenu)
import Config (window) 
import Graphics.Gloss.Juicy (loadJuicyPNG)
import qualified Data.List as L 


--Cambio de escena
render :: World -> Picture
render w = 
    case scene w of
        Menu     -> renderMenu w 
        Playing  -> renderGame w
        GameOver -> renderGameOver w


-- Dibuja el tiempo/puntaje
renderTime :: World -> Picture
renderTime world = 
    let timeValue = floor (time world)
        timeText = "Tiempo: " ++ show timeValue ++ "s"
    in renderText timeText (-125) 345

--Render del juego cuando se esta Playing
renderGame :: World -> Picture

renderGame w = pictures (backgroundPicture ++ gameElements)
  where
    backgroundPicture = case arenaBackgroundSprite w of
        Just pic -> [pic]
        Nothing  -> []
    gameElements = renderWall : renderHealth w : renderKillCounters w : renderPlayer : renderEnemies ++ renderBullets ++ renderPowerUps ++ [renderPowerUpIndicator w, renderTime w]
    -- MURO REDUCIDO
    x_wall = 1250 / 2
    y_wall = 650 / 2
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
            enemySprite = case eType e of
                Grunt -> gruntSprite w
                Tank  -> tankSprite w
        in case enemySprite of
            Just pic -> translate ex ey pic
            Nothing  -> let eColor = case eType e of { Grunt -> red; Tank -> yellow }
                            eSize  = case eType e of { Grunt -> 10; Tank -> 15 }
                        in translate ex ey $ color eColor $ circleSolid eSize


--Render PowerUp
renderPowerUpIndicator :: World -> Picture
renderPowerUpIndicator w = 
    let p = player w
        indicatorColor = if pHasPowerUp p then green else greyN 0.3
        indicatorSize  = 30                                         
        indicatorX = 0                                              
        indicatorY = -(740 / 2) + (indicatorSize / 2) + 10          
    in translate indicatorX indicatorY $ color indicatorColor $ rectangleSolid indicatorSize indicatorSize


-- Helper general para dibujar texto en una posición
renderText :: String -> Float -> Float -> Picture
renderText str x y = 
    translate x y $ scale 0.25 0.25 $ color white $ text str

-- Dibuja la salud del jugador 
renderHealth :: World -> Picture
renderHealth world = 
    let hp = pHealth (player world)
        
        -- Configuración de los bloques de vida
        heartSize  = 20.0
        spacing    = 25.0
        startX     = -520.0 -- Inicio de los bloques (después de la etiqueta "VIDA:")

        labelPic = renderText "Hp:" (-610) 340 
        
        heartsPic = 
            [ translate (startX + (fromIntegral i * spacing)) 345 $ color red $ rectangleSolid heartSize heartSize
            | i <- [0 .. hp - 1] 
            ]
        
    in pictures (labelPic : heartsPic) -- Junta la etiqueta y los corazones

-- Dibuja la pantalla de Game Over
renderGameOver :: World -> Picture
renderGameOver w = pictures
    [ translate 0 150 $ scale 0.5 0.5 $ color red $ text "GAME OVER"
    , translate 0 50 $ scale 0.2 0.2 $ color white $ text ("Sobreviviste: " ++ show (round (time w) :: Int) ++ " segundos")
    , translate 0 0 $ scale 0.2 0.2 $ color white $ text ("Grunts eliminados: " ++ show (gruntKills w))
    , translate 0 (-50) $ scale 0.2 0.2 $ color white $ text ("Tanks eliminados: " ++ show (tankKills w))
    ]

-- Dibuja los contadores de enemigos eliminados
renderKillCounters :: World -> Picture
renderKillCounters w =
    let gruntText = "Grunts: " ++ show (gruntKills w)
        tankText  = "Tanks: " ++ show (tankKills w)
        
        gruntCounterPic = renderText gruntText 500 395
        tankCounterPic  = renderText tankText 500 365
        
    in pictures [gruntCounterPic, tankCounterPic]