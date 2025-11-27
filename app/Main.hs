module Main where

-- Imports de librerías
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Juicy (loadJuicyPNG)
--Import 
import Config
import Types
import Render
import Input
import Logic

--Estado inicial
initialState :: StdGen -> Maybe Picture -> World
initialState gen sprite = World 
  { player     = Player (0, 0) 0.5 (False, False, False, False) (False, False, False, False) False
  , enemies    = [Enemy (300, 300) 1 Grunt] 
  , bullets    = []
  , powerups   = []
  , time       = 0
  , rng        = gen
  , spawnTimer = 1.0
  , puSpawnTimer = 5.0
  , scene      = Menu
  , playerSprite = sprite
  }

--Funcion principal
main :: IO ()
main = do
    gen <- getStdGen
    sprite <- loadJuicyPNG "assets/playerRight.png"
    play window 
         background 
         fps 
         (initialState gen sprite)
         render 
         handleInput_pure 
         update_pure