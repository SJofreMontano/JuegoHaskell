module Main where

-- Imports de librerías
import Graphics.Gloss
import System.Random

--Import 
import Config
import Types
import Render
import Input
import Logic

--Estado inicial
initialState :: StdGen -> World
initialState gen = World 
  { player     = Player (0, 0) 0.5 (False, False, False, False) (False, False, False, False) False
  , enemies    = [Enemy (300, 300) 1 Grunt] 
  , bullets    = []                                                                                  
  , powerups   = []                                                                                  
  , time       = 0
  , rng        = gen
  , spawnTimer = 1.0
  , puSpawnTimer = 5.0    
  , scene      = Menu                                                                           
  }

--Funcion principal
main :: IO ()
main = do
    gen <- getStdGen
   
    play window 
         background 
         fps 
         (initialState gen) 
         render 
         handleInput_pure 
         update_pure