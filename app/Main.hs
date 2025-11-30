module Main where
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Config
import Types
import Render
import Input
import Logic

initialState :: StdGen -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> World
initialState gen pSprite bgSprite gSprite tSprite arenaSprite = World 
  { player     = Player (0, 0) 0.5 (False, False, False, False) (False, False, False, False) False 5 0.0
  , enemies    = [Enemy (300, 300) 1 Grunt] 
  , bullets    = []
  , powerups   = []
  , time       = 0
  , rng        = gen
  , spawnTimer = 1.0
  , puSpawnTimer = 5.0
  , scene      = Menu
  , playerSprite = pSprite
  , backgroundSprite = bgSprite
  , gruntSprite = gSprite
  , tankSprite = tSprite
  , arenaBackgroundSprite = arenaSprite
  , gruntKills = 0
  , tankKills  = 0
  }

main :: IO ()
main = do

    gen <- getStdGen
    pSprite <- loadJuicyPNG "assets/playerRight.png"
    bgSprite <- loadJuicyPNG "assets/menuBackground.png"
    gSprite <- loadJuicyPNG "assets/grunt.png"
    tSprite <- loadJuicyPNG "assets/tank.png"
    arenaSprite <- loadJuicyPNG "assets/arenaBackground.png"
    play window 
         background 
         fps 
         (initialState gen pSprite bgSprite gSprite tSprite arenaSprite)
         render 
         handleInput_pure 
         update_pure