--Tipos de datos que se usan en el juego
module Types (
    Pos, Vel,
    Scene(..),
    Player(..),
    EnemyType(..),
    Enemy(..),
    Bullet(..),
    PowerUpType(..),
    PowerUp(..),
    World(..) -- Esto exporta el tipo World y todos sus campos, ¡incluyendo 'scene'!
) where

import System.Random (StdGen)
import Graphics.Gloss (Picture)

type Pos = (Float, Float)
type Vel = (Float, Float)


--Define escena del juego, puede estar en menu o jugando
data Scene = Menu | Playing | GameOver deriving (Eq, Show)

--Define el jugador
data Player = Player
  { pPos        :: Pos
  , pCooldown   :: Float 
  , pMoveKeys   :: (Bool, Bool, Bool, Bool) -- WASD (W, S, A, D)
  , pShootKeys  :: (Bool, Bool, Bool, Bool) -- Flechas (Arr, Abj, Izq, Der)
  , pHasPowerUp :: Bool
  , pHealth        :: Int  
  , pInvincibleTimer     :: Float    
  }

--Enemigos
data EnemyType = Grunt | Tank deriving (Eq, Show)

data Enemy = Enemy 
  { ePos  :: Pos
  , eHp   :: Int         
  , eType :: EnemyType   
  }


--Armas
data Bullet = Bullet { bPos :: Pos, bVel :: Vel, bDuration :: Float }
data PowerUpType = BurstShot 
data PowerUp = PowerUp
  { puPos  :: Pos
  , puType :: PowerUpType
  }


--Configuracion del mundo
data World = World
  { player     :: Player
  , enemies    :: [Enemy]
  , bullets    :: [Bullet]
  , powerups   :: [PowerUp]
  , time       :: Float
  , rng        :: StdGen
  , spawnTimer :: Float
  , puSpawnTimer :: Float
  , scene      :: Scene
  , playerSprite :: Maybe Picture
  , backgroundSprite :: Maybe Picture
  , gruntSprite :: Maybe Picture
  , tankSprite  :: Maybe Picture
  , arenaBackgroundSprite :: Maybe Picture
  , gruntKills :: Int
  , tankKills  :: Int
  }