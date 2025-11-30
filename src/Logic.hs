module Logic where

import Control.Monad.State
import System.Random
import Types
import Math
import Data.List (partition)

--Actualiza el estado
update_pure :: Float -> World -> World
update_pure dt oldWorld = execState (update_monadic dt) oldWorld

-- Dispatchea al Game Over o al juego normal.
update_monadic :: Float -> State World ()
update_monadic dt = do
    w <- get
    case scene w of
        Menu    -> return ()
        GameOver -> return ()
        Playing -> updateGameLogic dt


-- Secuencia Lógica Principal
updateGameLogic :: Float -> State World ()
updateGameLogic dt = do
    updateInvincibility dt
    movePlayer dt
    handleSpawning dt
    handlePowerUpSpawning dt  
    handleShooting dt
    moveBullets dt
    moveEnemies dt
    handleCollisions
    handlePlayerEnemyCollision
    handlePowerUpCollisions
    updateTime dt


-- Funciones de Lógica Pura

activatePowerUp :: State World ()
activatePowerUp = modify $ \w ->
    let p = player w
    in if pHasPowerUp p 
       then w { player = p { pHasPowerUp = False } 
              , bullets = (createBurstBullets (pPos p)) ++ (bullets w) 
              }
       else w 
  where
    createBurstBullets :: Pos -> [Bullet]
    createBurstBullets pos = 
        [ Bullet { bPos = pos, bVel = (cos rad * 450, sin rad * 450), bDuration = 1.0 }
        | i <- [0..127] :: [Int]
        , let rad = fromIntegral i * (2 * pi / 128)
        ]
 
--Mueve al jugador
movePlayer :: Float -> State World ()
movePlayer dt = modify $ \w ->
    let p = player w
        (mvx, mvy) = getDirectionFromKeys (pMoveKeys p)
        (px, py) = pPos p
        
        potentialX = px + mvx * 200 * dt
        potentialY = py + mvy * 200 * dt
        
        x_boundary = (1260 / 2) - 10 
        y_boundary = (700 / 2) - 10
        
        clampedX = clamp (-x_boundary) x_boundary potentialX
        clampedY = clamp (-y_boundary) y_boundary potentialY
            
    in w { player = p { pPos = (clampedX, clampedY) } }


--Maneja la spawn de los enemigos normales y tanques
handleSpawning :: Float -> State World ()
handleSpawning dt = do
    w <- get 
    let currentSpawnTimer = spawnTimer w - dt
    
    if currentSpawnTimer <= 0
    then do
        let (randX, gen1) = randomR (-610 :: Float, 610 :: Float) (rng w) 
            (randY, gen2) = randomR (-330 :: Float, 330 :: Float) gen1 
            (chance, gen3) = randomR (1 :: Int, 8 :: Int) gen2
            (newEnemy, nextRng) = if chance == 1
                then (Enemy (randX, randY) 3 Tank, gen3) 
                else (Enemy (randX, randY) 1 Grunt, gen3) 

            nextTime      = max 0.2 (2.0 - (time w * 0.05))
            
        put w { enemies = newEnemy : enemies w, 
                rng = nextRng, 
                spawnTimer = nextTime 
              }
    else
        put w { spawnTimer = currentSpawnTimer }


--Maneja como aparecen los PowerUp
handlePowerUpSpawning :: Float -> State World ()
handlePowerUpSpawning dt = do
    w <- get
    let currentPuSpawnTimer = puSpawnTimer w - dt

    if currentPuSpawnTimer <= 0
    then do
        let (randX, gen1) = randomR (-500 :: Float, 500 :: Float) (rng w) 
            (randY, gen2) = randomR (-250 :: Float, 250 :: Float) gen1
            newPowerUp    = PowerUp (randX, randY) BurstShot
            nextTime      = 10.0 + fst (randomR (-5.0 :: Float, 5.0 :: Float) gen2) 
        put w { powerups = newPowerUp : powerups w, 
                rng = gen2, 
                puSpawnTimer = nextTime 
              }
    else
        put w { puSpawnTimer = currentPuSpawnTimer }

--Disparo del jugador
handleShooting :: Float -> State World ()
handleShooting dt = modify $ \w ->
    let p = player w
        (svx, svy) = getDirectionFromKeys (pShootKeys p)
        isShooting = svx /= 0 || svy /= 0
        
        currentCooldown = pCooldown p - dt
        
        (newCooldown, newBulletList) = 
            if currentCooldown <= 0 && isShooting
            then (0.2, newBullet (pPos p) (svx, svy) : bullets w) 
            else (currentCooldown, bullets w)
            
    in w { player = p { pCooldown = newCooldown }, bullets = newBulletList }
  where
    newBullet pos (svx, svY) = Bullet 
        { bPos = pos             
        , bVel = (svx * 400, svY * 400) 
        , bDuration = 2.0             
        }

--Movimiento de las balas
moveBullets :: Float -> State World ()
moveBullets dt = modify $ \w ->
    let 
        movedBullets = map updateBullet (bullets w)
        isBulletInBounds :: Bullet -> Bool
        isBulletInBounds b = 
            let (x, y) = bPos b
                x_bound = 1260 / 2 
                y_bound = 700 / 2 
            in x < x_bound && x > (-x_bound) && y < y_bound && y > (-y_bound)
        updatedBullets = filter (\b -> bDuration b > 0 && isBulletInBounds b) movedBullets
    in w { bullets = updatedBullets }
  where
    updateBullet :: Bullet -> Bullet
    updateBullet b = b { bPos = (x + vx * dt, y + vy * dt), 
                            bDuration = bDuration b - dt }
      where (x, y) = bPos b; (vx, vy) = bVel b


--Movimiento enemigos persiquiendo al jugador
moveEnemies :: Float -> State World ()
moveEnemies dt = modify $ \w ->
    let ppos = pPos (player w)
        movedEnemies = map (moveEnemy ppos dt) (enemies w)
    in w { enemies = movedEnemies }
  where
    moveEnemy :: Pos -> Float -> Enemy -> Enemy
    moveEnemy ppos dt e = 
        let (ex, ey) = ePos e
            (dx, dy) = normalize (sub ppos (ex, ey))
            speed    = case eType e of  --Velocidad de movimiento de los enemigos
                         Grunt -> 80
                         Tank  -> 50 
        in e { ePos = (ex + dx * speed * dt, ey + dy * speed * dt) }


--Colision entre las balas y los enemigos (daño)
handleCollisions :: State World ()
handleCollisions = modify $ \w ->
    let collisionDist = 13.0
        allEnemies = enemies w
        allBullets = bullets w
        hitEnemy b = any (\e -> mag (sub (bPos b) (ePos e)) < collisionDist) allEnemies
        survivingBullets = filter (\b -> not (hitEnemy b)) allBullets
        
        damageEnemy :: Enemy -> Enemy
        damageEnemy e = 
            let bulletsHittingThisEnemy = filter (\b -> mag (sub (ePos e) (bPos b)) < collisionDist) allBullets
                damage = length bulletsHittingThisEnemy
            in e { eHp = eHp e - damage } 

        damagedEnemies = map damageEnemy allEnemies
        (killedEnemies, survivingEnemies) = partition (\e -> eHp e <= 0) damagedEnemies

        -- Contamos los enemigos eliminados por tipo
        killedGrunts = length $ filter (\e -> eType e == Grunt) killedEnemies
        killedTanks  = length $ filter (\e -> eType e == Tank) killedEnemies
    
    in w { enemies = survivingEnemies
         , bullets = survivingBullets
         , gruntKills = gruntKills w + killedGrunts
         , tankKills  = tankKills w + killedTanks
         }

-- Colisión Jugador-Enemigo y Lógica de Muerte
handlePlayerEnemyCollision :: State World ()
handlePlayerEnemyCollision = modify $ \w ->
    let p = player w
        px = pPos p
        isInvincible = pInvincibleTimer p > 0
        collisionDist = 20.0 
        
        isColliding = any (\e -> mag (sub px (ePos e)) < collisionDist) (enemies w)
        shouldTakeDamage = isColliding && not isInvincible

        newHealth = if shouldTakeDamage then pHealth p - 1 else pHealth p
        
        newInvincibleTimer = if shouldTakeDamage then 1.0 else pInvincibleTimer p
        
        -- Si la vida llega a 0, cambiamos la escena a GameOver
        newScene = if newHealth <= 0 then GameOver else scene w
        
    in w { player = p { pHealth = newHealth, pInvincibleTimer = newInvincibleTimer }
         , scene = newScene 
         }


--Colision del jugador con los PowerUps
handlePowerUpCollisions :: State World ()
handlePowerUpCollisions = modify $ \w ->
    let p = player w
        px = pPos p
        collisionDist = 18.0 
        notCollectedPowerUps = filter (\pu -> mag (sub px (puPos pu)) >= collisionDist) (powerups w)
        playerCollectedAny = length notCollectedPowerUps < length (powerups w)
        newPlayer = if playerCollectedAny
                    then p { pHasPowerUp = True } 
                    else p
    in w { player = newPlayer, powerups = notCollectedPowerUps }


--Incrementa el tiempo de juego
updateTime :: Float -> State World ()
updateTime dt = modify $ \w -> w { time = time w + dt }

-- Define los radios de colisión para el jugador y los enemigos
playerCollisionRadius :: Float
playerCollisionRadius = 20.0 

enemyCollisionRadius :: Float
enemyCollisionRadius = 20.0 

-- Actualiza el cronómetro de invencibilidad
updateInvincibility :: Float -> State World ()
updateInvincibility dt = modify $ \w ->
    let p = player w
        currentInvincibleTime = pInvincibleTimer p
        
        newTime = max 0.0 (currentInvincibleTime - dt)
        
    in w { player = p { pInvincibleTimer = newTime } }