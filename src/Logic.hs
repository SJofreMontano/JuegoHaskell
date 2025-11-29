module Logic where

import Data.List (partition)
import Control.Monad.State
import System.Random
import Types
import Math 


update_pure :: Float -> World -> World
update_pure dt oldWorld = execState (update_monadic dt) oldWorld

--Actualiza el estado
update_monadic :: Float -> State World ()
update_monadic dt = do
    w <- get
    case scene w of
        Menu    -> return ()
        Playing -> updateGameLogic dt


-- Secuencia Lógica Principal
updateGameLogic :: Float -> State World ()
updateGameLogic dt = do
    w <- get
    -- Si el jugador está muerto, no actualizamos la lógica del juego.
    unless (pDead (player w)) $ do
        movePlayer dt
        handleSpawning dt
        handlePowerUpSpawning dt  
        handleShooting dt
        moveBullets dt
        moveEnemies dt
        handleCollisions -- Colisiones de balas con enemigos
        handlePlayerEnemyCollisions -- Colisiones de jugador con enemigos
        handlePowerUpCollisions

    -- Comprueba si el jugador ha muerto en este frame para cambiar la escena
    w_after_updates <- get
    when (pDead (player w_after_updates)) $
        modify $ \w' -> w' { scene = GameOver }

    updateTime dt


-- Funciones de Lógica Pura

--Esta es llamada por Input.hs
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
        
        x_boundary = (1200 / 2) - 10 
        y_boundary = (640 / 2) - 10
        
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
        let (randX, gen1) = randomR (-580 :: Float, 580 :: Float) (rng w) 
            (randY, gen2) = randomR (-300 :: Float, 300 :: Float) gen1 
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
        movedBullets = map (updateBullet dt) (bullets w)
        isBulletInBounds :: Bullet -> Bool
        isBulletInBounds b = 
            let (x, y) = bPos b
                x_bound = 1200 / 2 
                y_bound = 640 / 2 
            in x < x_bound && x > (-x_bound) && y < y_bound && y > (-y_bound)
        updatedBullets = filter (\b -> bDuration b > 0 && isBulletInBounds b) movedBullets
    in w { bullets = updatedBullets }
  where
    updateBullet :: Float -> Bullet -> Bullet
    updateBullet dt b = b { bPos = (x + vx * dt, y + vy * dt), 
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
        survivingEnemies = filter (\e -> eHp e > 0) damagedEnemies
    
    in w { enemies = survivingEnemies, bullets = survivingBullets }

--Colision entre el jugador y los enemigos
handlePlayerEnemyCollisions :: State World ()
handlePlayerEnemyCollisions = modify $ \w ->
    let p = player w
        es = enemies w
        (updatedPlayer, remainingEnemies) = handleEnemyCollisions p es
    in w { player = updatedPlayer, enemies = remainingEnemies }



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
playerCollisionRadius = 20.0 -- Hitbox jugador

enemyCollisionRadius :: Float
enemyCollisionRadius = 20.0 -- Hitbox enemigo

-- Comprueba si un enemigo colisiona con el jugador
isColliding :: Player -> Enemy -> Bool
isColliding player enemy = distance < (playerCollisionRadius + enemyCollisionRadius)
  where
    distance = mag $ sub (pPos player) (ePos enemy)

-- Maneja las colisiones entre el jugador y una lista de enemigos
handleEnemyCollisions :: Player -> [Enemy] -> (Player, [Enemy])
handleEnemyCollisions player enemies = 
    let 
        -- Separa los enemigos que colisionan de los que no
        (collidingEnemies, nonCollidingEnemies) = partition (isColliding player) enemies
        
        -- Calcula el daño total basado en el número de enemigos que colisionan
        damage = length collidingEnemies

        -- Reduce la vida del jugador
        newHp = pHp player - damage

        -- Actualiza el estado del jugador
        updatedPlayer = player 
            { pHp = newHp
            , pDead = newHp <= 0
            }
    in
        -- Si el jugador no está muerto, devuelve el jugador actualizado y los enemigos que no colisionaron
        if not (pDead player)
        then (updatedPlayer, nonCollidingEnemies)
        else (player { pDead = True }, enemies) -- Si ya estaba muerto, no hagas nada
