module Main where

  import Graphics.UI.Fungen
  import Graphics.Rendering.OpenGL (GLdouble)
  import Control.Concurrent
  import Control.Concurrent.MVar
  import System.Random

  
  type PlayerCharacter = GameObject ()
  type PlayerBullet = GameObject ()
  type PlayerAction a = IOGame GameAttribute () GameState TileAttribute a
  
  type Boss = GameObject ()
  type InvaderAction a = IOGame GameAttribute () GameState TileAttribute a
  bossHealth = 5
  bossSpeed = 2
  invaderYSpeed = 0
  invaderXSpeed = 4
  enemy1Amount = 64
  enemy1Delay = 2.0
  enemy1Speed = 3.0
  enemy2Amount = 72
  enemy2Delay = 3.5
  enemy2Speed = 6.5

  data GameAttribute = GA Int Int Double Bool Int Int

  data GameState = Level Int
  data TileAttribute = NoTileAttribute
  type GameTile = Tile TileAttribute
  type PLCGameMap = TileMatrix TileAttribute

  velocidadeDoPulo = 20
  pSqSize = 12
  bulletSpeed = 15
  maxAmmo = 100

  gravityScale = 10
  frameTime = 16

  width = 780
  height = 600
  w = fromIntegral width :: GLdouble
  h = fromIntegral height :: GLdouble

  tileSize :: GLdouble
  tileSize = 32.0

  border1, border2, border3, free1, free2, free3, inimigoMaior, miniInimigo, megamen, boss  :: Int
  border1 = 1
  border2 = 2
  border3 = 3
  free1   = 4
  free2   = 5
  free3   = 6
  inimigoMaior = 8
  miniInimigo = 10
  megamen = 9
  boss = 7

  magenta :: InvList
  magenta = Just [(255,0,255)]

  main :: IO ()
  main = do
    let winConfig = ((100,80),(width,height),"PLC Project")

        bmpList  = [("tex.bmp",             magenta),
                    ("border1.bmp",         magenta),
                    ("border2.bmp",         magenta),
                    ("border3.bmp",         magenta),
                    ("free1.bmp",           magenta),
                    ("free2.bmp",           magenta),
                    ("free3.bmp",           magenta),
                    ("boss.bmp",            magenta),
                    ("inimigoMaior.bmp",    magenta),
                    ("megamen.bmp",         magenta),
                    ("miniInimigo.bmp",     magenta)]

        gameMap  = multiMap [(tileMap map1 tileSize tileSize),
                             (tileMap map2 tileSize tileSize),
                             (tileMap map3 tileSize tileSize)] 0

        groups = [(objectGroup "playerGroup"      [createPlayer] ),
                  (objectGroup "bulletGroup"       createBullets ),
                  (objectGroup "bossGroup"       [initBoss] ),
                  (objectGroup "enemy1Group"     createEnemiesT1 ),
                  (objectGroup "enemy2Group"     createEnemiesT2 )]

        initAttributes = GA bossHealth maxAmmo 1.0 False enemy1Amount enemy2Amount

        input = [
          (SpecialKey KeyUp,              Press, playerJump)
          ,(SpecialKey KeyDown,            Press, playerFall)
          ,(Char 'z',                     Press, spawnBullet)
          ,(Char 'x',                     Press, spawnBullet)
          ,(Char 'q',                 Press, \_ _ -> funExit)
          ]
    
    sem1 <- newMVar False
    sem2 <- newMVar False
    threadKiller <- newMVar 0
    funInit winConfig gameMap groups (Level 1) initAttributes input (gameCycle sem1 sem2 threadKiller) (Timer frameTime) bmpList

  createPlayer:: PlayerCharacter
  createPlayer =
     let playerPoly = Tex (134/2, 113/2) megamen
     in object "player" playerPoly False (w - 25, pSqSize*10) (0,0) ()

  playerDefaultCol :: PlayerAction ()
  playerDefaultCol = do
    player <- findObject "player" "playerGroup"
    (pX, pY) <- getObjectPosition player
    tile <- getTileFromWindowPosition (pX, pY - pSqSize)
    (vX, vY) <- getObjectSpeed player
    (GA e a t b et1 et2) <- getGameAttribute
    when(getTileBlocked tile) (do 
      setObjectPosition (pX, tileSize + (pSqSize)) player
      setObjectSpeed (vX, 0) player
      if(b)
       then(setGameAttribute(GA e a t False et1 et2))
       else(return())
      )
    when(not (getTileBlocked tile))( (setObjectSpeed ( (0.0, vY-(gravityScale/10) ) ) player) ) 
   
  playerEnemyCollision :: MVar Int -> PlayerAction() 
  playerEnemyCollision threadKiller = do
    player <- findObject "player" "playerGroup"
    
    enemies <- getObjectsFromGroup "enemy1Group"
    col1 <- objectListObjectCollision enemies player
    
    enemies <- getObjectsFromGroup "enemy2Group"
    col2 <- objectListObjectCollision enemies player

    enemies <- getObjectsFromGroup "bossGroup"
    col3 <- objectListObjectCollision enemies player

    when(col1 || col2 || col3) (do 
              killThreads threadKiller
              setGameState(Level 1)
              setNewLevel 1)

  checkPlayerCollisions :: MVar Int -> PlayerAction ()
  checkPlayerCollisions threadKiller = do
    playerEnemyCollision threadKiller
    playerDefaultCol

  playerJump :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
  playerJump _ _ = do
     player <- findObject "player" "playerGroup"
     (vX, vY) <- getObjectSpeed player
     (GA e a t b et1 et2) <- getGameAttribute
     if(not b)
      then (do 
        setObjectSpeed(vX, velocidadeDoPulo) player
        setGameAttribute(GA e a t True et1 et2)
        )
      else return()

 
  playerFall :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
  playerFall _ _ = do
     player <- findObject "player" "playerGroup"
     (vX, vY) <- getObjectSpeed player
     (GA e a t b et1 et2) <- getGameAttribute
     if(b)
      then (do 
        setObjectSpeed(vX, -velocidadeDoPulo) player
        )
      else return()

  createBullets :: [PlayerBullet]
  createBullets =
    let bulletPic = Basic (Circle 3.0 1.0 1.0 0 Filled)
    in (createAsleepBullets 1 maxAmmo bulletPic)

  createAsleepBullets :: Int -> Int -> ObjectPicture -> [PlayerBullet]
  createAsleepBullets tMin tMax pic
   | (tMin > tMax) = []
   | otherwise = (object ("bullet" ++ (show tMin)) pic True (0,0) (bulletSpeed,0) ()):(createAsleepBullets (tMin + 1) tMax pic)

  spawnBullet :: Modifiers -> Position -> PlayerAction ()
  spawnBullet _ _= do
    (GA e a t b et1 et2) <- getGameAttribute
    bullet <- findObject ("bullet" ++ (show a)) "bulletGroup" 
    player <- findObject "player" "playerGroup" 
    (pX, pY) <- getObjectPosition player
    (sX, sY) <- getObjectSize player
    setObjectAsleep False bullet
    setObjectPosition (pX, pY) bullet 
    setObjectSpeed (-bulletSpeed, 0) bullet
    setGameAttribute(GA e (a-1) t b et1 et2)

  checkBulletCollision :: PlayerBullet -> [Boss] -> PlayerAction ()
  checkBulletCollision bullet (x:xs) = do
    col <- objectsCollision bullet x
    if(col)
      then( do
            setObjectAsleep True bullet
            (GA e a t b et1 et2) <- getGameAttribute
            setGameAttribute(GA (e-1) a t b et1 et2) 
            if(e <= 0)
              then( setObjectAsleep True x)
              else return())
      else if(not(length xs == 0))
        then (checkBulletCollision bullet xs)
        else return()

  checkAllBulletsCollision :: [PlayerBullet] -> PlayerAction ()
  checkAllBulletsCollision (b:bs)
   | (length bs == 0) = do
                        invaders <- getObjectsFromGroup "bossGroup"
                        checkBulletCollision b invaders
                        return()
   | otherwise = do 
                 invaders <- getObjectsFromGroup "bossGroup"
                 checkBulletCollision b invaders
                 checkAllBulletsCollision bs 

  disableAllBullets :: [PlayerBullet] -> PlayerAction ()
  disableAllBullets [] = return ()
  disableAllBullets (x:xs) = do
    setObjectAsleep True x
    disableAllBullets xs         
                 
  createInvaderAt :: (GLdouble, GLdouble) -> String -> Boss
  createInvaderAt (pX, pY) name =
      let invaderBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)]
          invaderPoly   = Basic (Polyg invaderBounds 1.0 0.5 0.0 Filled)
      in object name invaderPoly False (pX, pY) (invaderXSpeed, invaderYSpeed) ()


  initBoss :: Boss
  initBoss = 
    let invaderPoly = Tex (175/2, 215/2) inimigoMaior
    in object "boss" invaderPoly False (-20, (215/2) - 40) (bossSpeed, 0) ()
  resetInvadersPos :: [Boss] -> InvaderAction ()
  resetInvadersPos [] = return ()
  resetInvadersPos (i:xs) = do  
      setObjectAsleep False i
      setObjectPosition (-20,(215/2) - 40) i
      resetInvadersPos xs

      
  checkInvadersMapCol :: [Boss] -> InvaderAction ()
  checkInvadersMapCol [] = return()
  checkInvadersMapCol (x:xs) = do
    invaderPos <- getObjectPosition x
    tile <- getTileFromWindowPosition invaderPos
    if(getTileBlocked tile)
      then (do 
             setGameState (Level 1)
             setNewLevel 1)
      else checkInvadersMapCol xs

  checkInvadersBottomMapCol :: InvaderAction ()
  checkInvadersBottomMapCol = do
    invaders <- getObjectsFromGroup "bossGroup"
    checkInvadersMapCol invaders

  killThreads :: MVar Int -> PlayerAction()
  killThreads threadKiller = do
      state <- getGameState
      case state of
          Level n -> case n of
                     1 -> return()
                     2 -> liftIOtoIOGame $ modifyMVar_ threadKiller (\x -> return(x+2))
                     3 -> liftIOtoIOGame $ modifyMVar_ threadKiller (\x -> return(x+2))
  
  delayedStartup :: IO() -> IO()
  delayedStartup f = do
      threadDelay 500000000
      f
  
  wakePeriodically :: MVar Bool -> MVar Int -> Double -> IO()
  wakePeriodically sem threadKiller avg = do
      modifyMVar_ sem (\b -> return(True))
      gen <- getStdGen
      (rand, newgen) <- return(randomR (-avg*2/3,avg/4) gen)
      setStdGen newgen
      threadDelay ((round (avg + rand) * 1000000))
      i <- readMVar threadKiller
      if(0<i)
      then (do
          modifyMVar_ threadKiller (\x -> return(x-1))
          modifyMVar_ sem (\b -> return(False))
          return()
          )
      else (do
          wakePeriodically sem threadKiller avg
          )

  createEnemyT1 :: String -> Boss
  createEnemyT1 name =
      let invaderPoly = Tex (117/2, 108/2) miniInimigo
      in object name invaderPoly True (-10, ((108/2))) (enemy1Speed,0) ()

  createAsleepEnemies1 :: Int -> [Boss]
  createAsleepEnemies1 amount
   | (amount >= enemy1Amount) = []
   | otherwise = do 
      let name = "enemy1" ++ show(amount)
      (createEnemyT1 (name)):createAsleepEnemies1(amount+1)    

  createEnemiesT1 :: [Boss]
  createEnemiesT1 = createAsleepEnemies1 0    

  spawnT1Enemy :: MVar Bool -> InvaderAction ()
  spawnT1Enemy sem = do
    (GA e a t b et1 et2) <- getGameAttribute
    b <- liftIOtoIOGame(readMVar sem)
    when(b && 0 < et1) (do 
      enemy <- findObject ("enemy1" ++ show(et1 - 1)) "enemy1Group"
      setObjectAsleep False enemy
      setGameAttribute (GA e a t b (et1 - 1) et2)
      liftIOtoIOGame (modifyMVar_ sem (\b -> return (False)))
      )

  resetT1Enemies :: [Boss] -> InvaderAction ()
  resetT1Enemies [] = return ()
  resetT1Enemies (x:xs) = do
    setObjectPosition ((-10, (pSqSize+30))) x  
    setObjectAsleep True x
    resetT1Enemies xs
      
  createEnemyT2 :: String -> Boss
  createEnemyT2 name =
      let invaderPoly = Tex (132/2, 131/2) boss
      in object name invaderPoly True (-10, (131/2) + 5) (enemy2Speed,0) ()

  createAsleepEnemies2 :: Int -> [Boss]
  createAsleepEnemies2 amount
   | (amount >= enemy2Amount) = []
   | otherwise = do 
      let name = "enemy2" ++ show(amount)
      (createEnemyT2 (name)):createAsleepEnemies2(amount+1)    

  createEnemiesT2 :: [Boss]
  createEnemiesT2 = createAsleepEnemies2 0   
  
  spawnT2Enemy :: MVar Bool -> InvaderAction ()
  spawnT2Enemy sem = do
    (GA e a t b et1 et2) <- getGameAttribute
    b <- liftIOtoIOGame(readMVar sem)
    when(b && 0 < et2) (do 
      enemy <- findObject ("enemy2" ++ show(et2 - 1)) "enemy2Group"
      setObjectAsleep False enemy
      setGameAttribute (GA e a t b et1 (et2 - 1))
      liftIOtoIOGame (modifyMVar_ sem (\b -> return (False)))
      )

  resetT2Enemies :: [Boss] -> InvaderAction ()
  resetT2Enemies [] = return ()
  resetT2Enemies (x:xs) = do
    setObjectPosition (-10, (pSqSize+30)) x  
    setObjectAsleep True x
    resetT2Enemies xs

  setNewLevel :: Int -> PlayerAction ()
  setNewLevel n = do
    if(n >= 1 && n <= 3)
      then(do
           resetLevelAttributes (bossHealth*(n))

           invaders <- getObjectsFromGroup "bossGroup"
           resetInvadersPos invaders

           t1Enemies <- getObjectsFromGroup "enemy1Group"
           resetT1Enemies t1Enemies

           t2Enemies <- getObjectsFromGroup "enemy2Group"
           resetT2Enemies t2Enemies

           bullets <- getObjectsFromGroup "bulletGroup"
           disableAllBullets bullets

           setCurrentMapIndex (n-1))
           
      else return()     

  resetLevelAttributes :: Int -> PlayerAction()
  resetLevelAttributes hp = do
     (GA e a t b et1 et2) <- getGameAttribute
     setGameAttribute(GA hp maxAmmo t b enemy1Amount enemy2Amount)
 
  gameCycle :: MVar Bool -> MVar Bool -> MVar Int -> PlayerAction ()
  gameCycle sem1 sem2 threadKiller = do 
    bullets <- getObjectsFromGroup "bulletGroup"
    gState <- getGameState
    (GA e a t b et1 et2) <- getGameAttribute

    spawnT1Enemy sem1
    spawnT2Enemy sem2

    checkAllBulletsCollision bullets
    checkPlayerCollisions threadKiller
    checkInvadersBottomMapCol

    if(e <= 0)
      then(do         
        case gState of
          Level n -> case n of
                     1 -> (do 
                          setGameState (Level 2)
                          setNewLevel 2
                          liftIOtoIOGame (forkIO (wakePeriodically sem1 threadKiller enemy1Delay))
                          liftIOtoIOGame (forkIO (wakePeriodically sem2 threadKiller enemy1Delay))
                          return())
                     2 -> (do 
                          setGameState (Level 3)
                          setNewLevel 3
                          liftIOtoIOGame (forkIO (wakePeriodically sem1 threadKiller enemy1Delay))
                          liftIOtoIOGame (forkIO (wakePeriodically sem2 threadKiller enemy1Delay))
                          return())
                     3 -> (do 
                          killThreads threadKiller
                          e1 <- getObjectsFromGroup "enemy1Group"
                          e2 <- getObjectsFromGroup "enemy2Group"
                          resetT1Enemies e1
                          resetT2Enemies e2))
      else return()

    showFPS TimesRoman24 (w-24, h-28) 1.0 0.0 0.0
    printOnScreen ("Boss Health: " ++ show e) TimesRoman24 (0,0) 1.0 1.0 1.0
    



  b,f,b2,g2,b3,g3 :: GameTile
  b = (border1, True,  0.0, NoTileAttribute)
  f = (free1,   False, 0.0, NoTileAttribute)
  b2 = (border2, True,  0.0, NoTileAttribute)
  g2 = (free2, False,  0.0, NoTileAttribute)
  b3 = (border3, True,  0.0, NoTileAttribute)
  g3 = (free3, False,  0.0, NoTileAttribute)
    

  map1 :: PLCGameMap
  map1 = [[f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
          [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]]

  map2 :: PLCGameMap
  map2 = [[g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
          [b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2]]

  map3 :: PLCGameMap
  map3 = [[g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
          [b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3]]      