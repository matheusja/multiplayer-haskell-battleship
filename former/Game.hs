module Game where

import System.IO as I_O
import qualified System.Random as Random
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as MMaybe

import qualified Battleship
import qualified Sea
import qualified Display
import qualified Setup

type Pos      = Sea.Pos
type Player   = (Sea.Sea, Battleship.Fleet)
type PlayerAI = (Player, Memory)
data Result   = Victory | Defeat | Tie | Yield
data Memory   = Hunt | Target Pos [(Dir, Maybe Int)] -- 2 "states"
type State    = (Player, PlayerAI)
type Dir      = Battleship.Dir

newAI :: Player -> PlayerAI
newAI p = (p, Hunt)

getAIPlayer :: State -> Player
getAIPlayer = fst . snd

getHumanPlayer :: State -> Player
getHumanPlayer = fst

getPlayerSea :: Player -> Sea.Sea
getPlayerSea = fst

setup :: Sea.Bounds -> Battleship.FleetDef -> IO (State)
setup bounds fleetDef = do
  fleetPlayer <- Setup.ships   bounds fleetDef
  fleetAI     <- Setup.shipsAI bounds fleetDef
  let seaPlayer = Sea.generateRealSea $ Sea.placeFleet fleetPlayer $ Sea.setupCreate bounds
  let seaAI     = Sea.generateRealSea $ Sea.placeFleet fleetAI     $ Sea.setupCreate bounds
  return ((seaPlayer, fleetPlayer), newAI (seaAI, fleetAI))


reportBombingAI :: Sea.BombResult -> String
reportBombingAI (Sea.Hit (Battleship.Inst (Battleship.Def name _) _ _)) =
  "O inimigo acertou seu " ++ name ++ " na rodada anterior!"
reportBombingAI (Sea.ShipDestroyed (Battleship.Inst (Battleship.Def name _) _ _)) =
  "O inimigo afundou seu " ++ name ++ " na rodada anterior!"
reportBombingAI Sea.Miss =
  "O inimigo não atingiu sua frota na rodada anterior!"

reportBombingYou :: Sea.BombResult -> String
reportBombingYou (Sea.Hit (Battleship.Inst (Battleship.Def name _) _ _)) =
  "Você acertou um navio inimigo na rodada anterior!"
reportBombingYou (Sea.ShipDestroyed (Battleship.Inst (Battleship.Def name _) _ _)) =
  "Você afundou o " ++ name ++ " inimigo na rodada anterior!"
reportBombingYou Sea.Miss =
  "Você não atingiu a frota inimiga na rodada anterior!"

run :: State -> IO((Result, State))
run gameState = loop (0,0) gameState (Sea.Miss, Sea.Miss)

reportResult :: Result -> String
reportResult Victory = "Você ganhou ao destruir a frota inimiga!"
reportResult Defeat  = "Você perdeu ao ter sua frota destruída pelo inimigo!"
reportResult Tie     = "Você empatou ao destruir a frota inimiga e ter sua frota destruída pelo inimigo no mesmo turno!"
reportResult Yield   = "Você perdeu ao desistir do jogo!"




after :: (Result, State) -> IO ()
after (result, state) = do
  let mySeaDraw = Sea.drawFull $ getPlayerSea $ getHumanPlayer $ state
  let aiSeaDraw = Sea.drawFull $ getPlayerSea $ getAIPlayer    $ state
  Display.resetScreen
  I_O.putStrLn $ reportResult result
  I_O.putStrLn ""
  I_O.putStrLn ""
  I_O.putStrLn ""
  I_O.putStrLn ""
  
  I_O.putStrLn $ Display.withoutCursor aiSeaDraw
  
  I_O.putStrLn ""
  I_O.putStrLn ""
  I_O.putStrLn ""
  
  I_O.putStrLn $ Display.withoutCursor mySeaDraw
  
  

loop :: Pos -> State -> (Sea.BombResult, Sea.BombResult) -> IO((Result, State))
loop pos state (bresYou, bresAI)  = do
  result <- player_turn state pos (bresYou, bresAI)
  case result of
    Nothing -> return (Yield, state)
    Just (newState0, pos, nbresYou) -> do
      (newState1, nbresAI) <- ai_turn newState0
      case (check_player_win newState1, check_ai_win newState1) of
        (True , True ) -> return (Tie, newState1)
        (False, True ) -> return (Defeat, newState1)
        (True , False) -> return (Victory, newState1)
        (False, False) -> loop pos newState1 (nbresYou, nbresAI)

data Command = Move (Pos -> Pos) | Quit | Fire
 
parseCommand :: Char -> Maybe Command
parseCommand '\n' = Just Fire
parseCommand  'q' = Just Quit
parseCommand   x  = fmap (Move . Battleship.getDisPlacement) $ Battleship.parseDir x

check_player_win :: State -> Bool
check_player_win = check_win . getPlayerSea . getAIPlayer

check_ai_win :: State -> Bool
check_ai_win     = check_win . getPlayerSea . getHumanPlayer 

seaTileHasIntactShip :: Sea.SeaTile -> Bool
seaTileHasIntactShip = uncurry (&&) . Bifunctor.bimap (== Sea.Pristine) (/= Sea.ShipAbsent)

check_win :: Sea.Sea -> Bool
check_win = not . any seaTileHasIntactShip . concat

showInstructions :: IO ()
showInstructions = do
  I_O.putStr "Atire onde acha que os navios inimigos estao, utilize as teclas "
  Display.putStrRed "WASD"
  I_O.putStrLn " para posicionar o ponteiro."
  I_O.putStr "Utilize a tecla "
  Display.putStrRed "Enter"
  I_O.putStrLn " para disparar."
  

player_turn :: State -> Pos -> (Sea.BombResult,Sea.BombResult) -> IO(Maybe (State, Pos, Sea.BombResult))
player_turn state@(myThings@(mySea, _), aiThings@((aiSea, _),_)) pos bres@(bresYou, bresAI) = do
  let mySeaDraw =                 Sea.drawFull mySea
  let aiSeaDraw = Sea.hideShips $ Sea.drawFull aiSea
  Display.resetScreen
  showInstructions
  I_O.putStrLn $ reportBombingAI  bresAI
  I_O.putStrLn $ reportBombingYou bresYou
  I_O.putStrLn ""
  I_O.putStrLn $ Display.withCursor pos aiSeaDraw
  
  I_O.putStrLn ""
  I_O.putStrLn ""
  I_O.putStrLn ""
  
  I_O.putStrLn $ Display.withoutCursor  mySeaDraw
  
  cmd <- fmap parseCommand I_O.getChar
  case cmd of
    Nothing       -> player_turn state    pos  bres
    Just Quit     -> return Nothing
    Just (Move m) -> do
      let npos = m pos
      if Sea.checkPosBounds npos $ Sea.dims aiSea
      then player_turn state npos bres
      else player_turn state  pos bres
    Just Fire     -> do
      case Sea.bombPos pos aiSea of
        Nothing -> player_turn state    pos  bres
        Just (nAISea, bombres) -> return $ Just ((myThings, substSeaAI aiThings nAISea), pos, bombres) 

substSeaAI :: PlayerAI -> Sea.Sea -> PlayerAI
substSeaAI aiThings newSea = Bifunctor.first (Bifunctor.first (\_ -> newSea)) aiThings
  
ai_turn :: State -> IO((State, Sea.BombResult))
ai_turn state@(player@(playerSea, _), myData@(_, Hunt)) = do
  let try_again = ai_turn state
  let (mx, my) = Sea.dims playerSea
  x <- Random.randomRIO (0, mx-1)
  y <- Random.randomRIO (0, my-1)
  let pos = (x,y)
  case Sea.bombPos pos playerSea of
    Nothing -> try_again
    Just (newSea, bombRes) -> do
      let playerWithNewSea = Bifunctor.first (\_ -> newSea) player
      case bombRes of
        Sea.Hit _ -> do
          let myNewData = Bifunctor.second (\_ -> Target pos []) myData
          return ((playerWithNewSea, myNewData), bombRes) 
        _     -> return ((playerWithNewSea, myData), bombRes)

ai_turn state@(player@(playerSea, _), myData@(_, Target pos tried)) = do
  let notFailed = MMaybe.mapMaybe extendMaybe tried
  case tryHead notFailed of
    Nothing -> do
      let dirTried = fmap fst tried
      let notTried = filter (not . (`elem` dirTried)) Battleship.directions
      if null notTried then error "A IA nao sabe mais o que fazer" else return () 
      nextTry <- getRandomPos notTried
      let nextPos = (Battleship.getDisPlacement nextTry) pos
      if Sea.checkPosBounds nextPos $ Sea.dims playerSea then
        case Sea.bombPos nextPos playerSea of
          Nothing -> do
            case snd $ uncurry Sea.gPos nextPos playerSea of
              Sea.ShipAbsent -> ai_turn (player, (fst myData, Target pos $ (nextTry, Nothing):tried))
              Sea.ShipPresent _ -> ai_turn (player, (fst myData, Target pos $ (nextTry, Just 1):tried))
          Just (newSea, bombRes) -> do
            case bombRes of
              Sea.Miss -> do
                return (((newSea, snd player), (fst myData, Target pos $ (nextTry, Nothing):tried)), bombRes)
              Sea.Hit _ -> do
                return (((newSea, snd player), (fst myData, Target pos $ (nextTry, Just 1):tried)), bombRes)
              Sea.ShipDestroyed _ -> do
                return (((newSea, snd player), (fst myData, Hunt)), bombRes)
        else ai_turn (player, (fst myData, Target pos $ (nextTry, Nothing):tried))
            
    Just (dir, dist) -> do
      let nextPos = applyNtimes (Battleship.getDisPlacement dir) (dist + 1) pos
      if Sea.checkPosBounds nextPos $ Sea.dims playerSea then
        case Sea.bombPos nextPos playerSea of
          Nothing -> do 
            case snd $ uncurry Sea.gPos nextPos playerSea of
              Sea.ShipAbsent -> ai_turn (player, (fst myData, Target pos $ updateByIndex dir (\_ -> Nothing) tried))
              Sea.ShipPresent _ -> ai_turn (player, (fst myData, Target pos $ updateByIndex dir (fmap (+1)) tried))
          Just (newSea, bombRes) -> do
            case bombRes of
              Sea.Miss -> do
                return (((newSea, snd player), (fst myData, Target pos $ updateByIndex dir (\_ -> Nothing) tried)), bombRes)
              Sea.Hit _ -> do
                return (((newSea, snd player), (fst myData, Target pos $ updateByIndex dir (fmap (+1)) tried)), bombRes)
              Sea.ShipDestroyed _ -> do
                return (((newSea, snd player), (fst myData, Hunt)), bombRes)
      else ai_turn (player, (fst myData, Target pos $ updateByIndex dir (\_ -> Nothing) tried))

guard :: (a -> Bool) -> a -> Maybe a
guard f = MMaybe.listToMaybe . filter f . MMaybe.maybeToList . Just

extendMaybe :: (a, Maybe b) -> Maybe (a,b)
extendMaybe (a, x) = fmap ((,) a) x
{-
extendMaybe :: (a, Maybe b) -> Maybe (a,b)
extendMaybe (a, Nothing) = Nothing
extendMaybe (a, Just b) = Just (a,b)
-}

applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f n a = (iterate f a) !! n

updateByIndex :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateByIndex i f [    ] = []
updateByIndex i f (x:xs)
  | i == fst x = (fmap f x):updateByIndex i f xs
  | otherwise  =         x :updateByIndex i f xs
       
    
getRandomPos :: [a] -> IO a
getRandomPos [ ] = error "Empty choice list"
getRandomPos  l  = do
  let len = length l
  c <- Random.randomRIO (0, len - 1)
  return (l !! c)



tryHead :: [a] -> Maybe a
tryHead [    ] = Nothing
tryHead (x:xs) = Just x
    


