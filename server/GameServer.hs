module GameServer where
import Conversions

import Game

import qualified System.Random as Random

import Network.Socket
import Network.Socket.ByteString as NS

import qualified Data.Maybe as MMaybe
import qualified Data.Bifunctor as Bifunctor

import qualified Battleship
import qualified Sea
import qualified Setup
import qualified Control.Monad

type Player   = (Sea.Sea, Battleship.Fleet)
data Memory   = Hunt | Target Pos [(Dir, Maybe Int)] -- 2 "states"
type PlayerAI = (Player, Memory)
type State    = (Player, PlayerAI)
type Dir      = Battleship.Dir

{-# ANN module "HLint: ignore Use camelCase" #-}

newAI :: Player -> PlayerAI
newAI p = (p, Hunt)

seaTileHasIntactShip :: Sea.SeaTile -> Bool
seaTileHasIntactShip = uncurry (&&) . Bifunctor.bimap (== Sea.Pristine) (/= Sea.ShipAbsent)

check_win :: Sea.Sea -> Bool
check_win = not . any seaTileHasIntactShip . concat

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



game :: Socket -> Socket -> IO ()
game s1 s2 = do
  let bounds = Sea.standardBounds
  let fleetdef = Battleship.standardFleet
  let config = show bounds ++ "\n" ++ show fleetdef 
  NS.sendAll s1 $ string_to_utf8 $ "Setup\n" ++ config 
  NS.sendAll s2 $ string_to_utf8 $ "Setup\n" ++ config
  putStrLn "Recieving player 1"
  rawdata_p1 <- string_from_utf8 <$>  NS.recv s1 1024
  putStrLn "Recieving player 2"
  rawdata_p2 <- string_from_utf8 <$> NS.recv s2 1024
  putStrLn "Recieved both players"
  let data_p1 = read rawdata_p1 :: [Battleship.Inst]
  let data_p2 = read rawdata_p2 :: [Battleship.Inst]
  let base_sea = Sea.setupCreate bounds
  
  print data_p1
  print data_p2



substSeaAI :: PlayerAI -> Sea.Sea -> PlayerAI
substSeaAI aiThings newSea = Bifunctor.first (Bifunctor.first (const newSea)) aiThings

ai_turn :: State -> IO((State, Sea.BombResult))
ai_turn state@(player@(playerSea, _), myData@(_, Hunt)) = do
  let try_again = ai_turn state
  let (mx, my) = Sea.dims playerSea
  x <- Random.randomRIO (0, mx-1)
  y <- Random.randomRIO (0, my-1)
  let pos = (x,y)
  case Sea.bombPosOwned pos playerSea of
    Nothing -> try_again
    Just (newSea, bombRes) -> do
      let playerWithNewSea = Bifunctor.first (const newSea) player
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
      Control.Monad.when (null notTried) $ error "A IA nao sabe mais o que fazer"
      nextTry <- getRandomPos notTried
      let nextPos = Battleship.getDisPlacement nextTry pos
      if Sea.checkPosBounds nextPos $ Sea.dims playerSea then
        case Sea.bombPosOwned nextPos playerSea of
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
        case Sea.bombPosOwned nextPos playerSea of
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
