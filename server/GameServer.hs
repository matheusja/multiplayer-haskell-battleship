module GameServer where
import Conversions

import Game

import qualified System.Random as Random

import Network.Socket
import Network.Socket.ByteString as NS

import Data.Maybe
import qualified Data.Bifunctor as Bifunctor

import qualified Battleship
import qualified Sea
import qualified Setup
import qualified Control.Monad
import Control.Monad

type Player   = (Sea.Sea, Battleship.Fleet)
type Dir      = Battleship.Dir

{-# ANN module "HLint: ignore Use camelCase" #-}

seaTileHasIntactShip :: Sea.SeaTile -> Bool
seaTileHasIntactShip = uncurry (&&) . Bifunctor.bimap (== Sea.Pristine) (/= Sea.ShipAbsent)

check_lose :: Sea.Sea -> Bool
check_lose = not . any seaTileHasIntactShip . concat


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
  
  let sea_p1 = Sea.generateRealSea $ fromMaybe (error "Invalid fleet from player 1") $ Sea.tryPlaceFleet data_p1 base_sea
  let sea_p2 = Sea.generateRealSea $ fromMaybe (error "Invalid fleet from player 2") $ Sea.tryPlaceFleet data_p2 base_sea
  loop s1 s2 sea_p1 sea_p2
  putStrLn "Game over, rematch?"
  r1 <- read . string_from_utf8 <$> NS.recv s1 1024 :: IO PostGameActions
  putStrLn $ show r1 ++ " from p1"
  r2 <- read . string_from_utf8 <$> NS.recv s2 1024 :: IO PostGameActions
  putStrLn $ show r2 ++ " from p2"
  let rejected = string_to_utf8 $ show RematchRejected
  case (r1, r2) of
    (Rematch, Rematch) -> game s1 s2
    (Rematch, Bye) -> NS.sendAll s1 rejected
    (Bye, Rematch) -> NS.sendAll s2 rejected
    (Bye, Bye) -> return ()
    


loop :: Socket -> Socket -> Sea.Sea -> Sea.Sea -> IO ()
loop s1 s2 sea1 sea2 = do
  rawdata_p1 <- string_from_utf8 <$> NS.recv s1 1024
  rawdata_p2 <- string_from_utf8 <$> NS.recv s2 1024
  putStrLn rawdata_p1
  putStrLn rawdata_p2
  let data_p1 = read rawdata_p1 :: ClientTurnDecision
  let data_p2 = read rawdata_p2 :: ClientTurnDecision
  case (data_p1, data_p2) of
    (Surrender, Surrender) -> bothSurrendered s1 s2
    (Surrender, Attack p2) -> firstSurrendered s1 s2 sea1 sea2 p2
    (Attack p1, Surrender) -> secondSurrendered s1 s2 sea1 sea2 p1
    (Attack p1, Attack p2) -> do
      let (newSea2, status1) = fromMaybe (error "Player 1 bombed invalid position") $ Sea.bombPosOwned p1 sea2
      let (newSea1, status2) = fromMaybe (error "Player 2 bombed invalid position") $ Sea.bombPosOwned p2 sea1
      let status_attack1 = show $ convert_report status1
      let status_attack2 = show $ convert_report status2
      -- Notify attack authors
      NS.sendAll s1 $ string_to_utf8 $ status_attack1
      NS.sendAll s2 $ string_to_utf8 $ status_attack2

      ok_p1 <- read . string_from_utf8 <$> NS.recv s1 1024 :: IO ClientACK
      ok_p2 <- read . string_from_utf8 <$> NS.recv s2 1024 :: IO ClientACK
      
      case (check_lose newSea1, check_lose newSea2) of
        (True , True ) -> bothLost s1 s2 p1 p2
        (False, True ) -> firstLost s1 s2 p2
        (True , False) -> secondLost s1 s2 p1
        (False, False) -> do -- carry on
          let status_attacked1 = string_to_utf8 $ show $ JustAttackedAt p1
          let status_attacked2 = string_to_utf8 $ show $ JustAttackedAt p2
          -- Notify attack recievers
          NS.sendAll s2 status_attacked1
          NS.sendAll s1 status_attacked2
          -- Next turn
          loop s1 s2 newSea1 newSea2

firstSurrendered :: Socket -> Socket -> Sea.Sea -> Sea.Sea -> Sea.Pos-> IO ()
firstSurrendered s1 s2 sea1 sea2 p2 = do
  --let status_attack1 = string_to_utf8 $ show $ Yielded
  let (newSea1, status2) = fromMaybe (error "Winning player bombed invalid position") $ Sea.bombPosOwned p2 sea1
  let status_attack2 = string_to_utf8 $ show $ convert_report status2
  --NS.sendAll s1 status_attack1
  NS.sendAll s2 status_attack2
  ok_p2 <- read . string_from_utf8 <$> NS.recv s2 1024 :: IO ClientACK
  
  let result1 = string_to_utf8 $ show $ JustEnd Yield
  let result2 = string_to_utf8 $ show $ JustEnd EnemyYield
  NS.sendAll s1 result1
  NS.sendAll s2 result2

secondSurrendered s1 s2 sea1 sea2 p1 = firstSurrendered s2 s1 sea2 sea1 p1

bothSurrendered :: Socket -> Socket -> IO ()
bothSurrendered s1 s2 = do
  --let status_attack1 = string_to_utf8 $ show $ Yielded
  --let status_attack2 = string_to_utf8 $ show $ Yielded
  --NS.sendAll s1 status_attack1
  --NS.sendAll s2 status_attack2 
  let result = string_to_utf8 $ show $ JustEnd TieYield
  NS.sendAll s1 result
  NS.sendAll s2 result 

bothLost :: Socket -> Socket -> Sea.Pos -> Sea.Pos -> IO ()
bothLost s1 s2 p1 p2 = do
  let result1 = string_to_utf8 $ show $ BothAtackAndEnd p2 Tie
  let result2 = string_to_utf8 $ show $ BothAtackAndEnd p1 Tie
  NS.sendAll s1 result1
  NS.sendAll s2 result2

firstLost :: Socket -> Socket -> Sea.Pos -> IO ()
firstLost s1 s2 p2 = do
  let result1 = string_to_utf8 $ show $ JustEnd Victory
  let result2 = string_to_utf8 $ show $ BothAtackAndEnd p2 Defeat
  NS.sendAll s1 result1
  NS.sendAll s2 result2

secondLost :: Socket -> Socket -> Sea.Pos -> IO ()
secondLost s1 s2 p1 = firstLost s2 s1 p1 


convert_report :: Sea.BombResult -> ServerAttackNotify
convert_report bombres = case bombres of
  (Sea.Hit _) -> Hit
  (Sea.ShipDestroyed ship) -> Sunk (Battleship.nameInst ship)
  (Sea.Miss) -> Miss