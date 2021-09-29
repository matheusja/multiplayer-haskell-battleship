module SeaClient where
import Sea

import Control.Monad

import Network.Socket
import Network.Socket.ByteString as NS

import Conversions

import qualified Battleship


import Data.List (foldl')
import Game
-- ~  ~  ~ [ ] ;  #  #  ~  ~  ~  ~ 
drawFull :: Sea -> [[Char]]
drawFull = (fmap . fmap) convertTile
  where
    convertTile :: SeaTile -> Char
    convertTile (Pristine, ShipPresent _) = '#'
    convertTile (Bombed  , ShipPresent _) = ';'
    convertTile (Pristine, ShipAbsent)    = '~'
    convertTile (Bombed  , ShipAbsent)    = ' '

drawSetup :: Setup -> [[Char]]
drawSetup = drawFull . (fmap . fmap) ((,) Pristine)

hideShips :: [[Char]] -> [[Char]]
hideShips = (map . map) hideTile
  where
    hideTile '#' = '~'
    hideTile  c  =  c

bombPos :: Socket -> Sea.Pos -> Sea -> IO(Maybe(Sea, BombResult))
bombPos s pos sea =
  if fst(uncurry gPos pos sea) == Pristine then do
    putStrLn "Esperando por resposta - não aperte em nada"
    NS.sendAll s $ string_to_utf8 $ show $ Attack pos
    msg <- string_from_utf8 <$> NS.recv s 1024
    print msg
    NS.sendAll s $ string_to_utf8 $ show $ ClientACK
    let result = read msg :: ServerAttackNotify
    return $ Just $ liftM2 (,) (updateSea pos sea) id $ convertResponse result
  else
    return Nothing


convertResponse :: ServerAttackNotify -> BombResult
convertResponse resp = case resp of
  Game.Miss -> Sea.Miss
  Game.Hit -> Sea.Hit Battleship.dummyShip
  (Game.Sunk shipname) -> Sea.ShipDestroyed $ Battleship.onlyName shipname

updateSea :: Sea.Pos -> Sea -> BombResult -> Sea
updateSea pos sea result
  = case result of
    (Sea.Hit x) -> there_is_ship x
    (Sea.ShipDestroyed x) -> there_is_ship x
    (Sea.Miss) -> there_is_no_ship
  where
    there_is_ship :: Battleship.Inst -> Sea
    there_is_ship x = (uncurry uPos) pos (Bombed, ShipPresent x) sea
    there_is_no_ship :: Sea
    there_is_no_ship = (uncurry uPos) pos (Bombed, ShipAbsent) sea