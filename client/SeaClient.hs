module SeaClient where
import Sea

import Control.Monad

import Network.Socket
import Network.Socket.ByteString as NS

import Conversions

import qualified Battleship


import Data.List (foldl')
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

bombPos :: Socket -> Pos -> Sea -> IO(Maybe (Sea, BombResult))
bombPos s pos sea = do
  NS.sendAll s $ string_to_utf8 $ "fire " ++ show pos
  msg <- string_from_utf8 <$> NS.recv s 1024
  let result = read msg :: Maybe BombResult
  return $ liftM2 (,) (updateSea pos sea) id <$> result

updateSea :: Pos -> Sea -> BombResult -> Sea
updateSea pos sea result
  = case result of
    (Hit x) -> there_is_ship x
    (ShipDestroyed x) -> there_is_ship x
    (Miss) -> there_is_no_ship
  where
    there_is_ship :: Battleship.Inst -> Sea
    there_is_ship x = (uncurry uPos) pos (Bombed, ShipPresent x) sea
    there_is_no_ship :: Sea
    there_is_no_ship = (uncurry uPos) pos (Pristine, ShipAbsent) sea