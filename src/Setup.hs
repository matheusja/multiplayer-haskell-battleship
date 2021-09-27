module Setup where
import qualified System.IO as I_O
import qualified System.Console.ANSI as Console

import qualified Battleship
import qualified Sea

import Data.List (foldl')
import Data.Bifunctor (first, second)
import Data.Char (toLower)
import System.Random


type Dir = Battleship.Dir
type Pos = Sea.Pos

tryPlace :: Sea.Bounds -> Battleship.Inst -> [Battleship.Inst] -> Maybe Battleship.Inst
tryPlace bounds shipinst previous =
  let intersects_with_other_ships = any (Battleship.checkIntersection shipinst) previous
      inside_bounds = Sea.checkBounds shipinst bounds
  in if inside_bounds && (not intersects_with_other_ships) then
    Just shipinst
  else
    Nothing


randomDir :: IO(Dir)
randomDir = do
  n <- randomRIO (0,1)
  return $ dir n
  where
    dir :: Int -> Dir
    dir 0 = Battleship.South
    dir _ = Battleship.East
    -- Battleship.South ~ Battleship.North
    -- Battleship.East  ~ Battleship.West
    -- espelhado

randomPos :: Battleship.Def -> Sea.Bounds -> IO(Battleship.Inst)
randomPos bts@(Battleship.Def _ size) (xM, yM) = do
  dir <- randomDir    
  let (decx, decy) = if dir == Battleship.South then (0, size) else (size, 0)
  x <- randomRIO (0, xM-decx) -- evitar colocar elemento fora da grade 
  y <- randomRIO (0, yM-decy) 
  return (Battleship.Inst bts (x,y) dir)

shipsAI :: Sea.Bounds -> Battleship.FleetDef -> IO(Battleship.Fleet)
shipsAI bounds = foldl' joinF (return []) . fmap (placeShipAI bounds)

joinF :: IO [Battleship.Inst] -> ([Battleship.Inst] -> IO Battleship.Inst) -> IO [Battleship.Inst]
joinF mli f = do
  li <- mli
  i <- f li
  joinF (return (i:li)) f

placeShipAI :: Sea.Bounds -> Battleship.Def -> [Battleship.Inst] -> IO(Battleship.Inst)
placeShipAI bounds def previous = do
  let tryAgain = placeShipAI bounds def previous
  ship <- randomPos def bounds
  maybe tryAgain return $ tryPlace bounds ship previous



