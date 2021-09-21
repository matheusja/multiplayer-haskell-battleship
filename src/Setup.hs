module Setup where
import qualified System.IO as I_O
import qualified System.Console.ANSI as Console

import qualified Battleship
import qualified Sea
import qualified Display

import Data.List (foldl')
import Data.Bifunctor (first, second)
import Data.Char (toLower)
import System.Random


type Dir = Battleship.Dir
type Pos = Sea.Pos

defaultInst :: Battleship.Def -> Battleship.Inst
defaultInst def = Battleship.Inst def (0,0) Battleship.South

ships :: Sea.Bounds -> Battleship.FleetDef -> IO (Battleship.Fleet)
ships bounds = foldl' joinF (return []) . fmap (placeShipLoop bounds . defaultInst)

joinF :: Monad m => m [a] -> ([a] -> m a) -> m [a] -- talvez eu pudesse fazer isso com uma composição maluca
joinF ml f = do
  l <- ml      -- l :: [a]
  r <- f l     -- r ::  a
  return (r:l) -- (r:l) :: [a]

-- joinF :: [a] -> ([a] -> m a) -> m [a]
-- >>=s

data Command = Move (Battleship.Inst -> Battleship.Inst) | Place

parseCommand :: Char -> Maybe Command
parseCommand 'e'  = Just  $  Move   Battleship.rotateClockwise
parseCommand 'q'  = Just  $  Move   Battleship.rotateCounterClockwise
parseCommand '\n' = Just  $  Place
parseCommand  x   = fmap (Move . Battleship.move . Battleship.getDisPlacement) $ Battleship.parseDir x


placeShipLoop :: Sea.Bounds -> Battleship.Inst -> [Battleship.Inst] -> IO Battleship.Inst
placeShipLoop bounds shipinst@(Battleship.Inst (Battleship.Def name size) pos dir) previous = do
  Display.resetScreen
  I_O.putStrLn $ "Posicione o " ++ name ++ "!"
  
  I_O.putStr "Utilize as teclas " 
  Display.putStrRed "WASD"
  I_O.putStr " para deslocá-lo e as teclas "
  Display.putStrRed "QE"
  I_O.putStrLn " para rotacioná-lo."
  
  I_O.putStr "Aperte a tecla "
  Display.putStrRed "Enter"
  I_O.putStrLn " para fixá-lo"
  
  let segments = Battleship.getSegments shipinst
  let previousSegments = concat $ fmap Battleship.getSegments $ previous
  let intersects_with_other_ships = any (Battleship.checkIntersection shipinst) previous
  if intersects_with_other_ships then
    Display.putStrRed "Navios não podem sobrepor uns aos outros!\n"
  else putStrLn ""
  putStrLn ""
  let seaMap = Sea.placeFleet (shipinst:previous) $ Sea.setupCreate bounds
  
  I_O.putStr $ Display.withoutCursor $ Sea.drawSetup seaMap
  
  
  cmd <- fmap (parseCommand . toLower) $ I_O.getChar 
  
  let tryAgain = placeShipLoop bounds shipinst previous
  
  case cmd of
    Nothing -> tryAgain
    Just(Place) ->
      maybe tryAgain return (tryPlace bounds shipinst previous)
    Just(Move m) -> do
      let newship = m shipinst
      if Sea.checkBounds newship bounds then  -- nao me compliquem, pfv
        placeShipLoop bounds newship previous
      else
        tryAgain

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

placeShipAI :: Sea.Bounds -> Battleship.Def -> [Battleship.Inst] -> IO(Battleship.Inst)
placeShipAI bounds def previous = do
  let tryAgain = placeShipAI bounds def previous
  ship <- randomPos def bounds
  maybe tryAgain return $ tryPlace bounds ship previous



