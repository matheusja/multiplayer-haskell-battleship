module SetupClient where
import System.IO as I_O


import Data.Char
import Data.List


import qualified Battleship
import qualified Sea
import qualified SeaClient as Sea
import qualified Display
import Setup

data Command = Move (Battleship.Inst -> Battleship.Inst) | Place

parseCommand :: Char -> Maybe Command
parseCommand 'e'  = Just  $  Move   Battleship.rotateClockwise
parseCommand 'q'  = Just  $  Move   Battleship.rotateCounterClockwise
parseCommand '\n' = Just  $  Place
parseCommand  x   = fmap (Move . Battleship.move . Battleship.getDisPlacement) $ Battleship.parseDir x


defaultInst :: Battleship.Def -> Battleship.Inst
defaultInst def = Battleship.Inst def (0,0) Battleship.South

ships :: Sea.Bounds -> Battleship.FleetDef -> IO Battleship.Fleet
ships bounds = foldl' joinF (return []) . fmap (placeShipLoop bounds . defaultInst)

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

