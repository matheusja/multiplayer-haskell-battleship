
import qualified System.IO as I_O
import qualified System.Console.ANSI as Console

import qualified Battleship
import qualified Sea
import qualified Game

main = do
  setupTerminal
  gameState <- Game.setup Sea.standardBounds Battleship.standardFleet
  gameResult <- Game.run gameState
  Game.after gameResult
  exit
  
setupTerminal = do
  I_O.hSetBuffering I_O.stdin I_O.NoBuffering
  I_O.hSetEcho      I_O.stdin False
  Console.hideCursor

exit = do
  I_O.hSetEcho      I_O.stdin  True
  Console.showCursor
