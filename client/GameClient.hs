module GameClient where
import System.IO as I_O

import Game
import Conversions

import qualified Sea
import qualified SeaClient as Sea
import qualified Battleship
import qualified Display

import Network.Socket
import qualified Network.Socket.ByteString as NS
import Data.Maybe

import Control.Monad
import Control.Monad.Trans.Maybe
import qualified SetupClient as Setup
import Data.Char (toLower)
import qualified System.Console.ANSI as Console

{-# ANN module "HLint: ignore Use camelCase" #-}

data Command = Quit | Fire | Move (Pos -> Pos)
type MyState = (Sea.Sea, Sea.Sea)

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

getHumanPlayer = fst

getAIPlayer = snd

after :: (Result, MyState) -> IO ()
after (result, state) = do
  let mySeaDraw = Sea.drawFull $ getHumanPlayer $ state
  let aiSeaDraw = Sea.drawFull $ getAIPlayer    $ state
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

showInstructions :: IO ()
showInstructions = do
  I_O.putStr "Atire onde acha que os navios inimigos estao, utilize as teclas "
  Display.putStrRed "WASD"
  I_O.putStrLn " para posicionar o ponteiro."
  I_O.putStr "Utilize a tecla "
  Display.putStrRed "'E'"
  I_O.putStr " para disparar e "
  Display.putStrRed "'Q'"
  I_O.putStr " para desistir."

parseCommand :: Char -> Maybe Command
parseCommand 'q' = Just Quit
parseCommand 'e' = Just Fire 
parseCommand  c  = Move . Battleship.getDisPlacement <$> Battleship.parseDir c


player_turn :: Socket -> MyState -> Pos -> (Sea.BombResult,Sea.BombResult) -> IO(Maybe (MyState, Pos, Sea.BombResult))
player_turn socket state@(mySea, aiSea) pos bres@(bresYou, bresAI) = do
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
  
  cmd <- parseCommand . toLower <$> I_O.getChar
  case cmd of
    Nothing       -> player_turn socket state pos bres
    Just Quit     -> surrender socket
    Just (Move m) -> do
      let npos = m pos
      if Sea.checkPosBounds npos $ Sea.dims aiSea
      then player_turn socket state npos bres
      else player_turn socket state  pos bres
    Just Fire -> do
      mresult <- Sea.bombPos socket pos aiSea
      let on_sucess_do (nAISea, bombres) = return $ Just ((mySea, nAISea), pos, bombres)
      let else_do = player_turn socket state pos bres
      maybe else_do on_sucess_do mresult

blankEnemySea :: Sea.Sea -> Sea.Sea
blankEnemySea = (map . map) (const (Sea.Pristine, Sea.ShipAbsent))

game s = do
  msg <- string_from_utf8 <$> NS.recv s 1024
  if msg == show RematchRejected then
    putStrLn "Seu oponente não quer jogar outra partida"
  else do
    putStrLn msg
    let [header, size_line, fleetdef_line] = lines msg
    let size = (read size_line :: Sea.Bounds)
    let fleetdef = (read fleetdef_line :: Battleship.FleetDef)
    fleet <- Setup.ships size fleetdef
    let strdef = show fleet
    --putStrLn strdef
    NS.sendAll s $ string_to_utf8 strdef 
    --_ <- getLine

    let sea = Sea.generateRealSea $ Sea.placeFleet fleet $ Sea.setupCreate size
    (result, (mySea, enemySea)) <- run s sea
    Console.clearScreen
    I_O.putStrLn $ reportResult result
    I_O.putStrLn $ Display.withoutCursor $ Sea.drawFull mySea
    I_O.putStrLn ""
    I_O.putStrLn ""
    I_O.putStrLn ""
    I_O.putStrLn $ Display.withoutCursor $ Sea.drawFull enemySea
    I_O.putStrLn "Pressione 'Y' para começar outra partida contra o mesmo oponente - qualquer outro botão para sair"
    c <- toLower <$> I_O.getChar
    if c == 'y' then do
      NS.sendAll s $ string_to_utf8 $ show Rematch
      putStrLn "Esperando por resposta - não aperte em nada"
      game s
    else
      NS.sendAll s $ string_to_utf8 $ show Bye
  

run :: Socket -> Sea.Sea -> IO((Result, MyState))
run socket mySea = loop socket (0,0) (mySea, blankEnemySea mySea) (Sea.Miss, Sea.Miss)

loop :: Socket -> Pos -> MyState -> (Sea.BombResult, Sea.BombResult) -> IO((Result, MyState))
loop socket pos state (bresYou, bresAI) = do
  resultMe <- player_turn socket state pos (bresYou, bresAI)
  let (newState0, pos, nbresYou) = fromMaybe (state, pos, bresYou) resultMe
  (state, nbresMe, ending) <- ai_turn socket newState0
  case ending of
    (Just result) -> return (result, state)
    Nothing -> loop socket pos state (nbresYou, fromJust nbresMe)

surrender socket = do
  NS.sendAll socket $ string_to_utf8 $ show $ Surrender
  return Nothing

ai_turn :: Socket -> MyState -> IO (MyState, Maybe Sea.BombResult, Maybe Result)
ai_turn server state@(mySea, aiSea) = do
  putStrLn "Esperando por resposta - não aperte em nada"
  response <- string_from_utf8 <$> NS.recv server 1024
  let status = read response :: ServerEnemyAttackNotify
  case status of
    (JustAttackedAt pos) -> do
      let (myNewSea, bombResult) = fromMaybe (error "Server sent invalid coordinates") $ Sea.bombPosOwned pos mySea
      let nextstate = (myNewSea, aiSea)
      return (nextstate, Just bombResult, Nothing)
    (JustEnd result) -> do
      return (state, Nothing, Just result)
    (BothAtackAndEnd pos result) -> do
      let (myNewSea, bombResult) = fromMaybe (error "Server sent invalid coordinates") $ Sea.bombPosOwned pos mySea
      let nextstate = (myNewSea, aiSea)
      return (nextstate, Just bombResult, Just result)

reportResult :: Result -> String
reportResult Victory    = "Você ganhou ao destruir a frota inimiga!"
reportResult Defeat     = "Você perdeu ao ter sua frota destruída pelo inimigo!"
reportResult Tie        = "Você empatou ao destruir a frota inimiga e ter sua frota destruída pelo inimigo no mesmo turno!"
reportResult Yield      = "Você perdeu ao desistir do jogo!"
reportResult EnemyYield = "Você ganhou pois o seu oponente desistiu"
reportResult TieYield   = "Você empatou ao desistir do jogo junto com o inimigo!"
