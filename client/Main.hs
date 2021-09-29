-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import Network.Socket
import qualified Network.Socket.ByteString as NS
import qualified Conversions as C
import System.Environment
import Data.Maybe
import Data.List

import qualified System.IO as I_O
import qualified System.Console.ANSI as Console

import Game
import qualified Sea
import qualified SetupClient as Setup
import qualified Battleship
import Control.Monad
import GameClient
import Sea (placeFleet)
{-# ANN module "HLint: ignore Use camelCase" #-}

start = do
  I_O.hSetBuffering I_O.stdin I_O.NoBuffering
  I_O.hSetEcho      I_O.stdin False
  Console.hideCursor

exit = do
  I_O.hSetEcho      I_O.stdin  True
  Console.showCursor


parse_args args = case args of
  ["help"] -> Nothing
  [ip] -> Just (ip, "3000", Lobby)
  [ip, port] -> Just (ip, port, Lobby)
  [ip, port, lobby] -> Just (ip, port, Join $ read lobby)
  [] -> Just ("127.0.0.1", "3000", Lobby)
  _ -> Nothing 
   
print_usage = do
  putStrLn "USAGE:"
  putStrLn "<program> <ip> <port> <lobby code>; will join lobby assigned to code"
  putStrLn "<program> <ip> <port>; will create lobby"
  putStrLn "<program> <ip>; port = 3000"
  putStrLn "<program>; ip = 127.0.0.1"
  
main :: IO ()
main = do
  args <- getArgs
  case parse_args args of
    Nothing -> print_usage
    Just (add, port, message) -> E.bracket_ start exit (connection add port message)

  
  
connection add port message = runTCPClient add port $ \s -> (do
  NS.sendAll s $ C.string_to_utf8 $ show message
  when (message == Lobby) (do 
    msg <- C.string_from_utf8 <$> NS.recv s 1024
    putStrLn $ "Received:\n" ++ msg
    )
  game s

  return ()
  )

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
