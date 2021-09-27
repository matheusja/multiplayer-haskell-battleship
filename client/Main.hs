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
  [ip] -> Just (ip, "3000", "lobby")
  [ip, port] -> Just (ip, port, "lobby")
  [ip, port, lobby] -> Just (ip, port, "join " ++ lobby)
  [] -> Just ("127.0.0.1", "3000", "lobby")
  _ -> Nothing 
   
print_usage = do
  putStrLn "USAGE:"
  putStrLn "<program> <ip> <port> <lobby code>; will join lobby assigned to code"
  putStrLn "<program> <ip> <port>; will create lobby"
  putStrLn "<program> <ip>; port = 3000"
  putStrLn "<program>; "
  
main :: IO ()
main = do
  args <- getArgs
  case parse_args args of
    Nothing -> print_usage
    Just (add, port, message) -> E.bracket_ start exit (connection add port message)

  
  
connection add port message = runTCPClient add port $ \s -> (do
  NS.sendAll s $ C.string_to_utf8 message
  msg <- NS.recv s 1024
  putStrLn $ "Received: " ++ C.string_from_utf8 msg
  _ <- getLine -- evitar sair
  msg <- NS.recv s 1024
  putStrLn $ "Received: " ++ C.string_from_utf8 msg
  _ <- getLine -- evitar sair
  return ())

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
