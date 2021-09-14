-- Echo server program
module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString as NS -- (recv, sendAll)
import qualified Lobbying

import qualified Conversions as C

lobby_start :: IO Lobbying.Code -> Socket -> IO ()
lobby_start aloc socket = do
  msg <- NS.recv socket 1024
  let m = C.string_from_utf8 msg
  putStrLn $ "Received: " ++ m
  if m == "Lobby" then do
    code <- aloc
    putStrLn $ "Generated " ++ (Lobbying.code_text code)
    NS.sendAll socket $ Lobbying.encode code
  else do
    putStrLn "Command not understood"
    NS.sendAll socket $ C.string_to_utf8 "What?"


main :: IO ()
main = do
  aloc <- Lobbying.alocator 
  runTCPServer Nothing "3000" (lobby_start aloc)


-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
