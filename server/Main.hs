-- Echo server program
module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.IORef
import Network.Socket
import Network.Socket.ByteString as NS -- (recv, sendAll)
import qualified Lobbying

import qualified Conversions as C

ignoreSecond f a _ = f a

main :: IO ()
main = do
  (return_code, aloc) <- Lobbying.alocator
  (register_socket, take_socket) <- Lobbying.register
  runTCPServer Nothing "3000" $ Lobbying.lobby_start return_code aloc register_socket take_socket


-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IORef [Socket] -> IO a) -> IO a
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
        owned <- newIORef [conn]
        void $ forkFinally (server conn owned) $ const $ do
          owned <- readIORef owned
          mapM_ (`gracefulClose` 5000) owned