-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import Network.Socket
import qualified Network.Socket.ByteString as NS
import qualified Conversions as C


main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    NS.sendAll s $ C.string_to_utf8 "Lobby"
    _ <- getLine -- evitar sair
    msg <- NS.recv s 1024
    putStrLn $ "Received: " ++ (C.string_from_utf8 msg) 
    return ()

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
