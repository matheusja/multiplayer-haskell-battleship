module Lobbying where 
import Data.IORef
import Data.ByteString(ByteString)
import Data.List(uncons)
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString as NS
import qualified Code

import Conversions(string_to_utf8, string_from_utf8)

alocator :: IO (Code.Type -> IO (), IO Code.Type)
alocator = do
  counter_now <- newIORef 0
  unused_list <- newIORef []
  let addUnused x = atomicModifyIORef' unused_list $ (,) <$> (x:) <*> const ()
  let alocate = do {
    v_unused_list <- readIORef unused_list;
    case uncons v_unused_list of
      -- unused_list == []
      Nothing ->
        Code.Type <$> atomicModifyIORef' counter_now ((+1) >>= (,))
      Just (first_element, remainer_elems) -> do
        atomicWriteIORef unused_list remainer_elems
        return first_element
  }
  return (addUnused , alocate)


lobby_start :: (Code.Type -> IO ()) -> IO Code.Type -> Socket -> IO ()
lobby_start return_code aloc socket = do
  msg <- NS.recv socket 1024
  let m = string_from_utf8 msg
  putStrLn $ "Received: " ++ m
  if m == "Lobby" then do 
    E.bracket alocate_code close_code use_code
  else do
    putStrLn "Command not understood"
    NS.sendAll socket $ string_to_utf8 "What?"
  where
    alocate_code = do {
      code <- aloc;
      putStrLn $ "Generated " ++ (Code.code_text code);
      NS.sendAll socket $ Code.encode code;
      newIORef [code]
    }
    close_code codes_ref = do {
      codes <- readIORef codes_ref;
      let code_lines = ["Code is now unused: " ++ Code.code_text code | code <- codes];
      in putStr $ unlines code_lines
    }
    use_code codes_ref = do {
      codes <- readIORef codes_ref;
      putStrLn $ "Code " ++ Code.code_text (codes !! 0) ++ " is in use";
      _ <- getLine; -- evitar sair
      sequence_ $ return_code <$> codes
    }


game :: Socket -> Socket -> IO ()
game s1 s2 = return ()