module Lobbying where 
import Data.IORef
import Data.ByteString(ByteString)
import Data.List(uncons)
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString as NS
import qualified Code

import Conversions(string_to_utf8, string_from_utf8)
import Data.Bifunctor (first, second)
{-# ANN module "HLint: ignore Use camelCase" #-}
alocator :: IO (Code.Type -> IO (), IO Code.Type)
alocator = do
  generator_state <- newIORef ([], 0)
  let addUnused = atomicModifyIORef' generator_state . add_value . Code.unwrap
  let alocate = Code.Type <$> atomicModifyIORef' generator_state extract_value
  return (addUnused, alocate)
  where
    add_value value (unused_list, next_value) =
      ((value:unused_list, next_value), ())
    extract_value (unused_list, next_value) =
      case uncons unused_list of
        -- unused_list == []
        Nothing ->
          ((unused_list, next_value+1), next_value)
        Just (first_element, remainer_elems) -> do
          ((remainer_elems, next_value), first_element)




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