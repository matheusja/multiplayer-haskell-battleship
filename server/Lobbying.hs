module Lobbying where
import Data.IORef
import Data.ByteString(ByteString)
import Data.List(uncons)
import qualified Data.Map as M
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString as NS
import qualified Code

import Conversions
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

register :: IO(Code.Type -> Socket -> IO(), Code.Type -> IO (Maybe Socket))
register = do
  register_state <- newIORef M.empty
  let insert_value_in_register code socket = atomicModifyIORef' register_state $ \dict -> (M.insert code socket dict, ())
  let take_value_from_register = atomicModifyIORef' register_state . take_from
  return (insert_value_in_register, take_value_from_register)
  where
    take_from key dict =
      let
        taken_value = M.lookup key dict
        new_dict = remove_from_dict key dict
      in
        (new_dict, taken_value)
    remove_from_dict = M.alter (const Nothing)


lobby_start :: (Code.Type -> IO ()) -> IO Code.Type -> (Code.Type -> Socket -> IO())
 -> (Code.Type -> IO (Maybe Socket)) -> Socket -> IORef [Socket] -> IO ()
lobby_start return_code aloc register_socket take_socket socket owned = do
  msg <- NS.recv socket 1024
  let m = string_from_utf8 msg
  putStrLn $ "Received: " ++ m;
  let firstword = head (words m)
  case firstword of
    "lobby" | length m == length "looby" -> do
      code <- aloc
      putStrLn $ "Generated " ++ Code.code_text code
      NS.sendAll socket $ Code.encode code
      register_socket code socket
      writeIORef owned []
    "join" | length (words m) == 2 -> do
      let code_str = words m !! 1
      putStrLn $ "Recieved request to join lobby \"" ++ code_str ++ "\""
      let code = Code.decode code_str
      case code of
        Nothing -> do
          putStrLn "Could not parse code"
          NS.sendAll socket $ string_to_utf8 "Could not parse code"
      
        Just n -> do
          putStrLn "Parsed code succesfully"
          msocket2 <- take_socket n
          case msocket2 of
            Nothing -> do
              putStrLn "Lobby code does not exist"
              NS.sendAll socket $ string_to_utf8 "Lobby code does not exist"
            Just socket2 -> do
              return_code n
              modifyIORef' owned (socket2:)
              putStrLn $ "Starting game from lobby " ++ show (Code.unwrap n)
              game socket socket2
    _ -> do
      putStrLn "Command not understood"
      NS.sendAll socket $ string_to_utf8 "What?"

game :: Socket -> Socket -> IO ()
game s1 s2 = do
  NS.sendAll s1 $ string_to_utf8 "Ok"
  NS.sendAll s2 $ string_to_utf8 "Ok"