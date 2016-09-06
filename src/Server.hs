{-# LANGUAGE ScopedTypeVariables #-}
module Server ( Send
              , Recv
              , Worker
              , Settings (..)
              , defaultSettings
              , server
              ) where

import Prelude hiding (read)
import Data.ByteString
import Data.Maybe
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (forever)
import Control.Concurrent
import Control.Exception
import Network.Simple.TCP

type Send = ByteString -> IO ()
type Recv = IO ByteString
type Worker = Send -> Recv -> IO ()

data Settings = Settings { bindAddress :: String
                         , port :: String
                         , bufferSize :: Int
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port = "3000"
                           , bufferSize = 2^18
                           }

server :: Settings -> Worker -> IO ()
server (Settings bindAddr port bufferSize) worker = do
  do
    print $ "Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      let write = send socket
          rcv = do
            d <- Network.Simple.TCP.recv socket bufferSize
            return $ fromMaybe empty d 

      worker write rcv `catch` \e -> print (e :: SomeException)

    return ()
