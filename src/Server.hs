module Server ( Send
              , Recv
              , Worker
              , Settings (..)
              , defaultSettings
              , server
              ) where

import Data.ByteString
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (forever)
import Control.Concurrent

type Send = ByteString -> IO ()
type Recv = IO ByteString
type Worker = Send -> Recv -> IO ()

data Settings = Settings { bindAddress :: String
                         , port :: PortNumber
                         , bufferSize :: Int
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port = 3000
                           , bufferSize = 2^18
                           }

server :: Settings -> Worker -> IO ()
server (Settings bindAddr port bufferSize) worker = do
  addr <- inet_addr bindAddr
  s <- socket AF_INET Stream defaultProtocol
  setSocketOption s ReuseAddr 1
  bind s $ SockAddrInet port addr
  listen s 5
  forever $ do
    (c, remote) <- accept s
    forkIO $ do
      let snd = sendAll c
      let rcv = recv c bufferSize
      worker snd rcv
      close c
