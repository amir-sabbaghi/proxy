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

import OpenSSL
import OpenSSL.Session

type Send = ByteString -> IO ()
type Recv = IO ByteString
type Worker = Send -> Recv -> IO ()

data Settings = Settings { bindAddress :: String
                         , port :: String
                         , bufferSize :: Int
                         , https :: Bool
                         , cert :: String
                         , key :: String
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port = "3000"
                           , bufferSize = 2^18
                           , https = False
                           , cert = "cert.pem"
                           , key = "key.pem"
                           }

server :: Settings -> Worker -> IO ()
server (Settings bindAddr port bufferSize True cert key) worker = withOpenSSL $ do

  ctx <- context
  contextSetPrivateKeyFile ctx key
  contextSetCertificateFile ctx cert

  result <- contextCheckPrivateKey ctx
  if not result then
    Prelude.putStrLn "Private key does not match the certificate"
  else
    do
      print $ "[HTTPS Proxy] Listening on port " ++ port

      serve HostAny port $ \(socket, remoteAddr) -> do
        ssl <- connection ctx socket
        OpenSSL.Session.accept ssl

        let reliableWrite bs = write ssl bs `catch` \(e :: ProtocolError) -> reliableWrite bs
            reliableRead = read ssl bufferSize `catch` \(e :: ProtocolError) -> reliableRead

        worker reliableWrite reliableRead `catch` \e -> print (e :: SomeException)

        OpenSSL.Session.shutdown ssl Bidirectional

      return ()

server (Settings bindAddr port bufferSize False _ _) worker =
  do
    print $ "Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      let write = send socket
          rcv = do
            d <- Network.Simple.TCP.recv socket bufferSize
            return $ fromMaybe empty d 

      worker write rcv `catch` \e -> print (e :: SomeException)

    return ()
