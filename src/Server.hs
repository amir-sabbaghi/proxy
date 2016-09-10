{-# LANGUAGE ScopedTypeVariables #-}
module Server ( Send
              , Recv
              , Worker
              , ServerSettings (..)
              , HTTP (..)
              , HTTPS (..)
              , server
              ) where

import Prelude hiding (read)
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (forever)
import Control.Concurrent
import Control.Exception
import Network.Simple.TCP
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as TG

import Network.TLS
import Network.TLS.Extra.Cipher
import Data.X509
import Data.Default.Class

type Send = ByteString -> IO ()
type Recv = IO ByteString
type Worker = Send -> Recv -> IO ()

data HTTP = HTTP { httpPort :: String } deriving (Show)
instance Default HTTP where
  def = HTTP { httpPort = "8080" }

data HTTPS = HTTPS { httpsPort :: String
                   , cert :: String
                   , key  :: String
                   } deriving (Show)
instance Default HTTPS where
  def = HTTPS { httpsPort = "8081"
              , cert = ""
              , key  = ""
              }


data ServerSettings = ServerSettings { bindAddress :: String
                                     , bufferSize  :: Int
                                     , http        :: Maybe HTTP
                                     , https       :: Maybe HTTPS
                                     } deriving (Show)
instance Default ServerSettings where
  def = ServerSettings { bindAddress = "0.0.0.0"
                       , bufferSize  = 2^18
                       , http        = Nothing
                       , https       = Nothing
                       }


server :: ServerSettings -> Worker -> IO ()
server (ServerSettings bindAddr bufferSize (Just http) (Just https)) worker =
  do
    g <- TG.new
    TG.forkIO g $ server (ServerSettings bindAddr bufferSize (Just http) Nothing) worker
    TG.forkIO g $ server (ServerSettings bindAddr bufferSize Nothing (Just https)) worker
    TG.waitN 2 g

server (ServerSettings bindAddr _ _ (Just (HTTPS port cert key))) worker =
  do
    chain <- credentialLoadX509 cert key

    let (cc, pkey) = either error id chain
        (CertificateChain c) = cc
        params = def { serverCACertificates = def c
                     , serverWantClientCert = False
                     , serverShared         = def { sharedCredentials = Credentials [(cc, pkey)] }
                     , serverSupported      = def { supportedCiphers = ciphersuite_all }
                     , serverDebug = def
                     } :: ServerParams

    print $ "[HTTPS] Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      ctx <- contextNew socket params
      handshake ctx

      let write = sendData ctx . BL.fromStrict
          rcv   = recvData ctx

      worker write rcv `catch` \e -> print (e :: SomeException)

      bye ctx

    return ()

server (ServerSettings bindAddr bufferSize (Just (HTTP port)) _) worker =
  do
    print $ "[HTTP] Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      let write = send socket
          rcv = do
            d <- Network.Simple.TCP.recv socket bufferSize
            return $ fromMaybe empty d 

      worker write rcv `catch` \e -> print (e :: SomeException)

    return ()
