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
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (forever)
import Control.Concurrent
import Control.Exception
import Network.Simple.TCP

import Network.TLS
import Network.TLS.Extra.Cipher
import Data.X509
import Data.Default.Class

type Send = ByteString -> IO ()
type Recv = IO ByteString
type Worker = Send -> Recv -> IO ()

data Settings = Settings { bindAddress :: String
                         , port        :: String
                         , bufferSize  :: Int
                         , cert        :: String
                         , key         :: String
                         , https       :: Bool
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port        = "3000"
                           , bufferSize  = 2^18
                           , cert        = "cert.pem"
                           , key         = "key.pem"
                           , https       = True
                           }

server :: Settings -> Worker -> IO ()
server (Settings bindAddr port _ cert key True) worker =
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

server (Settings bindAddr port bufferSize cert key False) worker =
  do
    print $ "Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      let write = send socket
          rcv = do
            d <- Network.Simple.TCP.recv socket bufferSize
            return $ fromMaybe empty d 

      worker write rcv `catch` \e -> print (e :: SomeException)

    return ()
