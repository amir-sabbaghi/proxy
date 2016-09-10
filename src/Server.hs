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
                         , cert        :: String
                         , key         :: String
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port        = "3000"
                           , bufferSize  = 2^18
                           , cert        = "cert.pem"
                           , key         = "key.pem"
                           }

server :: Settings -> Worker -> IO ()
server (Settings bindAddr port bufferSize cert key) worker =
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

    print $ "Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      ctx <- contextNew socket params
      handshake ctx

      let write = sendData ctx . BL.fromStrict
          rcv   = recvData ctx

      worker write rcv `catch` \e -> print (e :: SomeException)

      bye ctx

    return ()
