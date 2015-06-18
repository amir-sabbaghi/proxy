{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Network.Socket hiding (send, recv)
import Control.Monad (forever, unless)
import Prelude hiding (getContents)
import Network.Socket.ByteString
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as BS
import Text.Printf
import HTTP
import qualified Control.Concurrent.Thread.Group as TG
import Data.List

bufferSize = 2^18

main = do
    addr <- inet_addr "0.0.0.0"
    s <- socket AF_INET Stream defaultProtocol
    setSocketOption s ReuseAddr 1
    bind s $ SockAddrInet 3000 addr
    listen s 1
    forever $ do
        (c, remote) <- accept s
        forkIO $ worker c remote

worker :: Socket -> SockAddr -> IO ()
worker c remote = do
    con <- recv c bufferSize
    unless (BS.null con) $ do
      case parseHTTP con of
        Nothing -> close c
        Just req -> handleRequest req c
      worker c remote

handleRequest :: HTTPRequest -> Socket -> IO ()
handleRequest req c = 
    case lookup "Host" (httpHeaders req) of
        Nothing -> do
            printf "No host found, closing...\n" -- 502
            close c
        Just h  -> do
            let (host, port') = break (== ':') h
                port = if null port' then
                         "80"
                       else
                         tail port'
            s <- socket AF_INET Stream defaultProtocol
            addr <- getAddrInfo Nothing (Just host) (Just port)
            case addr of
                [] -> do
                    printf "address not found for %s\n" h -- 502
                    close c
                    return ()
                (a:_) -> do
                    connect s $ addrAddress a
                    case httpMethod req of
                      "CONNECT" -> do
                                   putStrLn $ "CONNECT " ++ h
                                   handleConnect s c (httpBody req)
                      _ -> do
                           putStrLn $ httpMethod req ++ " " ++ httpPath req
                           handleSocket req s c

handleSocket :: HTTPRequest -> Socket -> Socket -> IO ()
handleSocket req s c = do
    send s $ fromString $ show (trim req) -- check output, trim req
    transfer
    where
        transfer = do
            b <- recv s bufferSize
            if BS.null b then
                close s
            else do
                send c b
                transfer

handleConnect :: Socket -> Socket -> BS.ByteString -> IO ()
handleConnect s c rem = do
  g <- TG.new
  send c "HTTP/1.1 200 Ok\r\n\r\n"
  send s rem
  (t1, _) <- TG.forkIO g $ transfer s c
  (t2, _) <- TG.forkIO g $ transfer c s
  TG.waitN 1 g
  killThread t1
  killThread t2
  close s
  where transfer s c = do
          b <- recv c bufferSize
          unless (BS.null b) $ do
            send s b
            transfer s c

trim :: HTTPRequest -> HTTPRequest
trim (HTTPRequest m p v h b) = HTTPRequest m path v headers b
    where
        path = trimPath p
        headers = trimHeaders h ++ [("Connection", "close")]
        trimPath p = if "http" `isPrefixOf` p then
                       dropWhile (/= '/') $ drop 7 p
                     else
                       p
        trimHeaders = filter (shouldBe.fst)
        shouldBe = not.(`elem` ["Proxy-Connection", "Connection"])
