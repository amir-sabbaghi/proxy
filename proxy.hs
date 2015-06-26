{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Network.Socket hiding (send, recv)
import Control.Monad (unless)
import Network.Socket.ByteString
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Thread.Group as TG
import Control.Exception
import Text.Printf
import Data.List
import Data.CaseInsensitive (mk)

import Server
import HTTPWorker
import HTTPParser

type State = (String, Socket, ThreadId)

main = server defaultSettings.httpWorker handleRequest $ []

handleRequest :: HTTPRequest -> Send -> Recv -> [State] -> IO [State]
handleRequest req cSend cRecv openSockets =
  case lookup "Host" (httpHeaders req) of
    Nothing -> do
      cSend "HTTP/1.1 502 Bad Gateway\r\n\r\n"
      printf "Host not found in headers\n"
      return openSockets
    Just h  -> do
      case find (\(a, _, _) -> a == h) openSockets of
       Just (h, s, tid) -> do
         send s $ fromString $ show (trim req) -- check output, trim req
         return openSockets
       Nothing -> do
         let (host, port') = break (== ':') h
             port = if null port' then
                      "80"
                    else
                      tail port'
         s <- socket AF_INET Stream defaultProtocol
         addr <- try $ getAddrInfo Nothing (Just host) (Just port)
         case addr of
             Left (_ :: IOException) -> do
               cSend "HTTP/1.1 502 Bad Gateway\r\n\r\n"
               printf "Could not resolve address: %s\n" host
               return openSockets
             Right (a:_) -> do
               connect s $ addrAddress a
               case httpMethod req of
                 "CONNECT" -> do
                   putStrLn $ "CONNECT " ++ h
                   handleConnect cSend cRecv (send s) (recv s (2^18))
                   close s
                   return openSockets
                 _ -> do
                   putStrLn $ httpMethod req ++ " " ++ httpPath req
                   tid <- forkIO $ handleSocket req cSend (send s) (recv s (2^18))
                   return $ (h, s, tid):openSockets

handleSocket :: HTTPRequest -> Send -> Send -> Recv -> IO ()
handleSocket req cSend sSend sRecv = do
    sSend $ fromString $ show (trim req) -- check output, trim req
    transfer
    where
        transfer = do
            b <- sRecv
            unless (BS.null b) $ do
              cSend b
              transfer

handleConnect :: Send -> Recv -> Send -> Recv -> IO ()
handleConnect cSend cRecv sSend sRecv = do
  g <- TG.new
  cSend "HTTP/1.1 200 Ok\r\n\r\n"
  (t1, _) <- TG.forkIO g $ transfer sSend cRecv
  (t2, _) <- TG.forkIO g $ transfer cSend sRecv
  TG.waitN 1 g
  killThread t1
  killThread t2
  where transfer snd rcv = do
          b <- rcv
          unless (BS.null b) $ do
            snd b
            transfer snd rcv

trim :: HTTPRequest -> HTTPRequest
trim (HTTPRequest m p v h b) = HTTPRequest m path v headers b
    where
        path = trimPath p
        headers = trimHeaders h
        trimPath p = if "http" == mk (take 4 p) then
                       dropWhile (/= '/') $ drop 7 p
                     else
                       p
        trimHeaders = filter (shouldBe.fst)
        shouldBe = not.(`elem` ["Proxy-Connection", "Connection"])
