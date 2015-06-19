module HTTPWorker ( httpWorker
                  ) where

import Server
import HTTPParser
import qualified Data.ByteString as BS
import Control.Monad (unless)

type RequestHandler = HTTPRequest -> Send -> Recv -> IO ()

httpWorker :: RequestHandler -> Worker
httpWorker handler send recv = do
  packet <- recv
  unless (BS.null packet) $ do
    case parseHTTP packet of
     Nothing  -> do
       putStrLn $ "[Error] Unrecognized HTTP request:"
       putStrLn $ show packet
     Just req -> handler req send recv
    httpWorker handler send recv
