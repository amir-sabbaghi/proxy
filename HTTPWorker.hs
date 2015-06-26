module HTTPWorker ( httpWorker
                  ) where

import Server
import HTTPParser
import qualified Data.ByteString as BS
import Control.Monad (unless)

type RequestHandler a = HTTPRequest -> Send -> Recv -> a -> IO a

httpWorker :: RequestHandler a -> a -> Worker
httpWorker handler state send recv = do
  packet <- recv
  unless (BS.null packet) $ do
    news <- case parseHTTP packet of
      Nothing  -> do
        putStrLn $ "[Error] Unrecognized HTTP request:"
        putStrLn $ show packet
        return state
      Just req -> handler req send recv state
    httpWorker handler news send recv
