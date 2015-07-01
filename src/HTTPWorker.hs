module HTTPWorker ( httpWorker
                  ) where

import Server
import HTTPParser
import qualified Data.ByteString as BS
import Control.Monad (unless, when)

type RequestHandler a = HTTPRequest -> Send -> Recv -> a -> IO (Bool, a)

httpWorker :: RequestHandler a -> a -> Worker
httpWorker handler state send recv = do
  packet <- recv
  unless (BS.null packet) $ do
    (cont, news) <- case parseHTTP packet of
      Nothing  -> do
        putStrLn $ "[Error] Unrecognized HTTP request:"
        putStrLn $ show packet
        return (False, state)
      Just req -> handler req send recv state
    when cont $ httpWorker handler news send recv
