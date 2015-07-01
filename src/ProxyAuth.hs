module ProxyAuth ( proxyAuth
                 ) where

import HTTPWorker
import HTTPParser
import Data.List
import Codec.Binary.Base64.String
import Data.ByteString.Char8 (pack)

proxyAuth :: String -> String -> RequestHandler a -> RequestHandler a
proxyAuth userpass realm handler req send recv state =
    case lookup "Proxy-Authorization" $ httpHeaders req of
        Nothing -> do
            send $ pack.intercalate "\r\n" $
                ["HTTP/1.1 407 Proxy Authentication Required"
                ,"Proxy-Authenticate: Basic realm=\""++realm++"\""
                ,"Connection: Keep-Alive"
                ,"Content-Length: 0"
                ,"\r\n"]
            return (True, state)
        Just auth -> let headers = filter (\(h, _) -> h /= "Proxy-Authorization") $ httpHeaders req
                         newreq  = req { httpHeaders = headers }
                     in if auth == "Basic " ++ encode userpass then
                            handler newreq send recv state
                        else
                            proxyAuth userpass realm handler newreq send recv state
