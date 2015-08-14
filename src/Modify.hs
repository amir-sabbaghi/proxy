module Modify ( modify
              , Criteria (..)
              ) where

import HTTPWorker
import HTTPParser
import Data.List

data Criteria = Criteria { method :: String
                         , prefix :: String
                         , terminator :: String
                         , replacement :: String
                         } deriving (Show, Read)

modify :: [Criteria] -> RequestHandler a -> RequestHandler a
modify []     handler req = handler req
modify (c:cs) handler req =
    if match c req then
        handler (replace c req)
    else
        modify cs handler req
    where match c r = method c == httpMethod r && prefix c `isPrefixOf` httpPath r
          replace c (HTTPRequest m p v h b) = let head = prefix c
                                                  t1 = drop (length head) p
                                                  cond = flip elem $ terminator c
                                                  tail = dropWhile (not.cond) t1
                                                  path = head ++ replacement c ++ tail
                                              in HTTPRequest m path v h b
