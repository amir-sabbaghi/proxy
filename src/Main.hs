module Main where

import System.Environment
import qualified Server as S
import qualified Network.Socket as S
import HTTPWorker
import Proxy

data Settings = Settings { bindAddress :: String
                         , port :: S.PortNumber
                         , bufferSize :: Int
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port = 8080
                           , bufferSize = 2^18
                           }

main = do
    args <- getArgs
    let settings = parseArgs args defaultSettings
    let servSett = S.defaultSettings { S.bindAddress = bindAddress settings
                                     , S.port = port settings
                                     , S.bufferSize = bufferSize settings
                                     }
    S.server servSett.httpWorker handleRequest $ []

parseArgs :: [String] -> Settings -> Settings
parseArgs [] s = s
parseArgs ("-p":as) s = parseArgs ("--port":as) s
parseArgs ("-b":as) s = parseArgs ("--bindaddr":as) s

parseArgs ("--port":as) s = case as of
    [] -> error "Please specify port number in front of --port"
    (p:as) -> parseArgs as $ s { port = fromInteger $ read p }

parseArgs ("--bindaddr":as) s = case as of
    [] -> error "Please specify bind address in front of --bindaddr"
    (b:as) -> parseArgs as $ s { bindAddress = b }
