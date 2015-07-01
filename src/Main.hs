module Main where

import Server
import HTTPWorker
import Proxy

main = server defaultSettings.httpWorker handleRequest $ []
