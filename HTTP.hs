module HTTP ( HTTPRequest (..)
            , parseHTTP
            , parse
            , httpParser
            ) where

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.ByteString hiding (elem, count)
import Data.ByteString.UTF8 (fromString, toString)

data HTTPRequest = HTTPRequest {
        httpMethod :: String,
        httpPath :: String,
        httpVersion :: String,
        httpHeaders :: [(String, String)],
        httpBody :: ByteString
    }

parseHTTP :: ByteString -> Maybe HTTPRequest
parseHTTP i = case parse httpParser "" i of
    Right e -> Just e
    Left e -> Nothing

httpParser :: Parser HTTPRequest
httpParser = do
    method <- methodParser
    char ' '
    path <- pathParser
    char ' '
    version <- versionParser
    crlf
    headers <- many headerParser
    crlf
    body <- case method of
                 "CONNECT" -> getInput
                 _ -> case lookup "Content-Size" headers of
                           Nothing -> return empty
                           Just sz -> getInput
    return (HTTPRequest method path version headers body)

methodParser :: Parser String
methodParser = many1 upper

pathParser :: Parser String
pathParser = let ch = alphaNum <|> oneOf "-._~:/?#[]@!$&'()*+,;=%"
             in many1 ch

versionParser :: Parser String
versionParser = do
    string "HTTP/"
    v <- many (digit <|> char '.')
    return ("HTTP/" ++ v)

headerParser :: Parser (String, String)
headerParser = do
    header <- many1 (alphaNum <|> char '-')
    char ':'
    char ' '
    value <- many1 (noneOf "\r\n")
    crlf
    return (header, value)

instance Show HTTPRequest where
    show (HTTPRequest m p v h b) = m ++ " " ++ p ++ " " ++ v ++ "\r\n" ++ headers ++ "\r\n" ++ toString b
        where headers = helper h
              helper [] = ""
              helper (h:hs) = fst h ++ ": " ++ snd h ++ "\r\n" ++ helper hs

