module Main where

import Data.Char
import Control.Applicative

data JsonValue = JsonNull 
               | JsonBool Bool 
               | JsonNumber Int
               | JsonFloat Float
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (<*>) (Parser p1) (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) (Parser p1) (Parser p2) = Parser $ \input ->
        p1 input <|> p2 input


charP :: Char -> Parser Char
charP c = Parser f
    where 
        f (x:xs)
            | c == x = Just (xs, x)
            | otherwise = Nothing 
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          f _       = undefined 

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input
                             in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
    (input', xs) <- p input
    if null xs
        then Nothing
        else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where f = JsonNumber . read

nullToZero :: Parser String -> Parser String
nullToZero (Parser p) = Parser $ \input -> do
    (input', xs) <- p input
    if null xs
        then Just (input', "0")
        else Just (input', xs)

jsonFloat :: Parser JsonValue
jsonFloat = JsonFloat . read <$> float
    where
        float = (\a b c -> a ++ (b:c)) <$> nullToZero (spanP isDigit)
                                       <*> charP '.' 
                                       <*> nullToZero (spanP isDigit)

stringLitteral :: Parser String
stringLitteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLitteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' 
                            *> ws 
                            *> elements 
                            <* ws
                            <* charP ']') 
    where 
        elements = sepBy (ws *> charP ',' <* ws) jsonP

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' 
                             *> ws 
                             *> pairs 
                             <* ws
                             <* charP '}')
    where pairs = sepBy (ws *> charP ',' <* ws) pair
          pair  = (\a _ b -> (a, b)) <$> stringLitteral 
                                     <*> (ws *> charP ':' <* ws) 
                                     <*> jsonP


jsonP :: Parser JsonValue
jsonP = jsonNull <|> jsonBool <|> jsonFloat <|> jsonNumber
        <|> jsonString <|> jsonArray <|> jsonObject


parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined
