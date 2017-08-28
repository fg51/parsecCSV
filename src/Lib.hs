module Lib
    ( someFunc
    ) where

import Text.Parsec (char, endBy, many, noneOf, ParseError, parse, sepBy, (<|>))
import Text.Parsec.String (Parser)


someFunc :: IO ()
someFunc = do
  putStrLn "parsec csv sample"
  file <- readFile "sample.csv"

  let result = parseCSV file
  either (\e -> print e) (\x -> print x)  result


parseCSV :: String -> Either ParseError [[String]]
parseCSV xs = parse csvFile "error" xs


csvFile :: Parser [[String]]
csvFile = endBy csvLine eol
  where
    eol = char '\n'


csvLine :: Parser [String]
csvLine = sepBy cell sep
  where
    sep = char ','


cell :: Parser String
cell = blank *> cell' <|> cell'
  where
    blank = char ' '
    cell' :: Parser String
    cell' = many (noneOf ",\n")
