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


data SampleData = SampleData
  { sampleDate :: String
  , sampleModel :: String
  , sampleSerial :: String
  , sampleData :: [[String]]
  } deriving Show


parseCSV :: String -> Either ParseError SampleData
parseCSV xs = parse csvSampleData "error" xs

csvSampleData :: Parser SampleData
csvSampleData = do
  header <- csvHead
  xs <- csvFile
  return SampleData
    { sampleDate   = header !! 0
    , sampleModel  = header !! 1
    , sampleSerial = header !! 2
    , sampleData   = xs
    }

csvHead :: Parser [String]
csvHead = csvLine <* eol


csvFile :: Parser [[String]]
csvFile = endBy csvLine eol

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


