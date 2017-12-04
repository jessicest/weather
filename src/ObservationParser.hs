
module ObservationParser where

import Text.ParserCombinators.ReadP
import FlightData
import Data.Time
import Data.Char
import Control.Applicative
import Data.Function

-- example input
-- 2014-12-31T13:44|10,5|243|AU

digit :: ReadP Char
digit = satisfy isDigit

-- parse a floating point number. We're ignoring things like exponential notation for now.
double :: ReadP Double
double = many1 (char '-' <|> char '.' <|> digit) <&> read

parseTimestamp :: ReadP UTCTime
parseTimestamp = readPTime True defaultTimeLocale "%FT%R" -- time format: yyyy-MM-ddThh:mm

parseLocation :: ReadP Location
parseLocation = do
  x <- double
  char ','
  y <- double
  pure $ Location x y

parseTemperature :: ReadP Temperature
parseTemperature = double

parseObservatoryID :: ReadP ObservatoryID
parseObservatoryID = do
  string <- many1 (satisfy isAsciiUpper)
  pure $ ObservatoryID string

parseObservation :: ReadP Observation
parseObservation = do
  timestamp <- parseTimestamp
  char '|'
  location <- parseLocation
  char '|'
  temperature <- parseTemperature
  char '|'
  observatoryID <- parseObservatoryID
  pure Observation {
      timestamp = timestamp,
      location = location,
      temperature = temperature,
      observatoryID = observatoryID
      }

-- input: the log file as described in the requirements
-- output 1: a list of entries that could not be parsed
-- output 2: a list of observations that were successfully parsed
parseFlightLog :: String -> ([String], [Observation])
parseFlightLog input = do
  let inputs = lines input
  let results = map (tryParse parseObservation) inputs
  splitEithers results

splitEithers :: [Either a b] -> ([a], [b])
splitEithers = foldr add ([], [])
  where add (Left a) (as, bs) = (a:as, bs)
        add (Right b) (as, bs) = (as, b:bs)

tryParse :: ReadP a -> String -> Either String a
tryParse reader input = case parseResult of
    [] -> Left input
    (result:results) -> Right (fst result)
  where parseResult = readP_to_S reader input & filter fullyParsed
        fullyParsed (a, string) = null string

parseFromFile :: FilePath -> IO ([String], [Observation])
parseFromFile filename = do
  contents <- readFile filename
  pure (parseFlightLog contents)
