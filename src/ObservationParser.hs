
module ObservationParser where

import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import Data.Text(Text)
import qualified Data.Text.Lazy as L
import FlightData
import Data.Time
import Data.Char(isDigit)
import Control.Applicative

-- example input
-- 2014-12-31T13:44|10,5|243|AU

(<&>) = flip (<$>)

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
  return $ Location x y

parseTemperature :: ReadP Temperature
parseTemperature = double

parseObservatoryID :: ReadP ObservatoryID
parseObservatoryID = do
  string <- many1 (satisfy (\char -> char >= 'A' && char <= 'Z'))
  return $ ObservatoryID (T.pack string)

parseObservation :: ReadP Observation
parseObservation = do
  timestamp <- parseTimestamp
  char '|'
  location <- parseLocation
  char '|'
  temperature <- parseTemperature
  char '|'
  observatoryID <- parseObservatoryID
  return Observation {
      timestamp = timestamp,
      location = location,
      temperature = temperature,
      observatoryID = observatoryID
      }

-- input: the log file as described in the requirements
-- output 1: a list of entries that could not be parsed
-- output 2: a list of observations that were successfully parsed
parseFlightLog :: String -> ([String], [Observation])
parseFlightLog input = lines input & readP_to_S parseObservation
