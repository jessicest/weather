
module ObservationParser where

import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import Data.Text(Text)
import FlightData
import Data.Time

-- example input
-- 2014-12-31T13:44|10,5|243|AU

isVowel :: Char -> Bool
isVowel char =
    any (char ==) "aouei"

vowel :: ReadP Char
vowel =
    satisfy isVowel

timestamp :: ReadP UTCTime
timestamp = readPTime True defaultTimeLocale "%FT%R" -- time format: yyyy-MM-ddThh:mm

location :: ReadP Location
location = undefined

parseObservation :: Text -> Either Text Observation
parseObservation = undefined

-- input: the log file as described in the requirements
-- output 1: a list of entries that could not be parsed
-- output 2: a list of observations that were successfully parsed
parseFlightLog :: Text -> ([Text], [Observation])
parseFlightLog = undefined
