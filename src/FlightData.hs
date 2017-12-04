
module FlightData where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.Time
import Data.Function

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

type Timestamp = UTCTime
data Location = Location Double Double deriving (Eq, Show) -- x, y position in meters
type Temperature = Double -- temperature in degrees kelvin
data ObservatoryID = ObservatoryID String deriving (Eq, Show, Ord)
data Observation = Observation {
  timestamp :: Timestamp,
  location :: Location,
  temperature :: Temperature,
  observatoryID :: ObservatoryID
  } deriving (Eq)

-- for these functions, we are *trying* to take advantage of laziness,
-- so that we can write code without dealing with the requirement that
-- the data won't necessarily fit in memory.

flightMinTemperature :: [Observation] -> Double
flightMinTemperature = minimum . map temperature

flightMaxTemperature :: [Observation] -> Double
flightMaxTemperature = maximum . map temperature

flightMeanTemperature :: [Observation] -> Double
flightMeanTemperature = average . map temperature

-- i don't believe this will have trouble with large data sets -- it should work well as a lazy method
flightNumObservationsPerObservatory :: [Observation] -> Map ObservatoryID Integer
flightNumObservationsPerObservatory = foldr incrementObservation Map.empty . map observatoryID
  where incrementObservation observatoryID = Map.insertWith (+) observatoryID 1

-- this one won't work with large data sets
flightTotalDistanceTravelled :: [Observation] -> Double
flightTotalDistanceTravelled [] = 0
flightTotalDistanceTravelled observations = sum $ zipWith distance locations (tail locations)
  where locations = map location observations

-- i don't think this will work on too-large-for-memory lists. Needs checking and probably rewriting
average :: [Double] -> Double
average [] = error "can't produce the average of an empty list" -- TODO: make sure our tests cover this case
average nums = sum nums / fromIntegral (length nums)

distance :: Location -> Location -> Double
distance (Location x1 y1) (Location x2 y2) = sqrt (x ** 2 + y ** 2)
  where x = x1 - x2
        y = y1 - y2

observationToString :: Observation -> String
observationToString observation
  = formatTime defaultTimeLocale "%FT%R" (timestamp observation)
  ++ "|"
  ++ showLocation (location observation)
  ++ "|"
  ++ show (temperature observation)
  ++ "|"
  ++ show (observatoryID observation)
  where showLocation (Location x y) = show x ++ "," ++ show y
