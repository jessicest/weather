module FlightStats where

import FlightData
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.Time
import Data.Function
import Data.List

-- for these functions, we are *trying* to take advantage of laziness,
-- so that we can write code without dealing with the requirement that
-- the data won't necessarily fit in memory.

flightMinTemperature :: [Observation] -> Temperature
flightMinTemperature = minimum . map temperature

flightMaxTemperature :: [Observation] -> Temperature
flightMaxTemperature = maximum . map temperature

flightMeanTemperature :: [Observation] -> Double
flightMeanTemperature observations
  | allTheSame (map tUnits temperatures) = average $ map tValue temperatures
  | otherwise = error "can't calculate mean unless all units are the same"
  where temperatures = map temperature observations
        allTheSame xs = and $ zipWith (==) xs (drop 1 xs)

flightNumObservationsPerObservatory :: [Observation] -> Map ObservatoryID Integer
flightNumObservationsPerObservatory = foldr incrementObservation Map.empty . map observatoryID
  where incrementObservation observatoryID = Map.insertWith (+) observatoryID 1

flightTotalDistanceTravelled :: [Observation] -> Double
flightTotalDistanceTravelled [] = 0
flightTotalDistanceTravelled observations = sum $ zipWith distance locations (tail locations)
  where locations = sortedObservations & map location
        sortedObservations = sortBy (\obs1 obs2 -> compare (timestamp obs1) (timestamp obs2)) observations

-- i don't think this will work on too-large-for-memory lists. Needs checking and probably rewriting
average :: [Double] -> Double
average [] = error "can't produce the average of an empty list" -- TODO: make sure our tests cover this case
average nums = sum nums / fromIntegral (length nums)

distance :: Location -> Location -> Double
distance (Location units1 x1 y1) (Location units2 x2 y2)
  = if units1 == units2
    then sqrt (x ** 2 + y ** 2)
    else error "todo: can't find distance between different units"
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
  where showLocation (Location units x y) = show x ++ "," ++ show y
