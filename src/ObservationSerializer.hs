
module ObservationSerializer
( serializeObservation
) where

import Control.Monad
import FlightData
import Data.Time

serializeObservation :: Observation -> String
serializeObservation observation
  = serializeTimestamp (timestamp observation)
  ++ "|"
  ++ serializeLocation (location observation)
  ++ "|"
  ++ serializeTemperature (temperature observation)
  ++ "|"
  ++ serializeObservatoryID (observatoryID observation)

serializeTimestamp :: Timestamp -> String
serializeTimestamp = formatTime defaultTimeLocale "%FT%R"

serializeLocation :: Location -> String
serializeLocation (Location _ x y) = show x ++ "," ++ show y

serializeTemperature :: Temperature -> String
serializeTemperature (Temperature _ value) = show value

serializeObservatoryID :: ObservatoryID -> String
serializeObservatoryID (ObservatoryID name) = name
