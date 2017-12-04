
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
  ++ show (temperature observation)
  ++ "|"
  ++ serializeObservatoryID (observatoryID observation)

serializeTimestamp :: Timestamp -> String
serializeTimestamp = formatTime defaultTimeLocale "%FT%R"

serializeLocation :: Location -> String
serializeLocation (Location x y) = show x ++ "," ++ show y

serializeObservatoryID :: ObservatoryID -> String
serializeObservatoryID (ObservatoryID name) = name
