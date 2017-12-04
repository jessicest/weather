
module Generator where

import Data.Function
import FlightData
import Control.Monad
import System.Random
import Data.Time
import Data.Char
import Data.Bifunctor
import ObservationSerializer

  -- example output
  -- 2014-12-31T13:44|10,5|243|AU

-- unfortunately i have to copy/paste this code from the quickcheck implementations because i'm not really clear if i can write one algorithm that works with both RandomGen and Gen

generateUTCTimeBetween :: (RandomGen g) => (UTCTime, UTCTime) -> g -> (UTCTime, g)
generateUTCTimeBetween (start, end) gen =
  let range = diffUTCTime end start & truncate :: Integer
      (numSecs, newGen) = randomR (0, range) gen
  in (addUTCTime (fromIntegral numSecs) start, newGen)

instance Random UTCTime where
  randomR = generateUTCTimeBetween
  random = randomR (start, end)
    where start = parseTimeOrError True defaultTimeLocale "%F" "1911-01-01"
          end   = parseTimeOrError True defaultTimeLocale "%F" "2111-12-31"

instance Random Location where
  randomR (Location units0 x0 y0, Location units1 x1 y1) gen0
    = if units0 == units1
        then (Location units0 x y, gen2)
        else error "Unit mismatch in Location.randomR"
    where (x, gen1) = randomR (x0, x1) gen0
          (y, gen2) = randomR (y0, y1) gen1
  random = randomR (Location Meters (-10000) (-10000), Location Meters 10000 10000)

randomTemperature :: (RandomGen g) => g -> (Temperature, g)
randomTemperature gen = randomR (-10000, 10000) gen & first (Temperature Kelvin)

randomObservatoryID :: (RandomGen g) => g -> (ObservatoryID, g)
randomObservatoryID gen = randomElement ["AU", "US", "FR", "XX"] gen & first ObservatoryID

-- Observation isn't an instance of Random because randomR would be a hassle to implement
-- (also because it would be confusing/weird for the user)

-- IO is simpler than RandomGen so we'll just be lazy and make this IO for now...
-- will refactor to use RandomGen if it's a blocker for something
randomObservation :: IO Observation
randomObservation = do
  timestamp <- randomIO
  location <- randomIO
  temperature <- getStdRandom randomTemperature
  observatoryID <- getStdRandom randomObservatoryID -- phew, getStdRandom exists so i can maintain my hack
  pure Observation {
           timestamp = timestamp,
           location = location,
           temperature = temperature,
           observatoryID = observatoryID
           }

randomElement :: (RandomGen g) => [a] -> g -> (a, g)
randomElement [] _ = error "can't choose randomElement from empty list"
randomElement list gen = randomR (0, length list - 1) gen & first (list !!)

-- given 2 ways to generate strings, return a mixture of both
intermix :: Int -> Double -> IO String -> IO String -> IO [String]
intermix numEntries chance leftGenerator rightGenerator
  = replicateM numEntries generate
  where generate = do
          number <- randomRIO (0, 1)
          if number < chance
             then leftGenerator
             else rightGenerator

generateUnreliableObservations :: Int -> Double -> IO String
generateUnreliableObservations numEntries errorChance
  = unlines <$> intermix numEntries errorChance generateError generateValid
    where generateError = pure "bad data" -- TODO: we could make this more elaborate, to test almost-valid strings
          generateValid = randomObservation <&> serializeObservation

generateAndWriteObservationsToFile :: Int -> Double -> FilePath -> IO ()
generateAndWriteObservationsToFile numEntries errorChance filename
  = generateUnreliableObservations numEntries errorChance >>= writeFile filename
