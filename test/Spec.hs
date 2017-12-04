
-- testing plans:
-- 1) if i generate an observation, serialise it, then parse it -- does it match?

import Test.Hspec
import Test.QuickCheck
import FlightData
import FlightStats
import Text.ParserCombinators.ReadP
import ObservationConversions
import Generator
import ObservationParser
import ObservationSerializer
import Data.Time
import Data.Function

{-
main =
  do
    let dateString = "26 Jan 2012 10:54 AM"
    let timeFromString = readTime defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime
    -- Format YYYY/MM/DD HH:MM
    print $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeFromString
    -- Format MM/DD/YYYY hh:MM AM/PM
    print $ formatTime defaultTimeLocale "%m/%d/%Y %I:%M %p" timeFromString
-}

-- convenience function. parse a date in format "YYYY-mm-dd"
parseDate :: String -> UTCTime
parseDate = parseTimeOrError False defaultTimeLocale "%F"

-- convenience function. A temperature when we don't care what temperature
defaultTemperature :: Temperature
defaultTemperature = Temperature Kelvin 333333

generateUTCTimeBetween :: UTCTime -> UTCTime -> Gen UTCTime
generateUTCTimeBetween start end = do
  let range = diffUTCTime end start
  let rangeInSeconds = range & truncate :: Integer
  numSecs <- choose (0, rangeInSeconds)
  pure $ addUTCTime (fromIntegral numSecs) start

instance Arbitrary UTCTime where
  arbitrary = generateUTCTimeBetween start end
    where start = parseDate "1911-01-01"
          end   = parseDate "2111-12-31"

generateLocation :: ObservatoryID -> Gen Location
generateLocation observatoryID = do
    let units = distanceUnitsOf observatoryID
    x <- choose (-10000, 10000)
    y <- choose (-10000, 10000)
    pure $ Location units x y

generateTemperature :: ObservatoryID -> Gen Temperature
generateTemperature observatoryID = do
  let units = tempUnitsOf observatoryID
  value <- choose (-10000, 10000)
  pure $ Temperature units value

instance Arbitrary Observation where
  arbitrary = do
    timestamp <- arbitrary
    observatoryID <- arbitrary
    location <- generateLocation observatoryID
    temperature <- generateTemperature observatoryID
    pure $ Observation timestamp location temperature observatoryID

instance Arbitrary ObservatoryID where
  arbitrary = elements ["AU", "US", "FR", "XX"] <&> ObservatoryID

main :: IO ()
main = hspec $ do
  describe "Serialising and parsing" $ do
    it "returns the same object after serialising then parsing" $ do
      property $ \observation ->
        let string = serializeObservation observation
            newObservations = readP_to_S parseObservation string
        in case newObservations of
          ((newObservation,_):[]) -> observation == newObservation
          _ -> False

  describe "Stats" $ do
    it "can calculate distance across unit types" $ do
      flightTotalDistanceTravelled observations1 `shouldBe` 5009
    it "can calculate distance with un-ordered observations" $ do
      flightTotalDistanceTravelled observations2 `shouldBe` 6000
        where observations1 = [
                Observation (parseDate "2001-01-01") (Location Kilometers 5 5) defaultTemperature (ObservatoryID "AU"),
                Observation (parseDate "2001-02-01") (Location Meters 5000 6000) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "2001-03-01") (Location Miles 5 5) defaultTemperature (ObservatoryID "US")
                ]
              observations2 = [
                Observation (parseDate "2500-01-01") (Location Meters 3000 1000) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "1200-02-01") (Location Meters 5000 1000) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "1950-03-01") (Location Meters 1000 1000) defaultTemperature (ObservatoryID "FR")
                ]
