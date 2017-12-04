
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
import Control.Applicative

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

-- an equality function, but we raise an error
-- so that in addition to a failed test we get extra info about what's not equal
(=!=) :: (Show a, Eq a) => a -> a -> Bool
a =!= b = if a == b
            then True
            else error $ show a ++ " =!= " ++ show b

-- convenience function. parse a date in format "YYYY-mm-dd"
parseDate :: String -> UTCTime
parseDate = parseTimeOrError False defaultTimeLocale "%F"

-- helpful for matching doubles and floats
class RoughEquality a where
  roughlyEqual :: a -> a -> Bool

instance RoughEquality Double where
  roughlyEqual lhs rhs = (lhs * 1000 & round) =!= (rhs * 1000 & round)

instance RoughEquality Observation where
  roughlyEqual lhs rhs
    = timestamp lhs =!= timestamp rhs
    && roughlyEqual (location lhs) (location rhs)
    && roughlyEqual (temperature lhs) (temperature rhs)
    && observatoryID lhs =!= observatoryID rhs

instance RoughEquality Location where
  roughlyEqual lhs rhs
    = lUnits lhs =!= lUnits rhs
    && roughlyEqual (lx lhs) (lx rhs)
    && roughlyEqual (ly lhs) (ly rhs)

instance RoughEquality Temperature where
  roughlyEqual lhs rhs
    = tUnits lhs =!= tUnits rhs
    && roughlyEqual (tValue lhs) (tValue rhs)

-- convenience function. A temperature when we don't care what temperature
defaultTemperature :: Temperature
defaultTemperature = Temperature Kelvin 333333

-- Control.Applicative only goes up to liftA3 apparently :(
-- so we need to roll our own
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 func fa fb fc fd = func <$> fa <*> fb <*> fc <*> fd

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

  shrink o
    = drop 1
     $ liftA4
       Observation
       (shrink2 $ timestamp o)
       (shrinkLocation $ location o)
       (shrinkTemperature $ temperature o)
       [observatoryID o]
      where shrink2 a = a : shrink a
            shrinkLocation location@(Location units x y) = liftA2 (Location units) (shrink2 x) (shrink2 y)
            shrinkTemperature temperature@(Temperature units value) = Temperature units <$> shrink value

instance Arbitrary ObservatoryID where
  arbitrary = elements ["AU", "US", "FR", "XX"] <&> ObservatoryID

main :: IO ()
main = hspec $ do
  describe "Serialising and parsing" $ do
    it "returns the same object after serialising then parsing (hspec)" $ do
      let observation = Observation (parseDate "1929-09-05") (Location Kilometers 2771.7835186020893 0.0) (Temperature Kelvin 0.0) (ObservatoryID "XX")
      let string = serializeObservation observation
      let result = readP_to_S parseObservation string
      length result `shouldBe` 1
      let (newObservation, cruft) = head result
      cruft `shouldBe` ""
      normalizeObservation newObservation `shouldSatisfy` roughlyEqual (normalizeObservation observation)

    it "returns the same object after serialising then parsing (quickcheck)" $ do
      property $ \observation ->
        let string = serializeObservation observation
            newObservations = readP_to_S parseObservation string
        in case newObservations of
          (newObservation,_) : [] -> roughlyEqual (normalizeObservation observation) (normalizeObservation newObservation)
          _ -> False

  describe "Stats" $ do
    it "can calculate distance across unit types" $ do
      flightTotalDistanceTravelled observations1 `shouldSatisfy` (roughlyEqual 2046.6999)
    it "can calculate distance with un-ordered observations" $ do
      flightTotalDistanceTravelled observations2 `shouldSatisfy` (roughlyEqual 6000)
    it "can calculate distance along diagonals" $ do
      flightTotalDistanceTravelled observations3 `shouldSatisfy` (roughlyEqual 10.198)
    it "auto-converts kilometer results to meters" $ do
      flightTotalDistanceTravelled observations4 `shouldSatisfy` (roughlyEqual 4000)
        where observations1 = [
                Observation (parseDate "2001-02-01") (Location Meters 0 6000) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "2001-03-01") (Location Miles  0    5) defaultTemperature (ObservatoryID "US")
                ]
              observations2 = [
                Observation (parseDate "2500-01-01") (Location Meters 3000 1000) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "1200-02-01") (Location Meters 5000 1000) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "1950-03-01") (Location Meters 1000 1000) defaultTemperature (ObservatoryID "FR")
                ]
              observations3 = [
                Observation (parseDate "2035-01-01") (Location Meters (-2) 8) defaultTemperature (ObservatoryID "FR"),
                Observation (parseDate "2035-01-02") (Location Meters   8 10) defaultTemperature (ObservatoryID "FR")
                ]
              observations4 = [
                Observation (parseDate "2035-01-01") (Location Kilometers 9 9) defaultTemperature (ObservatoryID "AU"),
                Observation (parseDate "2035-01-02") (Location Kilometers 9 5) defaultTemperature (ObservatoryID "AU")
                ]
