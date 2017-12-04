
-- testing plans:
-- 1) if i generate an observation, serialise it, then parse it -- does it match?

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import FlightData
import Text.ParserCombinators.ReadP
import ObservationParser
import Data.Time
import Data.Function
import System.Random

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

generateUTCTimeBetween :: UTCTime -> UTCTime -> Gen UTCTime
generateUTCTimeBetween start end = do
  let range = diffUTCTime end start
  let rangeInSeconds = range & truncate :: Integer
  numSecs <- choose (0, rangeInSeconds)
  pure $ addUTCTime (fromIntegral numSecs) start

instance Arbitrary UTCTime where
  arbitrary = generateUTCTimeBetween start end
    where start = parseTimeOrError True defaultTimeLocale "%F" "1911-01-01"
          end   = parseTimeOrError True defaultTimeLocale "%F" "2111-12-31"

instance Arbitrary Location where
  arbitrary = do
    x <- choose (-10000, 10000)
    y <- choose (-10000, 10000)
    pure $ Location x y

instance Arbitrary ObservatoryID where
  arbitrary = elements ["AU", "US", "FR", "XX"] <&> ObservatoryID

main :: IO ()
main = hspec $ do
  describe "Serialising and parsing" $ do
    it "returns the same object after serialising then parsing" $ do
      property $ \time loc temp obs ->
        let observation = Observation { timestamp = time, location = loc, temperature = temp, observatoryID = obs }
            string = observationToString observation
            newObservations = readP_to_S parseObservation string
        in case newObservations of
          ((newObservation,_):[]) -> observation == newObservation
          _ -> False

  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

      {-
  describe "flightStats" $ do
    it "returns stats about the flight" $ do

  describe "parseFlight" $ do

sampleFlight = [
    Flight
]
-}
