module Main where

import FlightData
import Generator
import ObservationParser
import System.Environment
import qualified Data.Map as Map
import Data.Map(Map(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("generate":filename:numElements:_) -> generateAndWriteObservationsToFile (read numElements) 0.1 filename -- TODO: change to a better function than "read" so we can recover if the user inputs a non-number
    ("parse":filename:_) -> parseFromFile filename >>= processParsedData "errors.log"
    _ -> putStrLn "Usage:\n  generate (filename) (num): Generate num lines of test data.\n  parse (filename): Parse the given filename and produce stats"

-- log errors, and display stats
processParsedData :: FilePath -> ([String], [Observation]) -> IO ()
processParsedData errorFilename results = do
  dumpErrors errorFilename (fst results)
  displayStats (snd results)

dumpErrors :: FilePath -> [String] -> IO ()
dumpErrors filename strings = writeFile filename (unlines strings)

displayStats :: [Observation] -> IO ()
displayStats observations = do
  putStrLn $ "Min temperature: " ++ show (flightMinTemperature observations)
  putStrLn $ "Max temperature: " ++ show (flightMaxTemperature observations)
  putStrLn $ "Mean temperature: " ++ show (flightMeanTemperature observations)
  putStrLn $ "Number of observations per observatory: "
  putStrLn $ addPrefix "    " (Map.toList $ flightNumObservationsPerObservatory observations)
  putStrLn $ "Total distance travelled: " ++ show (flightTotalDistanceTravelled observations)

addPrefix :: Show a => String -> [a] -> String
addPrefix prefix = unlines . map (prefix ++) . map show
