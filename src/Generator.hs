
module Generator where

import System.Random

  -- example output
  -- 2014-12-31T13:44|10,5|243|AU

digit :: IO Char
digit = randomRIO ('0', '9')

uppercase :: IO Char
uppercase = randomRIO ('A', 'Z')

-- generate a random date string
-- todo: if we can find the haskell functions to convert an epoch timestamp to a
--  haskell date/time format such as UTCTime,
--  that would be a much simpler way to generate a date/time,
--  and it would allow us to safely generate the 31st of a month, too.
-- note: this won't pad the digits, but it probably shouldn't matter too much
date :: IO String
date = concat <$> sequence [
  show <$> randomRIO (1900, 2100 :: Int),
  pure "-",
  show <$> randomRIO (1, 12 :: Int),
  pure "-",
  show <$> randomRIO (1, 28 :: Int)
  ]

  -- note: this won't pad the digits, but it probably shouldn't matter too much
time :: IO String
time = concat <$> sequence [
  show <$> randomRIO (0, 23 :: Int),
  pure ":",
  show <$> randomRIO (0, 59 :: Int)
  ]

datetime :: IO String
datetime = concat <$> sequence [
  date,
  pure "T",
  time
  ]

location :: IO String
location = do
  x <- randomRIO (-1000.0, 1000.0 :: Double)
  y <- randomRIO (-1000.0, 1000.0 :: Double)
  pure $ show x ++ "," ++ show y

temperature :: IO String
temperature = show <$> randomRIO (-1000.0, 1000.0 :: Double) -- these are probably too large to be realistic but it's ok because the system should be able to handle it anyway

observatoryID :: IO String
observatoryID = chooseRandomElement ["AU", "US", "FR", "XX"]

chooseRandomElement :: [a] -> IO a
chooseRandomElement [] = error "you can't chooseRandomElement on an empty list"
chooseRandomElement list = do
  index <- randomRIO (1, length list)
  pure (list !! (index - 1))

generateObservationString :: IO String
generateObservationString = concat <$> sequence [
  datetime,
  pure "|",
  location,
  pure "|",
  temperature,
  pure "|",
  observatoryID
  ]

generateAndWriteObservationsToFile :: Int -> String -> IO ()
generateAndWriteObservationsToFile = undefined
