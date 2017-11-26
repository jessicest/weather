
module FlightData where

import qualified Data.Text as T
import Data.Text(Text(..))

data Timestamp = Timestamp Integer -- we'll clarify this one later
data Location = Location Integer Integer -- x, y position in meters
data Temperature = Temperature Integer -- temperature in degrees kelvin
data ObservatoryID = ObservatoryID Text
data Observation = Observation Timestamp Location Temperature ObservatoryID

parseObservation :: Text -> Either Text Observation
parseObservation = undefined
