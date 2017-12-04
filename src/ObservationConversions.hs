
module ObservationConversions where

import FlightData

convertLocation :: DistanceUnits -> Location -> Location
convertLocation targetUnits location@(Location sourceUnits x y)
  = case sourceUnits of
          Meters     -> convertFromMeters targetUnits location
          Kilometers -> convertFromMeters targetUnits (Location Meters (x * 1000) (y * 1000))
          Miles      -> convertFromMeters targetUnits (Location Meters (x * 1609.34) (y * 1609.34))
    where convertFromMeters targetUnits location@(Location sourceUnits x y)
            = case targetUnits of
                Meters     -> location
                Kilometers -> Location Kilometers (x / 1000) (y / 1000)
                Miles      -> Location Miles (x / 1609.34) (y / 1609.34)

normalizeLocation :: Location -> Location
normalizeLocation = convertLocation Meters

distanceUnitsOf :: ObservatoryID -> DistanceUnits
distanceUnitsOf (ObservatoryID "US") = Miles
distanceUnitsOf (ObservatoryID "FR") = Meters
distanceUnitsOf _ = Kilometers

convertTemperature :: TemperatureUnits -> Temperature -> Temperature
convertTemperature targetUnits temperature@(Temperature sourceUnits value)
  = case sourceUnits of
          Kelvin     -> convertFromKelvin targetUnits temperature
          Celsius    -> convertFromKelvin targetUnits (Temperature Kelvin (value * 273.15))
          Fahrenheit -> convertFromKelvin targetUnits (Temperature Kelvin ((value + 459.67) * 5/9))
          -- TODO: confirm/doublecheck Fahrenheit -> Kelvin formula from https://www.rapidtables.com/convert/temperature/how-fahrenheit-to-kelvin.html
    where convertFromKelvin targetUnits temperature@(Temperature sourceUnits value)
            = case targetUnits of
                Kelvin     -> temperature
                Celsius    -> Temperature Celsius (value / 273.15)
                Fahrenheit -> Temperature Fahrenheit (value * 9/5 - 459.67)

normalizeTemperature :: Temperature -> Temperature
normalizeTemperature = convertTemperature Kelvin

tempUnitsOf :: ObservatoryID -> TemperatureUnits
tempUnitsOf (ObservatoryID "AU") = Celsius
tempUnitsOf (ObservatoryID "US") = Fahrenheit
tempUnitsOf _ = Kelvin

normalizeObservation :: Observation -> Observation
normalizeObservation observation@(Observation _ location temperature _)
  = observation {
      location = normalizeLocation location,
      temperature = normalizeTemperature temperature
      }
