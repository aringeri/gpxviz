module Geo 
  ( TrackPoint(..)
  , elevationChange
  , calcDistance
  , calcDistanceWithEle
  , calcGradient
  ) where

data TrackPoint = TrackPoint
  { lat :: Double -- in degrees
  , lon :: Double -- in degrees
  , ele :: Double -- in meters
  } deriving (Show, Eq)

elevationChange :: TrackPoint -> TrackPoint -> Double
elevationChange p1 p2 = ele p2 - (ele p1)

-- Calculate distance considering elevation data.
-- Assuming straight line between the two points
calcDistanceWithEle :: TrackPoint -> TrackPoint -> Double
calcDistanceWithEle p1 p2 = 
  let d = calcDistance p1 p2
      e = abs $ elevationChange p1 p2
  in  sqrt $ d^2 + e^2

-- Equirectangular approximation
-- https://www.movable-type.co.uk/scripts/latlong.html
calcDistance :: TrackPoint -> TrackPoint -> Double
calcDistance p1 p2 = d
  where 
    lat1 = toRad $ lat p1
    lat2 = toRad $ lat p2
    lon1 = toRad $ lon p1
    lon2 = toRad $ lon p2
    x = (lon2 - lon1) * (cos $ (lat1 + lat2)/2)
    y = lat2 - lat1
    d = (sqrt $ x^2 + y^2) * earthRadius

earthRadius :: Double
earthRadius = 6371e3

toRad :: Floating a => a -> a
toRad d = d * (pi/180)

toDeg :: Floating a => a -> a
toDeg r = r * (180/pi)

calcGradient :: TrackPoint -> TrackPoint -> Double
calcGradient p1 p2 = 
  let d  = calcDistance p1 p2
      dE = elevationChange p1 p2
  in  if d == 0 then 0 else dE/d
