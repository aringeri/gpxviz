module Main where

import Geo
import GPX
import System.Environment  (getArgs)
import Data.Maybe (catMaybes)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk

import Text.XML.HXT.Core   (runX) 

type Degree = Double
type Meter = Double

data Args = Args 
  { source :: String
  } deriving (Show, Eq)

data TrackStep = TrackStep
  { distanceToPrev :: Double
  , gradient :: Double
  , elevation :: Double
  } deriving (Show, Eq)

data CumulativeStat = CumulativeStat
  { step     :: TrackStep
  , distance :: Double -- total distance
  } deriving (Show, Eq)

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Right a -> runGpx a
    Left e  -> putStrLn e >> printUsage

printUsage :: IO ()
printUsage = putStrLn "Usage:   gpx <source-file>"

parseArgs :: IO (Either String Args)
parseArgs = do
  args <- getArgs
  return $ case args of 
    [src] -> Right $ Args src
    _     -> Left "Error: Incorrect arguments"

runGpx :: Args -> IO ()
runGpx args = do
  pts <- runX $ parsePoints (source args)
  let as = analyze $ catMaybes pts
  toWindow 800 600 $ do
    layoutlr_title .= "Elevation"
    setColors [blue `withOpacity` 0.5, opaque lightgreen]
    plotLeft  $ fillBetween "elevation (m)"  
                            [(distance a, (0, elevation $ step a)) | a <- as]
    plotRight $ fillBetween "gradient (%)"   
                            [(distance a, (0, 100 * (gradient $ step a))) | a <- as]

fillBetween :: String -> [(x, (y, y))] -> EC l (PlotFillBetween x y)
fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle color
  plot_fillbetween_values .= vs

analyze :: [TrackPoint] -> [CumulativeStat]
analyze ps = 
  let sd = calcStepData ps
  in calcCumulativeStats sd

calcCumulativeStats :: [TrackStep] -> [CumulativeStat]
calcCumulativeStats [] = []
calcCumulativeStats (s:ss)  = (baseAcc s) : (accumulate' (baseAcc s) ss)
  where accumulate' _ []     = []
        accumulate' d (a:as) =
          let td = nextAcc a d
          in td : (accumulate' td as)

        baseAcc :: TrackStep -> CumulativeStat
        baseAcc st = CumulativeStat
          { step = st
          , distance = distanceToPrev st }

        nextAcc :: TrackStep -> CumulativeStat -> CumulativeStat
        nextAcc st acc = CumulativeStat 
          { step = st
          , distance = distance acc + (distanceToPrev st) }

pairs :: [a] -> [(a,a)]
pairs a = zip a (drop 1 a)

calcStepData :: [TrackPoint] -> [TrackStep]
calcStepData a = uncurry calcStep <$> (pairs a)

calcStep :: TrackPoint -> TrackPoint -> TrackStep
calcStep p1 p2 = TrackStep
  (calcDistanceWithEle p1 p2)
  (calcGradient p1 p2)
  (ele p2)
