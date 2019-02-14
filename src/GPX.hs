{-# LANGUAGE Arrows #-}
module GPX
  ( parsePoints
  ) where

import Text.XML.HXT.Core
import Geo (TrackPoint(..))
import Text.Read (readMaybe)

parsePoints :: String -> IOSArrow XmlTree (Maybe TrackPoint)
parsePoints src = 
  readDocument [withValidate no] src >>> getPoints
  where 
    getPoints = deep (isElem >>> hasName "trkpt") >>> 
      proc pt -> do
        lat' <- getAttrValue "lat" -< pt
        lon' <- getAttrValue "lon" -< pt
        ele' <- getText <<< getChildren <<< deep (hasName "ele") -< pt
        returnA -< 
          TrackPoint <$> (readMaybe lat')
            <*> (readMaybe lon')
            <*> (readMaybe ele')


