module POI
  (
    initPOI
  ) where

import System.Random (randomRIO)

import System.IO.Unsafe

import Location (randomLoc)
import Types (POI (..), notInitPOI, Dims)

-- | Initializes a POI. When read from a YAML file, a POI may have blank entries.
--     To separate IO from parsing, POI values are initialized randomly here.
initPOI :: Dims -> POI -> IO POI
initPOI dims (POI loc val coupling radius) = do
    loc'      <- if loc == (pLocation notInitPOI)
                 then randomLoc dims else return loc
    val'      <- if val == (pValue notInitPOI)
                 then randomRIO (1,10) else return val
    coupling' <- if coupling == (pCoupling notInitPOI)
                 then return 1 else return coupling
    radius'   <- if radius == (pObservationRadius notInitPOI)
                 then return 4
                 else do
      putStrLn $ show radius
      return radius
    return $ POI loc' val' coupling' radius'
