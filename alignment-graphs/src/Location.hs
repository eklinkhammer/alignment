module Location where

import System.Random
import Data.Random.Normal (normalsIO)

import Types (Dims (..), Location (..), Agent (..))

instance Num Location where
  (+) (Location (x,y)) (Location (a,b)) = Location (x+a, y+b)
  (*) (Location (x,y)) (Location (a,b)) = Location (x*a, y*b)
  abs (Location (x,y)) = Location (abs x, abs y)
  signum (Location (x,y)) = Location (signum x, signum y)
  fromInteger int = Location (fromInteger int, fromInteger int)
  negate (Location (x,y)) = Location (negate x, negate y)
  
randomLoc :: Dims -> IO Location
randomLoc (Dims (height, width)) = do
  x <- randomRIO (0, width)
  y <- randomRIO (0, height)
  return $ Location (x,y)

randomNormalLoc :: IO Location
randomNormalLoc = do
  (rx:ry:_) <- normalsIO :: IO [Double]
  return $ Location (rx,ry)
  
norm :: Location -> Double
norm loc = let (Location (x2,y2)) = loc * loc in sqrt (x2+y2)

locatan2 :: Location -> Double
locatan2 (Location (x,y)) = atan2 y x

global2Body :: Agent -> Location -> Location
global2Body agent (Location (x,y)) = Location (x',y')
  where
    (c,s) = let p = (-1) * psi agent in (cos p, sin p)
    x' = c * x - s * y
    y' = s * x + c * y
    

body2Global :: Agent -> Location -> Location
body2Global agent (Location (x,y)) = Location (x',y')
  where
    (c,s) = let p = psi agent in (cos p, sin p)
    x' = c * x - s * y
    y' = s * x + c * y
