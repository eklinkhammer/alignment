module Agent
  (
    randomAgent
  , perturbAgent
  ) where

import Types
import Location
import System.Random (randomRIO)

randomAgent :: Dims -> IO Agent
randomAgent dims = do
  loc <- randomLoc dims 
  h   <- randomRIO (-pi, pi)
  return (Agent loc h)

perturbAgent :: Agent -> IO Agent
perturbAgent agent@(Agent location heading) = do
  rLoc <- randomNormalLoc
  let dv = body2Global agent rLoc
      dpsi = locatan2 dv
      newLoc = location + dv
      newPsi = min (-pi) $ max pi (dpsi + heading)
  return (Agent newLoc newPsi)

  


