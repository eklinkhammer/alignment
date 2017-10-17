module World
  (
    randomWorld
  , perturbWorld
  , extractPoints
  ) where


import Types
import Agent
import Location

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (groupBy, sortOn)

randomWorld :: IO World
randomWorld = do
  width  <- randomRIO (10,100)
  height <- randomRIO (10,100)

  numAgents <- randomRIO (4,10)
  numPOIs   <- randomRIO (4,10)
  
  let dims = Dims (height, width)
  agents <- replicateM numAgents (randomAgent dims)
  pois   <- replicateM numPOIs (randomPOI dims)

  return (World dims agents pois)

randomPOI :: Dims -> IO POI
randomPOI dims = do
  loc <- randomLoc dims
  val <- randomRIO (1,10)
  return (POI loc val 4 1)
  
-- | World changes. This determines which states are adjacent to each other. 
perturbWorld :: World -> IO World
perturbWorld world = do
  newAgents <- mapM perturbAgent (agents world)
  return $ world { agents = newAgents }

-- | An instance of feature extraction from a world. A Point for each Agent in the
--     the rover domain
extractPoints :: FeatureExtractor
extractPoints world = map (agentState world) (agents world)

agentState :: World -> Agent -> Point
agentState world agent = eachQuadVal
  where
    poiQuadScores = map (poiQuadScore agent) (pois world)
    agentQuadScores = map (agentQuadScore agent) (filter (/= agent) (agents world))
    quadScores = poiQuadScores ++ agentQuadScores ++ map (\i -> (0,i)) [0..7]
    inQuadOrder = sortOn snd quadScores
    eachQuad = groupBy (\a b -> snd a == snd b) inQuadOrder
    eachQuadValsOnly = map (map fst) eachQuad
    eachQuadVal = map sum eachQuadValsOnly
    


agentQuadScore :: Agent -> Agent -> (Double, Int)
agentQuadScore agent other  = quad agent (location other)

poiQuadScore :: Agent -> POI -> (Double, Int)
poiQuadScore agent poi = (value * (pValue poi), quadrant + 4)
  where
    (value, quadrant) = quad agent (pLocation poi)

quad :: Agent -> Location -> (Double, Int)
quad agent loc = (value, quadrant)
  where
    locv = global2Body agent (loc - (location agent))
    diff = (location agent) - locv
    theta = locatan2 diff
    diffNorm = norm diff
    quadrant = angleQuad theta
    value = 1.0 / (max diffNorm 1.0)
    
angleQuad :: Double -> Int
angleQuad theta
  | theta >= pi / 2.0 = 3
  | theta >= 0        = 0
  | theta >= (-1) * pi / 2.0 = 1
  | otherwise = 2
