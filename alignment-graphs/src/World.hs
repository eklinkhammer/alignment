module World
  (
    randomWorld
  , perturbWorld
  , extractPoints
  , dims
  , World (..)
  , initWorld
  , resetWorld
  ) where


import Types
import Agent
import Location
import POI (initPOI)

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (groupBy, sortOn)

randomWorld :: IO World
randomWorld = do
  width  <- randomRIO (10,30) >>= return . Width
  height <- randomRIO (10,30) >>= return . Height

  numAgents <- randomRIO (4,10)
  numPOIs   <- randomRIO (4,10)

  let dims = Dims (height, width)
  agents <- replicateM numAgents (randomAgent dims)
  pois   <- replicateM numPOIs (randomPOI dims)

  return (World width height numAgents numPOIs agents pois)

resetWorld :: World -> IO World
resetWorld world = do
  newAgents <- replicateM (_numAgents world) (randomAgent (dims world))
  newPOIs <- mapM (randomizePOI (dims world)) (pois world)
  let worldWithAgents = world { agents = newAgents }
  return $ worldWithAgents { pois = newPOIs }
  
randomPOI :: Dims -> IO POI
randomPOI dims = do
  loc <- randomLoc dims
  val <- randomRIO (1,10)
  return (POI loc val 4 1)

randomizePOI :: Dims -> POI -> IO POI
randomizePOI dims p = do
  loc <- randomLoc dims
  val <- randomRIO (1,10)
  return (POI loc val (pCoupling p) (pObservationRadius p))

initWorld :: World -> IO World
initWorld (World (Width w) (Height h) nA nP as ps) = do
  h' <- if h < 0 then randomRIO (10, 100) >>= return . Height else return $ Height h
  w' <- if w < 0 then randomRIO (10, 100) >>= return . Width else return $ Width w
  
  let nA_' = if nA == (-1) then length as else nA
      nP_' = if nP == (-1) then length ps else nP
      as_ = take nA_' $ as ++ (repeat notInitAgent)
      ps_ = take nP_' $ ps ++ (repeat notInitPOI)
      dims_ = Dims (h', w')
      
  as_' <- mapM return as_ -- TODO
  ps_' <- mapM (initPOI dims_) ps_

  return $ World w' h' nA_' nP_' as_' ps_'
  
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
    theta = locatan2 locv
    diffNorm = norm diff
    quadrant = angleQuad theta
    value = 1.0 / (max diffNorm 1.0)
    
angleQuad :: Double -> Int
angleQuad theta
  | theta >= pi / 2.0 = 3
  | theta >= 0        = 0
  | theta >= (-1) * pi / 2.0 = 1
  | otherwise = 2


dims :: World -> Dims
dims w = Dims (height w, width w)
