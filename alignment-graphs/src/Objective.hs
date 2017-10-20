module Objective
  (
    global
  , infinitePOIs
  , onePOI
  , team
  , explore
  ) where

import Data.List

import Types
import World
import Location

-- General Notes
-- These are being used in one step processes, and being compared only to other applications of
--   these same functions on worlds and pois that, aside from agent position, are identical.
-- These relaxations remove the need to normalize any value, as well as remove the need
--   to preserve state between steps


-- | Classic rover domain. For a given coupling value, the sum of all POIs' values
--     that have sufficient rovers to satisfy the coupling requirement. The values
--     are weighted by the inverse of the average agent distance to the POIs. Only
--     the closest agents (to satisfy the coupling requirement) count.
global :: ObservationRadius -> Coupling -> Objective
global r coupling world = sum $ map (poiValue r coupling agentLocs) (pois world)
  where
    agentLocs = map location (agents world)

-- | A greedy rover domain. For all agents, they score for all pois if they are
--     within the observation radius.
infinitePOIs :: ObservationRadius -> Objective
infinitePOIs r world = sum $ map (agentValue r ps) (agents world)
  where
    ps = pois world
    
agentValue :: ObservationRadius -> [POI] -> Agent -> Double
agentValue r allPs agent = sum $ map (\p -> getValue r 1 (pValue p) p [agent]) allPs

-- | A partially greedy take on the rover domain. Each agent can only score with the
--     closest POI, but it ignores other agents.
onePOI :: ObservationRadius -> Objective
onePOI r world = sum $ map (\a -> agentValue r (closestN 1 a ps) a) (agents world)
  where
    ps = pois world

-- | Team forming domain. For a given team size, the sum of all agents that have
--     successfully formed teams. The score is weighted by the inverse of the average
--     distance from the agent to its closest team members (larger groups only count the
--     closest agents up to the team size). 
team :: TeamRadius -> TeamSize -> Objective
team radius size world =
  let as = agents world
  in sum $ map (\a -> getValue radius size 1.0 a (filter (/= a) as)) as

getValue :: (Located a, Located b) => Double -> Int -> Double -> a -> [b] -> Double
getValue radius required value x xs = val
  where
    norms = map (distance x) xs
    within = filter (<= radius) norms
    coupling = take required $ sort within
    totalDist = sum coupling
    val = let numSatisfy = length coupling
          in if numSatisfy < required then 0
             else value / (max 1.0 (totalDist / (fromIntegral numSatisfy)))

closestN :: (Located a, Located b) => Int -> b -> [a] -> [a]
closestN n one = take n . sortOn (distance one)

within :: (Located a, Located b) => Double -> a -> [b] -> [b]
within r x = filter (\y -> distance x y <= r)

-- | Exploration domain. For a given repulsion factor, the sum of all agents that
--     have moved beyond a certain radius from that number of other agents. Agents
--     still within that radius receive a scaled reward based on the average distance
--     to the furthest agents beyond the repulsion factor.
explore :: TeamRadius -> TeamSize -> Objective
explore radius size world =
  let as = agents world
  in sum $ map (\a -> 1 - (getValue radius size 1.0 a (filter (/= a) as))) as

poiValue :: ObservationRadius -> Coupling -> [Location] -> POI -> Double
poiValue radius coupling locs p =  value
  where
    norms = let pLoc = pLocation p in map (\l -> norm (pLoc - l)) locs
    withinRad = filter (\n -> n <= radius) norms :: [Double]
    couplingReq = take coupling $ sort withinRad
    totalDist = sum couplingReq
    value = let numSatisfy = length couplingReq
            in if numSatisfy < coupling then 0
               else (pValue p) / (max 1.0 (totalDist / (fromIntegral numSatisfy)))
                    

type TeamSize = Int

type TeamRadius = Double
