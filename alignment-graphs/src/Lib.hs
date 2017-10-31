module Lib
    ( aligned
    , getAlignmentScores
    , objectiveAlignments
    , generateTree
    , generateKPoints
    ) where

import Data.Random.Normal
import System.Random
import Control.Monad
import Data.List


import Objective
import Types
import Location
import Agent
import World

import qualified Data.Vector as V
import qualified Data.Trees.KdTree as K

data KPoint = KPoint (V.Vector Double) [Alignment] deriving (Eq, Show)

-- | Hard coded KdTree instance
instance K.Point KPoint where
  dimension _ = 8
  coord i (KPoint vec _) = vec V.! i
  dist2 (KPoint vec1 _) (KPoint vec2 _) = sum $ V.map (\x -> x * x) $ V.zipWith (-) vec2 vec1

generateTree :: FeatureExtractor -> Objective -> [Objective]
  -> Int -- ^ Number of worlds to generate
  -> Int -- ^ Samples per world
  -> IO (K.KdTree KPoint)
generateTree feature g objs numWorlds numSamples = do
  inits <- replicateM numWorlds randomWorld :: IO [World]
  kpoints <- mapM (generateKPoints feature numSamples g objs) inits
  return $ K.fromList $ concat kpoints

generateKPoints :: FeatureExtractor -> Int -> Objective -> [Objective] -> World -> IO ([KPoint])
generateKPoints feature numSamples g objs init = do
  next <- replicateM numSamples (perturbWorld init)
  return $ makeKPoint $ aligned feature g init next objs
  
makeKPoint :: ([Point], [[Alignment]]) -> [KPoint]
makeKPoint (pts, alignments) =
  let a = map qalign alignments
  in map (\p -> KPoint (V.fromList p) a) pts

-- | Calculates, for a set of different worlds, the alignment of a set of
--     objectives.
--   Returns the points in feature space corresponding to the base world, and
--     a list of lists (one per world) of bools (one per objective) on whether
--     that objective is aligned with those points.
--   Note that all points in the world share the alignment.
aligned :: FeatureExtractor  -- ^ A means of extracting a state from the world
        -> Objective         -- ^ G (the objective function all others compare to)
        -> World             -- ^ Initial world whose state is collecting align data
        -> [World]           -- ^ Other world states that will be compared to orig
        -> [Objective]       -- ^ Objective functions being compared to G
        -> ([Point], [[Alignment]])
aligned feature g init nextWorlds objs = (feature init, alignments)
  where
    alignments = objectiveAlignments init g nextWorlds objs


-- | Calculates for each objective a list of alignments (number between 1-9 representing
--    the relationship of that function with g between two world states) for some set
--    of world states
objectiveAlignments
  :: World         -- ^ Initial world
  -> Objective     -- ^ Objective all others are comparing to
  -> [World]       -- ^ Sample of nearby states
  -> [Objective]   -- ^ Objectives being compared
  -> [[Alignment]] -- ^ Alignments, per objective, across the states
objectiveAlignments w_o g ws objs = aligns_t
  where
    -- For the initial world, g on that world
    r_g_wo  = g w_o
    -- For all sampled worlds, g on that world
    r_g_ws  = map g ws
    -- For all sampled worlds, for g, the difference between g on that world and
    --   the inital world
    dr_g_ws = map (\r_g_wi -> r_g_wi - r_g_wo) r_g_ws

    -- For the initial world, all objectives on that world
    r_os_wo = map ($ w_o) objs
    -- For all sampled worlds, all objectives on that world
    r_os_ws = map (\w_i -> map ($ w_i) objs) ws
    -- For all sampled worlds, for all objectives, the difference between that
    --   objective on that world and the initial world
    dr_os_ws = map (\r_os_wi -> zipWith (-) r_os_wi r_os_wo) r_os_ws

    -- For all sampled worlds, for all objectives, the alignment between the difference
    --   in that objective values between the intial world the sampled one and the
    --   difference in g's value between the initial world and the sampled one
    aligns = map (\(dr_g_wi, dr_os_wi) ->
                    map (\dr_oi_wi -> alignment dr_g_wi dr_oi_wi) dr_os_wi)
             (zip dr_g_ws dr_os_ws)

    -- For all objectives, for all sampled worlds, the alignment of that objective
    --   with g.
    aligns_t = transpose aligns

-- Alignment Scores. Ordered by relative goodness
alignment :: Double -> Double -> Alignment
alignment x y
  | x >  0 && y >  0 = Alignment 1
  | x >  0 && y == 0 = Alignment 2
  | x == 0 && y >  0 = Alignment 3
  | x == 0 && y == 0 = Alignment 4
  | x <  0 && y <  0 = Alignment 5
  | x <  0 && y == 0 = Alignment 6
  | x <  0 && y >  0 = Alignment 7 
  | x == 0 && y <  0 = Alignment 8
  | x >  0 && y <  0 = Alignment 9
  

uniformWeight :: AlignmentScore
uniformWeight = map uniformAlign
  where
    uniformAlign :: [Bool] -> Double
    uniformAlign a = (fromIntegral (length $ filter id a)) / (fromIntegral (length a))

compressAlignment :: Alignment -> Alignment -> Alignment
compressAlignment (Alignment x) (Alignment y) = Alignment (min x y)

qalign :: [Alignment] -> Alignment
qalign = foldr compressAlignment (Alignment 9)

-- | Maps from a world and a list of objectives to the list of states defined by
--     the world and the alignment that all of the objectives have in those states
--   Note that the objectives share the alignment score across the points
getAlignmentScores
  :: FeatureExtractor       -- ^ Map from world to state
  -> AlignmentScore         -- ^ Determine alignment from a sample of alignments
  -> Objective              -- ^ Objective all others will be aligned with
  -> World                  -- ^ Starting world (generates returned states)
  -> Int                    -- ^ Number of adjacent states to sample
  -> [Objective]            -- ^ Objectives measured
  -> IO ([Point], [[Alignment]]) -- ^ All points in start state with how aligned each
                            --     objective is with the global one in that state
getAlignmentScores featureMap scoring g init n objs = do
  adjacentWorlds <- replicateM n (perturbWorld init)
  return $ aligned featureMap g init adjacentWorlds objs

someFunc :: IO ()
someFunc = putStrLn "someFunc"


    





    
