module Lib
    ( aligned
    , getAlignmentScores
    , objectiveAlignments
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
    -- Get the difference in g for the set of steps being considered
    g_init = g init -- 
    g_nexts = map g nextWorlds -- [g(1), g(2)]
    g_diffs = map (\x -> x - g_init) g_nexts :: [Double]-- [g(1) - g(

    -- Get the difference in obj for the set of steps being considered, for all obj
    o_inits = map ($ init) objs
    --o_nexts = map (\obj -> map obj nextWorlds) objs
    o_nexts = map (\w -> map ($ w) objs) nextWorlds
    o_diffs = map (\on -> zipWith (-) on o_inits) o_nexts :: [[Double]]

    -- [ [d_o1(s1), d_o2(s1)], [d_o1(s2), ..
    alignments = map (\(g_i, all_objs) -> map (alignment g_i) all_objs) (zip g_diffs o_diffs)


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
alignment :: Double -> Double -> Alignment
alignment x y
  | x >  0 && y >  0 = Alignment 1
  | x >  0 && y == 0 = Alignment 2
  | x >  0 && y <  0 = Alignment 3
  | x == 0 && y >  0 = Alignment 4
  | x == 0 && y == 0 = Alignment 5
  | x == 0 && y <  0 = Alignment 6
  | x <  0 && y >  0 = Alignment 7
  | x <  0 && y == 0 = Alignment 8
  | x <  0 && y <  0 = Alignment 9


uniformWeight :: AlignmentScore
uniformWeight = map uniformAlign
  where
    uniformAlign :: [Bool] -> Double
    uniformAlign a = (fromIntegral (length $ filter id a)) / (fromIntegral (length a))

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


    





    
