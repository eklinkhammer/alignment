{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Objective
import World
import Control.Monad
import Types
import Location
import Simulation

import System.Random

import GHC.Generics
import Data.Aeson (FromJSON(..), withObject, withText, (.:), (.:?), (.!=))
import Data.Yaml (decodeEither)
import Data.Text (Text)
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple
import Data.Trees.KdTree as K
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

import NN.NeuralNetwork
import CCEA

nnVars :: NNVars
nnVars = Map.fromList [("numberInputs", 8)
                      ,("numberHidden", 1)
                      ,("numberOutputs",2)
                      ,("timesToTrain",1)
                      ,("learningRate",0.25)
                      ,("sigmoidOrTanh",0)
                      ,("randomLowerBound", (-2))
                      ,("randomUpperBound", 2)
                      ,("gaussianMean", 0)
                      ,("gaussianStdDev", 1)
                      ,("mutationRate", 0.1)]

fitness :: NN n => Objective -> CCEAFitnessFunction n World
fitness obj = simFitness obj 1

breeding :: (RandomGen g, NN n) => BreedingStrategy n g
breeding = mutateWeights

breedingStrat :: (RandomGen g, NN n) => BreedingStrategy n g
breedingStrat g = elitist breeding g

agentCCEA :: RandomGen g => Objective -> World -> IO (CCEA Net World g)
agentCCEA obj w = do
  pop <- createPopulation (_numAgents w) 1 nnVars
  return $ CCEA pop (fitness obj) breedingStrat (tournament 2) w
  
showPoints = putStrLn . show . extractPoints

-- True if a generated point has the same objective selectede as its nearest neighbor
-- True also if the generated point selected an objective with the same alignment score (can't
-- be penalizing for ties)
testPoint tree g objs = do
  testW      <- randomWorld
  testKPoint <- generateKPoints extractPoints 1 g objs testW
  
  let p        = head testKPoint
      neighbor = nearestNeighbor tree p
      
  return $ case neighbor of
             Nothing -> False
             (Just x) -> whichObj p == whichObj x || let (KPoint _ as) = x
                                                         (KPoint _ bs) = p
                                                     in as !! (whichObj x) == bs !! (whichObj p)

testTree tree g objs = do
  test <- replicateM 100 (testPoint tree g objs)
  let score = length $ filter id test
  return (fromIntegral score / 100.0)

bestPolicyWithScore ccea obj w = do
  putStr "Objective value before running any policies: "
  putStrLn $ show (obj w)
  let teams = transpose (_pop ccea)
      scores = map (head . snd . simFitness obj 10 w) teams
  putStrLn $ show scores
  return $ head $ reverse $ sortOn snd (zip teams scores)

out s = putStrLn . ((++) s) . show . snd 
main :: IO ()
main = do
  let g    = global 6 1 :: Objective
      obj  = onePOI 4   :: Objective
      obj2 = team 4 1  :: Objective
      obj3 = explore 4 1 :: Objective
      obj4 = infinitePOIs 4 :: Objective
      objs = [obj, obj2, obj3, obj4] :: [Objective]

  contents <- B.readFile "test.yaml"
  let parsedContent = decodeEither contents :: Either String World
  case parsedContent of
    (Left str) -> error str
    (Right c)  -> do
      world <- initWorld c
      --world <- resetWorld world_i
      cceaObj <- agentCCEA g world :: IO (CCEA Net World StdGen)
      gen <- getStdGen
      let (gen', cceaObjT) = evolveNCCEA 1 gen cceaObj
      putStrLn $ show world
      bestPolicyWithScore cceaObj g world >>= out "After objective: "
      bestPolicyWithScore cceaObjT g world >>= out "After training, post policy: "
      putStrLn $ show $ map (map getWeights) (_pop cceaObj)
      putStrLn $ show $ map (map getWeights) (_pop cceaObjT)
      -- cceaObj3 <- agentCCEA obj3 world :: IO (CCEA Net World StdGen)
      -- gen <- getStdGen
      -- let (gen', cceaObj3T) = evolveNCCEA 20 gen cceaObj3
      -- bestPolicyWithScore cceaObj3 obj3 world >>= putStrLn . show . snd
      -- bestPolicyWithScore cceaObj3T obj3 world >>= putStrLn . show . snd
      
  -- putStrLn "Success Rate: 10 worlds with 20 wiggles"
  -- tree10 <- generateTree extractPoints g objs 10 20
  -- testTree tree10 g objs >>= putStrLn . show
  
  -- putStrLn "Success Rate: 100 worlds with 10 wiggles"
  -- tree20 <- generateTree extractPoints g objs 100 20
  -- testTree tree20 g objs >>= putStrLn . show

  -- putStrLn "Success Rate: 1000 worlds with 10 wiggles"
  -- tree50 <- generateTree extractPoints g objs 1000 20
  -- testTree tree50 g objs >>= putStrLn . show

  -- putStrLn "Success Rate: 2000 worlds with 10 wiggles"
  -- tree100 <- generateTree extractPoints g objs 2000 20
  -- testTree tree100 g objs >>= putStrLn . show

  -- putStrLn "Success Rate: 5000 worlds with 10 wiggles"
  -- tree200 <- generateTree extractPoints g objs 5000 20
  -- testTree tree200 g objs >>= putStrLn . show

  -- putStrLn "Success Rate: 10000 worlds with 10 wiggles"
  -- tree1000 <- generateTree extractPoints g objs 10000 20
  -- testTree tree1000 g objs >>= putStrLn . show

  -- init <- randomWorld :: IO World
  -- next <- replicateM 10 randomWorld :: IO [World]

  --     x    = objectiveAlignments init g next objs
  -- putStrLn $ show x
  -- putStrLn "Hello World"
  -- putStrLn $ show init
  -- putStrLn $ show $ extractPoints init

