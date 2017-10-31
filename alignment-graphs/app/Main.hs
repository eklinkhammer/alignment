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
import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple
import Data.Trees.KdTree as K
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

import NN.NeuralNetwork
import CCEA

nnVars :: NNVars
nnVars = Map.fromList [("numberInputs", 2)
                      ,("numberHidden", 2)
                      ,("numberOutputs",1)
                      ,("timesToTrain",1)
                      ,("learningRate",0.25)
                      ,("sigmoidOrTanh",0)
                      ,("randomLowerBound", (-2))
                      ,("randomUpperBound", 2)
                      ,("gaussianMean", 0)
                      ,("gaussianStdDev", 1)
                      ,("mutationRate", 0.5)]

fitness :: NN n => CCEAFitnessFunction n World
fitness = simFitness (global 4 1) 20

breeding :: (RandomGen g, NN n) => BreedingStrategy n g
breeding = mutateWeights

breedingStrat :: (RandomGen g, NN n) => BreedingStrategy n g
breedingStrat g = notElitist breeding breeding g

agentCCEA :: RandomGen g => World -> IO (CCEA Net World g)
agentCCEA w = do
  pop <- replicateM (_numAgents w) (replicateM 2 (create nnVars) :: IO [Net])
  return $ CCEA pop fitness breedingStrat (tournament 2) w
  
showPoints = putStrLn . show . extractPoints


main :: IO ()
main = do
  -- conn <- connect defaultConnectInfo {
  --   connectDatabase = "postgres"
  --   }

  -- putStrLn "3 + 5"
  -- mapM_ print =<< (query conn "select ? + ?" (3 :: Int, 5 :: Int) :: IO [Only Int])

  let g    = global 4 3 :: Objective
      obj  = onePOI 4   :: Objective
      obj2 = team 4 2  :: Objective
      obj3 = explore 4 3 :: Objective
      obj4 = infinitePOIs 4 :: Objective
      objs = [obj, obj2, obj3, obj4] :: [Objective]
      
  kdtree <- generateTree extractPoints g objs 10 10

  testW <- randomWorld

  putStrLn "Test point (with calculated alignment values)"
  testKPoint <- generateKPoints extractPoints 10 g objs testW
  let p = head testKPoint
  putStrLn $ show p
  putStrLn "Nearest Point (with alignment values)"
  let neighbor = nearestNeighbor kdtree p
  case neighbor of
    Nothing -> putStrLn "No neighbor"
    (Just x) -> putStrLn $ show x

  -- init <- randomWorld :: IO World
  -- next <- replicateM 10 randomWorld :: IO [World]

  --     x    = objectiveAlignments init g next objs
  -- putStrLn $ show x
  -- putStrLn "Hello World"
  -- putStrLn $ show init
  -- putStrLn $ show $ extractPoints init
  -- contents <- B.readFile "test.yaml"
  let parsedContent = decodeEither contents :: Either String World
  case parsedContent of
    (Left str) -> error str
    (Right c)  -> do
      world <- initWorld c
      
  --     putStrLn $ show world
  --     showPoints world
  --     putStrLn $ show $ global 4 1 world
  --     putStrLn $ show $ onePOI 4 world
  --     putStrLn $ show $ infinitePOIs 4 world
  
-- do
--   let quads = map (snd . quad agent) [(1,1), (-1,1), (-1,-1), (1,-1)]
--   putStrLn $ show quads
--   randomWorld >>= putStrLn . show . extractPoints
--   --randomWorld >>= getAlignmentScores extractPoints
