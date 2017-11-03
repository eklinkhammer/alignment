{-# LANGUAGE FlexibleContexts #-}
module Simulation
  (
    simFitness
  , simStep
  ) where

import Types
import Agent
import Location
import POI
import World

import Control.Monad
import Data.List
import System.Random

import NN.NeuralNetwork
import Numeric.LinearAlgebra.HMatrix hiding (corr)

import Control.Monad.Writer
import System.IO.Unsafe

simFitnessWriter obj n world nets = do
  tell (show world)
  return (simFitness obj n world nets)

simFitness :: NN n => Objective -> Int -> World -> [n] -> (World, [Double])
simFitness obj n world nets = let (w,r) = unsafePerformIO $ do
                                    let (w',r') = simulation obj n world nets
                                    --putStrLn $ show r'
                                    return (w',r')
                              in (w, replicate (_numAgents world) r)

simulation :: NN n => Objective -> Int -> World -> [n] -> (World, Double)
simulation obj 0 world nets = (world, obj world)
simulation obj n world nets =
  let (w,r) = simulation obj (n-1) (simStep world nets) nets
  in (w, max r (obj world))

simStep :: NN n => World -> [n] -> World
simStep world nets = world { agents = newAgents }
  where
    inputs = extractPoints world
    moves = map (\(n,i) -> getMove n i) (zip nets inputs)
    newAgents = map (\(m,a) -> moveAgent a m) (zip moves (agents world))
    

getMove :: NN n => n -> Point -> Location
getMove net = fromVec . get net . toVec 

toVec :: Point -> Vector Double
toVec = fromList

fromVec :: Vector Double -> Location
fromVec vec = let l = Location (vec ! 0) (vec ! 1)
                  n = norm l
              in Location (x l / n) (y l / n)
