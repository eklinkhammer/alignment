module Simulation
  (
    simFitness
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

simFitness :: NN n => Objective -> Int -> World -> [n] -> (World, [Double])
simFitness obj n world nets = let w = simulation n world nets
                                  g = obj w
                              in (w, replicate (_numAgents world) g)

simulation :: NN n => Int -> World -> [n] -> World
simulation 0 world nets = world
simulation n world nets = simulation (n-1) (simStep world nets) nets

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
fromVec vec = Location (vec ! 0) (vec ! 1)
