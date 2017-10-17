module Main where

import Lib
import Objective
import World
import Control.Monad
import Types

main :: IO ()
main = do
  init <- randomWorld :: IO World
  next <- replicateM 10 randomWorld :: IO [World]
  let g    = global 4 3 :: Objective
      obj  = onePOI 4   :: Objective
      objs = [obj, obj]      :: [Objective]
      x    = objectiveAlignments init g next objs
  putStrLn $ show x
  putStrLn "Hello World"
  putStrLn $ show init
  putStrLn $ show $ extractPoints init
-- do
--   let quads = map (snd . quad agent) [(1,1), (-1,1), (-1,-1), (1,-1)]
--   putStrLn $ show quads
--   randomWorld >>= putStrLn . show . extractPoints
--   --randomWorld >>= getAlignmentScores extractPoints
