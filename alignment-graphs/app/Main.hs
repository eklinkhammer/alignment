module Main where

import Lib

main :: IO ()
main = putStrLn "Hello World"
-- do
--   let quads = map (snd . quad agent) [(1,1), (-1,1), (-1,-1), (1,-1)]
--   putStrLn $ show quads
--   randomWorld >>= putStrLn . show . extractPoints
--   --randomWorld >>= getAlignmentScores extractPoints
