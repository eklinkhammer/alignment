{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Objective
import World
import Control.Monad
import Types
import Location

import GHC.Generics
import Data.Aeson (FromJSON(..), withObject, withText, (.:), (.:?), (.!=))
import Data.Yaml (decodeEither)
import Data.Text (Text)
import Control.Applicative
import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.Simple

showPoints = putStrLn . show . extractPoints


main :: IO ()
main = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "postgres"
    }

  putStrLn "3 + 5"
  mapM_ print =<< (query conn "select ? + ?" (3 :: Int, 5 :: Int) :: IO [Only Int])
  -- init <- randomWorld :: IO World
  -- next <- replicateM 10 randomWorld :: IO [World]
  -- let g    = global 4 3 :: Objective
  --     obj  = onePOI 4   :: Objective
  --     objs = [obj, obj]      :: [Objective]
  --     x    = objectiveAlignments init g next objs
  -- putStrLn $ show x
  -- putStrLn "Hello World"
  -- putStrLn $ show init
  -- putStrLn $ show $ extractPoints init
  -- contents <- B.readFile "test.yaml"
  -- let parsedContent = decodeEither contents :: Either String World
  -- case parsedContent of
  --   (Left str) -> error str
  --   (Right c)  -> do
  --     world <- initWorld c
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
