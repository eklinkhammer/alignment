{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Agent
  (
    randomAgent
  , perturbAgent
  , moveAgent
  ) where

import Types
import Location
import System.Random (randomRIO)

import GHC.Generics
import Data.Aeson (FromJSON(..), withObject, withText, (.:), (.:?), (.!=))
import Data.Yaml (decodeEither)
import Data.Text (Text)
import Control.Applicative


randomAgent :: Dims -> IO Agent
randomAgent dims = do
  loc <- randomLoc dims 
  h   <- randomRIO (-pi, pi)
  return (Agent loc h)

perturbAgent :: Agent -> IO Agent
perturbAgent a = randomNormalLoc >>= return . moveAgent a

moveAgent :: Agent -> Location -> Agent
moveAgent agent@(Agent location heading) cmd =
  let dv = body2Global agent cmd
      dpsi = locatan2 dv
      newLoc = location + dv
      newPsi = min (-pi) $ max pi (dpsi + heading)
  in Agent newLoc newPsi
