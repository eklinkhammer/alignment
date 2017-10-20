{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson (FromJSON(..), withObject, withText, (.:), (.:?), (.!=))
import Data.Yaml
import Data.Text (Text)
import Control.Applicative


type Heading = Double
type Value' = Double
type FeatureExtractor = (World -> [Point])
type Objective = (World -> Double)
type Point = [Double]
type Mutation = (Point -> Point)
type AlignmentScore = ([[Bool]] -> [Double])
type ObservationRadius = Double
type Coupling = Int

data Location = Location { x :: Double, y :: Double } deriving (Generic, Show, Eq)
instance FromJSON Location

newtype Width = Width Double deriving (Eq, Show, Ord, Generic)
newtype Height = Height Double deriving (Eq, Show, Ord, Generic)
instance FromJSON Width
instance FromJSON Height

newtype Dims = Dims (Height, Width)
  deriving (Eq, Show)

newtype Alignment = Alignment Int
  deriving (Eq, Show)

class Located a where
  getLocation :: a -> Location

instance Located Agent where
  getLocation = location

instance Located POI where
  getLocation = pLocation

instance Located Location where
  getLocation = id
  
data Agent = Agent {
  location :: Location,
  psi :: Heading
  } deriving (Eq, Show)



instance FromJSON Agent where
  parseJSON = withObject "Agent" $ \v -> Agent
    <$> v .: "location"
    <*> ((v .: "psi") <|> (return 0))

data POI = POI {
    pLocation :: Location
  , pValue :: Value'
  , pCoupling :: Coupling
  , pObservationRadius :: ObservationRadius
  } deriving (Eq, Show)

instance FromJSON POI where
  parseJSON = withObject "POI" $ \v -> POI
    <$> (v .: "pLocation" <|> return (pLocation notInitPOI))
    <*> (v .: "pValue" <|> return (pValue notInitPOI))
    <*> (v .: "pCoupling" <|> return (pCoupling notInitPOI))
    <*> (v .: "pObservationRadius" <|> return (pObservationRadius notInitPOI))

notInitPOI = POI (Location (-1) (-1)) 0 0 0
notInitAgent = Agent (Location (-1) (-1)) 0

data World = World {
  width :: Width
  , height :: Height
  , _numAgents :: Int
  , _numPOIs :: Int
  , agents :: [Agent]
  , pois :: [POI]
  } deriving (Eq, Show, Generic)
  
instance FromJSON World where
  parseJSON = withObject "World" $ \v -> World
    <$> (v .: "width" <|> return (Width (-1)))
    <*> (v .: "height" <|> return (Height (-1)))
    <*> (v .: "numAgents" <|> return (-1))
    <*> (v .: "numPOIs" <|> return (-1))
    <*> (v .: "agents" <|> return [])
    <*> (v .: "pois" <|> return [])

data WorldSetup = WorldSetup {
  wsWidth :: Width
  , wsHeight :: Height
  , wsNumAgents :: Int
  , wsNumPOIs :: Int
  , wsCoupling :: Int
  }
