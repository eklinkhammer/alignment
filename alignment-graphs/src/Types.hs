module Types where

type Heading = Double
type Value = Double
type FeatureExtractor = (World -> [Point])
type Objective = (World -> Double)
type Point = [Double]
type Mutation = (Point -> Point)
type AlignmentScore = ([[Bool]] -> [Double])
type ObservationRadius = Double
type Coupling = Int
newtype Location = Location (Double, Double)
  deriving (Eq, Show)

newtype Dims = Dims (Double,Double)
  deriving (Eq, Show)

newtype Alignment = Alignment Int
  deriving (Eq, Show)


data Agent = Agent {
  location :: Location,
  psi :: Heading
  } deriving (Eq, Show)

data POI = POI {
    pLocation :: Location
  , pValue :: Value
  , pCoupling :: Coupling
  , pObservationRadius :: ObservationRadius
  } deriving (Eq, Show)

data World = World {
  dims :: Dims
  , agents :: [Agent]
  , pois :: [POI]
  } deriving (Eq, Show)


