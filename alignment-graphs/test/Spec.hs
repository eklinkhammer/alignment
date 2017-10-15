import Test.HUnit
import Lib
import Types

import Location_Test (allLocationTests)
allTests :: Test
allTests = TestList [ TestLabel "Quadrant Tests" testsQuadrant
                    , TestLabel "Location Tests" allLocationTests]

main :: IO Counts
main = runTestTT allTests

testsQuadrant :: Test
testsQuadrant = TestList [ TestLabel "No Relative Angle" testsQuadrantNoRelativeAngle]

testsQuadrantNoRelativeAngle = TestList [ TestLabel "Quadrant 1" testRelativeQuadOne]

-- No angle means a bearing of 180
agentNoAngle = Agent (0,0) 0

testQuadrantOne :: Test
testQuadrantOne = TestCase $ assertEqual "Loc in quadrant 1" 1 q
  where q = snd $ quad agentNoAngle (1,-1)

agentForward = Agent (0,0) (pi/2)

testRelativeQuadOne :: Test
testRelativeQuadOne = TestCase $ assertEqual "" 1 (snd $ quad agentForward (1,1))
