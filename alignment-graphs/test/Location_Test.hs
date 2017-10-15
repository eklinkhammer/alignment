module Location_Test
  (
    allLocationTests
  ) where

import Test.HUnit
import Types
import Location

allLocationTests :: Test
allLocationTests = TestList [TestLabel "Num Instance Tests" testsNumInstance
                            ,TestLabel "Reference frame tests" testsReference
                            ,TestLabel "Vector tests" testsVector
                            ,TestLabel "Random tests" testsRandom]

testsNumInstance :: Test
testsNumInstance = TestList []

testsReference :: Test
testsReference = TestList []

testsVector :: Test
testsVector = TestList []

testsRandom :: Test
testsRandom = TestList []
