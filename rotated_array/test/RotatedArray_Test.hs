module RotatedArray_Test where
import Test.HUnit
import RotatedArray
import qualified Data.Vector as V

{---
 | Test checking list input for valid sort order.  
 ---}
isValidInput_Cases = TestLabel "Tests for isValidInput" (
    TestList [testValidEmpty, testCorrect, testIncorrect])

testValidEmpty = TestCase $ assertEqual 
  "Empty list evaluates as valid" 
  (True)
  (isValidInput [])

testCorrect = TestCase $ assertEqual 
  "Rotated/sorted input evaluates as valid"
  ((True, True))
  ((isValidInput [4, 1, 2, 3], isValidInput [1, 2, 3]))

testIncorrect = TestCase $ assertEqual 
  "Totally unsorted input evaluates as invalid"
  (False)
  (isValidInput [4, 2, 3, 1])

{---
 | Test creation of RotatedArray from list.  
 ---}
fromList_Cases = TestLabel "Tests for fromList" (
    TestList [testEmpty, testSorted])

testEmpty = TestCase $ assertEqual 
  "Should get RotatedArray [] from empty list" 
  (RotatedArray V.empty)
  (fromList 0 [])

testSorted = TestCase $ assertEqual 
  "Should get sorted RotatedArray from unsorted input"
  (fromList 0 [1, 2, 3, 4])
  (fromList 0 [4, 2, 3, 1])

{---
 | Test rotating a RotatedArray.
 ---}
rotateBy_Cases = TestLabel "Tests for rotateBy" (
    TestList [testRotateEmpty, testRotateZero, testRotateNegative])

testRotateEmpty = TestCase $ assertEqual 
  "Rotation should not change an Empty RotationArray" 
  (RotatedArray V.empty)
  (rotateBy 20 (RotatedArray V.empty))
 
testRotateZero = TestCase $ assertEqual 
  "Rotation by zero should not change anything" 
  (fromList 11 [1,2,3,4])
  (rotateBy 0 (fromList 11 [1,2,3,4]))

testRotateNegative = TestCase $ assertEqual 
  "Rotation by negative should work the same way" 
  (fromList 11 [1,2,3,4])
  (rotateBy 0 (fromList 11 [1,2,3,4]))

{---
 | Test computing the rotation of a RotatedArray.
 ---}
getRotation_Cases = TestLabel "Tests for getRotation" (
    TestList [testGetEmpty, testGetSorted, testGetUnsorted])

testGetEmpty = TestCase $ assertEqual
  "Rotation of empty RotatedArray should be 0" 
  (getRotation (fromList 0 []))
  (0)

testGetSorted = TestCase $ assertEqual 
  "Rotation of already sorted array should be 0" 
  (getRotation (fromList 0 [1, 2, 3, 5]))
  (0)

testGetUnsorted = TestCase $ assertEqual 
  "Rotation of test examples should be [1, 2]" 
  (map getRotation [fromList 1 [5, 1, 2, 3, 4], 
                    fromList 2 [4, 5, 1, 2, 3]])
  ([1, 2])

{---
 | Hook for running test suite.
 ---}
main = runTestTT $ TestList [isValidInput_Cases,
                             fromList_Cases, 
                             rotateBy_Cases, 
                             getRotation_Cases]
