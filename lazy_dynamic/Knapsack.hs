module Knapsack where
import Data.Array((!))
import qualified Data.Array as A

type Value = Int
type Values = [Value]

type Weight = Int
type Weights = [Weight]

{--
 | Plain recursive implementation.
 --}
naiveKnapsack :: Weight -> Weights -> Values -> Value
naiveKnapsack 0 _ _  = 0
naiveKnapsack wt wts vals
    | length wts /= length vals = error "Mismatching lengths."
    | length wts == 0 = 0
    | last wts > wt = naiveKnapsack wt (init wts) (init vals)
    | otherwise = max includingFinalElement ignoringFinalElement
        where initVals              = init vals
              lastValue             = last vals
              newWeight             = wt - (last wts)
              initWeights           = init wts
              ignoringFinalElement  = naiveKnapsack wt initWeights initVals
              includingFinalElement = lastValue + 
                  (naiveKnapsack newWeight initWeights initVals)
              
{--
 | Improve on naive solution by using a recursively defined array for dynamic
 | programming.
 --}
basicKnapsack :: Weight -> Weights -> Values -> Value
basicKnapsack wt wts vals
    | length wts /= length vals = error "Mismatching lengths."
    | otherwise = d n wt
        where n      = length vals
              wts'   = A.listArray (0, n-1) wts
              vals'  = A.listArray (0, n-1) vals
              bounds = ((0, 0), (n, wt))

              d 0 j = 0
              d i 0 = 0
              d i j
                  | j < wts' ! (i-1) = ds ! (i-1, j)
                  | otherwise = max a b
                      where a = vals' ! (i-1) + ds ! (i-1, (j - wts' ! (i-1)))
                            b = ds ! (i-1, j)

              ds :: A.Array (Int, Weight) Value
              ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]
