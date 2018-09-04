module RodCutting where
import Data.Array((!))
import qualified Data.Array as A

type Price = Int
type Prices = [Price]
type RodLength = Int

-- Helper for eager evaluation in folds. In the imperative solutions to
-- the rod cutting problem, loops are used, which will be replaced with
-- folds. This implementation is used from 
--     < https://wiki.haskell.org/Foldr_Foldl_Foldl' >
-- retrieved on January 3, 2016.
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z []     = z
foldl' f z (x:xs) = seq z' $ foldl' f z' xs
    where z' = z `f` x 
                    
{--
 | Plain recursive implementation. Each recursive call to naiveRodCutting 
 | results in a fold that will perform further calls to naiveRodCutting
 | until reaching the base case of a zero length rod. Thus, it is very
 | exponential.
 --}
naiveRodCutting :: Prices -> RodLength -> Price 
naiveRodCutting prc rl = foldl' (naiveRodCutting' prc rl) 0 [0..rl-1]  

{--
 | I use a helper function to store the state of "current price" and
 | "position" as part of the arguments. The only computation the helper
 | function does is to select the max between the current price and
 | the next calculated price, thus this can be used in a fold. However,
 | the next price in the where clause is where the recursion happens.
 --}
naiveRodCutting' :: Prices -> RodLength -> Price -> Int -> Price
naiveRodCutting' [] _ _ _ = 0
naiveRodCutting' prc rl curPrc pos = max curPrc newPrc
    where p      = prc !! pos
          newRl  = rl - pos - 1
          newPrc = p + naiveRodCutting prc newRl
              
{--
 | Improve on naive solution by using a recursively defined array for dynamic
 | programming. The overall solution is the function `v` evaluated on the rod
 | length. But v references entries in the array vs which in turn bottoms out
 | with base cases of function v. A helper function is used to allow for the
 | same fold styled computation, but not that instead of recursing, the helper
 | merely accesses previous calculations in vs.
 --}
basicRodCutting :: Prices -> RodLength -> Price
basicRodCutting prc rl = v rl
    where prc'         = A.listArray (0, rl-1) prc

          -- v carries out the work of the calculation by indexing into vs
          -- through the use of a helper function.
          v :: RodLength -> Price
          v 0          = 0
          v ii         = foldl' (helper ii) 0 [0..ii-1]

          -- Same as the outer loop bounds in an imperative implementation.
          bounds       = (0, rl)

          -- vs is the array recursively defined in terms of function v
          vs :: A.Array RodLength Price
          vs           = A.listArray bounds [v i | i <- A.range bounds]

          -- helper is used in a fold to compare the current max price with
          -- the price obtained via a recursive lookup into vs.
          helper i x j = max x (prc' ! j + vs ! (i - j - 1)) 

