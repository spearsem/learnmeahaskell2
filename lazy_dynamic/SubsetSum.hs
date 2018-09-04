module SubsetSum where
import Data.Array((!))
import qualified Data.Array as A

{--
 | This code is my own implementation of the subset-sum algorithms, but it
 | is based on the edit distance implementations found at the blog post
 | ``Lazy Dynamic Programming`` retrieved on January 2, 2016, from 
 | < http://jelv.is/blog/Lazy-Dynamic-Programming/ >. The blog post is a
 | tutorial on the mutually recursive array and function used for doing
 | dynamic programming in a lazy functional language. 
 --}


{--
 | Slow recursive implementation of subset sum, which doesn't store
 | any intermediate calculations, and hence is exponential.
 --}
naiveSubsetSum :: [Int] -> Int -> Bool
naiveSubsetSum _ 0 = True 
naiveSubsetSum [] _ = False
naiveSubsetSum arr sum    
    | (last arr) > sum = naiveSubsetSum (init arr) sum  
    | otherwise = ignoringLastElement || includingLastElement
        where front = init arr
              ignoringLastElement  = naiveSubsetSum front sum
              includingLastElement = naiveSubsetSum front (sum - (last arr))


{--
 | Improve on naive solution by using a recursively defined array.
 --}
basicSubsetSum :: [Int] -> Int -> Bool
basicSubsetSum arr sum = d sum n
    where n      = length arr
          arr'   = A.listArray (0, n-1) arr
          bounds = ((0, 0), (sum, n))

          d :: Int -> Int -> Bool
          d i 0  = True
          d 0 j  = False
          d i j 
              | i > arr' ! (j-1) = ds ! (i, j-1)
              | otherwise = ignoringLastElement || includingLastElement
                  where ignoringLastElement  = ds ! (i, j-1)
                        includingLastElement = ds ! ((i - arr' ! (j-1)), (j-1))

          ds :: A.Array (Int, Int) Bool
          ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]
