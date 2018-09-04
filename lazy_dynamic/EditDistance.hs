module EditDistance where
import Data.Array((!))
import qualified Data.Array as A

{--
 | This code is modified directly from the blog post ``Lazy Dynamic 
 | Programming`` retrieved on January 2, 2016, from 
 | < http://jelv.is/blog/Lazy-Dynamic-Programming/ >.
 --}



{--
 | Slow recursive implementation of string edit distance. In this
 | algorithm, the helper function `d` is not memoized or cached, so
 | each recursive call must recompute results of `d` for earlier
 | substrings. This results in an exponential running time.
 --}
naiveEditDistance :: String -> String -> Int
naiveEditDistance a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = d (i - 1) (j - 1)
          | otherwise = minimum [ 1 + d (i - 1) j
                                , 1 + d i (j - 1) 
                                , 1 + d (i - 1) (j - 1)
                                ]

{--
 | In this case, use a recursively defined Array as a means for
 | caching the results of intermediate calculations. Under the hood
 | because of the way Haskell automatically deals with the evaluation
 | of lazy computations, it means each substring's edit distance will
 | only be calculated one time.
 --}
basicEditDistance :: String -> String -> Int
basicEditDistance a b = d m n
  where (m, n) = (length a, length b)
        d i 0  = i
        d 0 j  = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
          | otherwise = minimum [ 1 + ds ! (i - 1, j)
                                , 1 + ds ! (i, j - 1)
                                , 1 + ds ! (i - 1, j - 1)
                                ]

        bounds = ((0, 0), (m, n))
        ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]

{--
 | In the final version, all use of List and (!!) index access is replaced
 | by more robust Array indexing (!). The arrays are also set to be 1-indexed
 | which very slightly tweaks the formulas for reading from them.
 --}
betterEditDistance :: String -> String -> Int
betterEditDistance a b = d m n
  where (m, n) = (length a, length b)
        a'     = A.listArray (1, m) a
        b'     = A.listArray (1, n) b

        d i 0 = i
        d 0 j = j
        d i j
          | a' ! i ==  b' ! j = ds ! (i - 1, j - 1)
          | otherwise = minimum [ 1 + ds ! (i - 1, j)     
                                , 1 + ds ! (i, j - 1)     
                                , 1 + ds ! (i - 1, j - 1) 
                                ]

        bounds = ((0, 0), (m, n))
        ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]

