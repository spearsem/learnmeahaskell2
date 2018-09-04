module Solution where
import Ball

partition :: BallList -> (BallList, Int)
partition [] = ([], 0)
partition bs = let reds  = [ball | ball <- bs, color ball == Red] 
                   blues = [ball | ball <- bs, color ball == Blue]
               in (reds ++ blues, length reds)

partition' :: BallList -> (BallList, Int)
partition' [] = ([], 0)
partition' (x:xs) = let (pxs, idx) = partition' xs
                    in if (color x) == Red 
                        then (x:pxs, 1+idx)
                        else (pxs ++ [x], idx)
main = do
    let bs = [Ball Red, Ball Blue, Ball Red, Ball Blue]
    putStrLn $ show (partition bs)
    putStrLn $ show (partition [])
    putStrLn $ show (partition' bs)
    putStrLn $ show (partition' [])

{---
 | Notes:
 | There's a lot to say about possible solutions. But in general in
 | Haskell, if you are working with a [Ball] structure, and it is
 | lazy, then you can't easily get fancy with mutating the list or
 | sorting it in place. Doing something like passing over the list
 | and recording index positions of red and blue balls, to later
 | re-index on a permutation of the original indices that sends the
 | balls into contiguous order, is a bad idea. That last step of
 | re-indexing presumably with the (!!) operator will be very costly
 | and probably won't buy you much above two distinct O(n) passes,
 | as the list comprehensions perform above. The comprehension method
 | is idiomatic Haskell, readable, and nearly as efficient as anything
 | else you're going to do with native Lists. The other method provided
 | uses a fold-like recursive style, delaying either an append or a pre-
 | pend operation until the tail of the list has been processed. This
 | only makes one pass over the array, but it does build up a stack frame
 | of expensive append thunks that have to be unwound. I did not give
 | much thought to tail recursiveness, so perhaps you can do the recursive
 | solution with a great deal of optimization.
 ---}
