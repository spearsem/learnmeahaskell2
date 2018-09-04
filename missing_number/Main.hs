module Main where

{---
 | Given an array of the integers 1..n, with exactly one
 | missing, write a function that returns the missing number.
 |
 | The trick is to ignore the permutation aspect. All that
 | matters is the sum of the array, since the sum of the
 | integers 1..n must be n*(n+1)/2. The difference between
 | the two must be the missing number.
 ---}

missingInteger :: [Int] -> Int
missingInteger xs = (n * (n+1) `div` 2) - sum xs 
    where n = 1 + length(xs)

main = do
    let test0 = []                    -- Missing 1
    let test1 = [1, 2, 3, 4]          -- Missing 5
    let test2 = [2, 5, 4, 1]          -- Missing 3
    let test3 = [1, 8, 4, 6, 2, 3, 5] -- Missing 7

    mapM_ putStrLn $ map (show . missingInteger) [test0, test1, test2, test3] 
