module Golf where
import Data.List (sort, group, intersect)
import Control.Applicative (pure, (<*>))

-- Exercise 1 (79 characters)

-- Helper function f is pretty self explanatory, it takes every "nth" element. 
-- skips works by using Applicative to generate the Cartesian product of
-- lengths, from 1 to length of input, with the input value itself, through
-- use of <*> to treat "all lengths" as arguments to f "simultaneously."

skips :: [a] -> [[a]]
skips a = pure f <*> [1..length a] <*> [a]
    where f 0 _ = []
          f n x = case drop (n-1) x of y:z -> y:f n z
                                       []  -> []
 


-- Exercise 2 (83 characters)

-- Here I break it down into the problem of checking > forwards
-- and checking > backwards in the list, then intersecting the results.
-- The helper function works by zipping with (>) the front of the list
-- with the tail of the list (the differently sized lists ensure zip only
-- gets the correct set of elements). This results in a list of Bools where
-- an element is "true" if it's greater than what follows it. The predicate
-- of the list comprehension then enforces that this is true and returns the
-- list of values. By doing this forward through the list, then backward
-- through the list, we ensure the intersection is just those elements that
-- are greater than both neighbors. The intersect function preserves order
-- based on the first list passed, so as long as this is the forward check,
-- the overall function will preserve order too.

localMaxima :: [Int] -> [Int]
localMaxima x = intersect (f x) $ f (reverse x)
    where f z = [y | (t, y) <- zip (zipWith (>) z (tail z)) z, t]



-- Exercise 3 (227 characters)

-- Helper function `f` "fuses" two strings together by perpetuating any '*'
-- characters that it sees. So `f "  *" "*"` would return "* *". This allows
-- "stacking" strings on top of each other. It's also used to "stack" 10
-- blanks initially, so that each string has length 10.

-- Helper function `m` makes a string line out of a list of positions of the
-- stars for that single line. It works by making a different string for each
-- position, with spaces followed by a single star, like "   *" and "  *", and
-- uses `f` to "stack" them into a single line of stars.

-- `h` does the work of recursing. `h` assumes it's given sorted and grouped 
-- integers from the overall list to plot. The portion `m (xs >>= take 1)` will
-- take one integer from each sorted group, and combine them into a single 
-- string fed into `m` to make a line. The recursion happens by pre-pending the 
-- result of another call to h, where one entry has been dropped from each sorted
-- subgroup and empty subgroups have been filtered out.

-- Overall, `histogram` works by using `group . sort` from Data.List to change a
-- list like [1,4,1,2,4,4,1] into [[1,1], [2], [4,4,4]], which is in the right
-- format to be given to the helper `h` -- and then it appends the desired
-- delimeters for the bottom of the histogram, the "==" and numeric stuff.

f :: String -> String -> String
f "" s = s
f s "" = s
f (a:b) (c:d) = (if elem '*' [a, c] then '*' else ' ') : f b d

m :: [Int] -> String
m = (foldl1 f) . map ((f "          ") . (++ "*") . (flip replicate $ ' '))

h :: [[Int]] -> String
h [] = ""
h x  = h (filter (not . null) (map (drop 1) x)) ++ "\n" ++ m (x >>= take 1)

histogram :: [Int] -> String
histogram = tail . (++ "\n==========\n0123456789\n") . h . group . sort  
