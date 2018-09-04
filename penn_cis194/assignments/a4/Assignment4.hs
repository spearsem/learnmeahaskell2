module Assignment4 where
import Data.List (sort)

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 $ 3 * n + 1


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate f 
    where f x | even x    = x `div` 2
              | otherwise = 3 * x + 1


-- Exercise 2

data Tree a = Leaf | Node Int (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
            
insert :: a -> Tree a -> Tree a
insert a Leaf                   = Node 0 Leaf a Leaf
insert a (Node _ Leaf v Leaf)   = Node 1 (insert a Leaf) v Leaf 
insert a (Node n Leaf v tRight) = Node n (insert a Leaf) v tRight
insert a (Node n tLeft v Leaf)  = Node n tLeft v (insert a Leaf)

insert a (Node n tLeft@(Node nL _ _ _) v tRight@(Node nR _ _ _)) 
    | nL < nR  = let newTLeft@(Node ntL _ _ _) = (insert a tLeft) 
                 in Node (1 + max ntL nR) newTLeft v tRight

    | otherwise = let newTRight@(Node ntR _ _ _) = (insert a tRight)
                  in Node (1 + max nL ntR) tLeft v newTRight 

-- alternate solution
foldTree' :: [a] -> Tree a
foldTree' [] = Leaf
foldTree' xs = Node (getHeight front back) front mid back
    where mid   = last xs
          vals  = init xs
          len   = length vals `div` 2
          front = foldTree' (take len vals)
          back  = foldTree' (drop len vals)
        
getHeight :: Tree a -> Tree a -> Int
getHeight Leaf Leaf = 0
getHeight Leaf tRight@(Node n tL v tR) = 1 + getHeight tL tR
getHeight tLeft@(Node n tL v tR) Leaf  = 1 + getHeight tL tR
getHeight tLeft tRight = max (getHeight tLeft Leaf) (getHeight tRight Leaf) 


-- Exercise 3

xor :: [Bool] -> Bool
xor xs = foldl h False xs
    where h x y = if y then not x && y else x 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr h []
    where h x y = (f x) : y

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = (foldr h id xs) base
    where h x g = \a -> g (f a x)

-- My own notes for how the solution works.
-- First do just one step of the helper function
--   (h x1 id) == \a -> id ( f a x1 ) 
-- Next imagine doing a second step.
--   (h x2 (\a -> id (f a x1))) == \a -> id (f (f a x1) x2)
--   ...
-- This results over all in:
--   \a -> id (f (f (f ... (f a x1) ... xN-2) xN-1) xN)  
-- and for a given argument a, this is the formula for foldl.


-- Exercise 4

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- no attempts to optimize the list comprehension.
sieveSundaram :: Int -> [Int]
sieveSundaram n = [2*v + 1 | v <- range, v `notElem` sieveVals]
    where range     = [1..n]
          sieveVals = [i + j + 2*i*j | (i, j) <- cartProd range range, 
                                       i <= j, 
                                       i + j + 2*i*j <= n]
