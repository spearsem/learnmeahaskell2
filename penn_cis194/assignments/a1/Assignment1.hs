module Assignment1 where

-- Credit Card checksum problems
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev i
    | i <= 0    = []
    | otherwise = (i `mod` 10) : (toDigitsRev (i `div` 10))  

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . (zipWith id (cycle [id, (*2)])) . reverse

sumList :: Num a => [a] -> a
sumList = foldl (+) 0 

sumDigits :: [Integer] -> Integer
sumDigits = sumList . (>>= toDigits)

{--
 | Here is an alternate way to achieve sumDigits if you don't
 | already know about the list monad:
 |
 | -- Takes an individual integer to its digits and sums them. 
 | sumDigitsInteger :: Integer -> Integer
 | sumDigitsInteger = sumList . toDigits
 |
 | sumDigits :: [Integer] -> Integer
 | sumDigits = sumList . (map sumDigitsInteger)
 --}

isZeroModTen :: Integer -> Bool
isZeroModTen = (==0) . (`mod` 10)

validate :: Integer -> Bool
validate = isZeroModTen . sumDigits . doubleEveryOther . toDigits


-- Towers of Hanoi problems
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = moveTopToC ++ moveBottom ++ moveTopToB
    where moveTopToC = hanoi (n-1) a c b
          moveBottom = hanoi 1 a b c
          moveTopToB = hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n <= 2    = hanoi n a b c
    | otherwise = moveTopToD ++ moveBottom2 ++ moveTopToB
        where moveTopToD  = hanoi4 (n-2) a d b c
              moveBottom2 = hanoi4 2 a b c d
              moveTopToB  = hanoi4 (n-2) d b a c

hanoi4' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4' n a b c d
    | n <= 2    = hanoi n a b c
    | otherwise = moveTopKToD ++ moveBottom ++ moveTopKToB
        where k           = n `div` 2
              moveTopKToD = hanoi4' k a d b c
              moveBottom  = hanoi  (n - k) a b c
              moveTopKToB = hanoi4' k d b a c

-- Final method uses optimal choice of k.
hanoi4'' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4'' n a b c d
    | n <= 2    = hanoi n a b c
    | otherwise = moveTopKToD ++ moveBottom ++ moveTopKToB
        where k           = n - ((round . sqrt . fromIntegral) (2*n + 1)) + 1
              moveTopKToD = hanoi4'' k a d b c
              moveBottom  = hanoi (n - k) a b c
              moveTopKToB = hanoi4'' k d b a c
              

