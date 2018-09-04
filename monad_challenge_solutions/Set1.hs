{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Set1 where
import MCPrelude

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f ga s0 = (f x, s1)
    where (x, s1) = ga s0

randEven :: Gen Integer
randEven = generalA (2*) rand

randTen :: Gen Integer
randTen = generalA (10*) rand

randOdd :: Gen Integer
randOdd = generalA ((1+) . (2*)) rand

fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
    where (a, s1) = rand $ mkSeed 1
          (b, s2) = rand s1
          (c, s3) = rand s2
          (d, s4) = rand s3
          (e, _)  = rand s4

randLetter :: Gen Char
randLetter = generalA toLetter rand

randString3 :: String
randString3 = [a, b, c]
    where (a, s1) = randLetter $ mkSeed 1
          (b, s2) = randLetter s1
          (c, _)  = randLetter s2

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s0 = ((x, y), s2)
    where (x, s1) = ga s0
          (y, s2) = gb s1

randPair = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s0 = (f x y, s2)
    where (x, s1) = ga s0
          (y, s2) = gb s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

randPair' = generalPair2 randLetter rand

repRandom :: [Gen a] -> Gen [a]
repRandom [] s0 = ([], s0)
repRandom (g:gs) s0 = ((x:xs), sf)
    where (x, s1)  = g s0
          (xs, sf) = repRandom gs s1

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f s0 = (f x) s1
    where (x, s1) = ga s0

mkGen :: a -> Gen a
mkGen a = \s0 -> (a, s0)

-- Below is added for exercise set 4
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = 
    genTwo ga (\aa -> 
        genTwo gb (\bb ->
            mkGen $ f aa bb))

generalPair2' :: Gen a -> Gen b -> Gen (a, b)
generalPair2' = generalB2 (,)

randPair'' = generalPair2' randLetter rand

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] = mkGen []
repRandom2 (g:gs) = 
    genTwo g (\g' ->
        genTwo (repRandom2 gs) (\gs' ->
            mkGen (g':gs')))

repRandom3 :: [Gen a] -> Gen [a]
repRandom3 [] = mkGen []
repRandom3 (g:gs) = generalB2 (:) g (repRandom3 gs)

