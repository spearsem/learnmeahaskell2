{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Set4 where
import MCPrelude

-- Set two was imported during the initial work on Set4. But after the
-- monad instances and helpers are made, we must re-implement, so then
-- the definition for Maybe, Card, Gen, etc., are brought over here.
--import Set2

--link   :: Maybe a -> (a -> Maybe b) -> Maybe b
--genTwo ::   Gen a -> (a -> Gen b)   -> Gen b
----- generalizes to m a -> (a -> m b) -> m b

--yChain :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
--generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
----- generalizes to (a -> b -> c) -> m a -> m b -> m c

--mkMaybe :: a -> Maybe a
--mkGen :: a -> Gen a
----- generalizes to a -> m a

{---
 | Type and data declarations.
 ---}
newtype Gen s a = Gen (s -> (a, s))
getFunc :: Gen s a -> s -> (a, s)
getFunc (Gen g) = g

data Maybe a = Just a | Nothing
instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

data Card = Card Int String
instance Show Card where
    show (Card rank suit) = (show rank) ++ (stripQuotes $ show suit)
        where stripQuotes = (filter . flip notElem) "\"" 


{---
 | Monad and instances.
 ---}
class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad Maybe where
    return a = Just a
    bind ma f = case ma of
        Nothing  -> Nothing
        (Just x) -> f x

instance Monad [] where
    return a = [a]
    bind xs f = concat $ map f xs

instance Monad (Gen s) where
    return a = Gen (\s -> (a, s))
    bind ga f = Gen (\s -> let (x, s1) = getFunc ga s 
                           in getFunc (f x) s1)


{---
 | Monadic generalizations of helper functions.
 ---}

-- generalization of generalA
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma =
   ma `bind` \a ->
   return $ f a

-- generalization of generalB, yLink, and allCombs
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = 
    ma `bind` \a -> 
    mb `bind` \b -> 
    return $ f a b

-- generalization of allCombs3
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = 
    ma `bind` \a ->
    mb `bind` \b ->
    mc `bind` \c ->
    return $ f a b c

-- generalization of repRandom
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (g:gs) = liftM2 (:) g (sequence gs)

-- generalization of chain
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

-- generalization of combine
join :: Monad m => m (m a) -> m a
join mma = mma `bind` id

-- generalization of combStep
-- also equal to `liftM2 id`
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = 
    mf `bind` \f ->
    ma `bind` \a ->
    return $ f a

-- some helpers required for manipulating the state monad
put :: a -> Gen a ()
put a = Gen $ \_ -> ((), a)

get :: Gen s s 
get = Gen $ \s -> (s, s)

evalState :: Gen s a -> s -> a
evalState gsa s = fst $ getFunc gsa s

execState :: Gen s a -> s -> s
execState gsa s = snd $ getFunc gsa s 

{--
 | Reimplementation of Set1 exercises.
 --}

randGen :: Gen Seed Integer
randGen = Gen rand

randEven :: Gen Seed Integer
randEven = liftM (2*) randGen

randTen :: Gen Seed Integer
randTen = liftM (10*) randGen

randOdd :: Gen Seed Integer
randOdd = liftM ((1+) . (2*)) randGen

fiveRands :: [Integer]
fiveRands = evalState (sequence (replicate 5 randGen)) (mkSeed 1)

randLetter :: Gen Seed Char
randLetter = liftM toLetter randGen

randString3 :: String
randString3 = evalState (sequence (replicate 3 randLetter)) (mkSeed 1)

generalPair :: Gen Seed a -> Gen Seed b -> Gen Seed (a, b)
generalPair = liftM2 (,)

randPair = generalPair randLetter randGen

repRandom :: [Gen Seed a] -> Gen Seed [a]
repRandom [] = return []
repRandom (g:gs) = liftM2 (:) g (repRandom gs)


{--
 | Reimplementation of Set2 exercises.
 --}
 
headMay :: [a] -> Maybe a
headMay []     = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []     = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((xa, xb):xs) 
    | a == xa   = Just xb
    | otherwise = lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0   = Nothing
divMay a1 a2 = Just (a1 / a2)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []      = Nothing
maximumMay (x:xs)  = Just (foldl max x xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []      = Nothing
minimumMay (x:xs)  = Just (foldl min x xs)   
 
queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = 
    (lookupMay s gd) `bind` \vals -> 
    (headMay vals)   `bind` \h ->
    (tailMay vals)   `bind` \t -> 
    (maximumMay t)   `bind` \m ->
    divMay (fromIntegral m) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals name1 name2 = 
    lookupMay name1 sals `bind` \s1 ->
    lookupMay name2 sals `bind` \s2 -> 
    return (s1 + s2)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sals name1 name2 = liftM2 (+) s1 s2
    where s1 = lookupMay name1 sals
          s2 = lookupMay name2 sals

tailProd :: Num a => [a] -> Maybe a
tailProd = (liftM product) . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = (liftM sum) . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = join . (liftM minimumMay) . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = join . (liftM maximumMay) . tailMay

{---
 | Reimplementation of Set3 exercises
 ---}
allPairs :: [a] -> [b] -> [(a, b)]
allPairs = liftM2 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = (return f) `ap` as `ap` bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = (return f) `ap` as `ap` bs `ap` cs



