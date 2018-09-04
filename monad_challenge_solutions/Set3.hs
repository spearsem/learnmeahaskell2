{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Set3 where
import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = (map ((,) x) ys) ++ (allPairs xs ys)

data Card = Card Int String

instance Show Card where
    show (Card rank suit) = (show rank) ++ (stripQuotes $ show suit)
        where stripQuotes = (filter . flip notElem) "\"" 

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = (map (Card x) ys) ++ (allCards xs ys) 

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) ys = (map (f x) ys) ++ (allCombs f xs ys)

allPairs2 :: [Int] -> [String] -> [(Int, String)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 _ _ [] _ = []
allCombs3 _ _ _ [] = []
allCombs3 f (x:xs) ys zs = 
    concat (map (\g -> map g zs) (map (f x) ys)) ++ 
    (allCombs3 f xs ys zs)

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) as = (map f as) ++ (combStep fs as)

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f as bs = (map f as) `combStep` bs

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f as bs cs = (map f as) `combStep` bs `combStep` cs

