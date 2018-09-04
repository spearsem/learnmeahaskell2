{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Set2 where
import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

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
queryGreek gd s = case lookupMay s gd of
    Nothing -> Nothing
    Just xs -> case headMay xs of
        Nothing -> Nothing
        Just h  -> case tailMay xs of
            Nothing -> Nothing
            Just t  -> case maximumMay t of
                Nothing -> Nothing
                Just m  -> divMay (fromIntegral m) (fromIntegral h)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain 
 
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s = 
    (lookupMay s gd) `link` \vals -> 
    (headMay vals)   `link` \h ->
    (tailMay vals)   `link` \t -> 
    (maximumMay t)   `link` \m ->
    divMay (fromIntegral m) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals name1 name2 = 
    lookupMay name1 sals `link` \s1 ->
    lookupMay name2 sals `link` \s2 -> 
    Just (s1 + s2)

mkMaybe :: a -> Maybe a
mkMaybe a = Just a

yChain :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yChain f a b = 
    a `link` \a' -> 
    b `link` \b' ->
    mkMaybe $ f a' b'

yLink :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
yLink a b f = yChain f a b

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sals name1 name2 = yLink s1 s2 (+)
    where s1 = lookupMay name1 sals
          s2 = lookupMay name2 sals

tailProd :: Num a => [a] -> Maybe a
tailProd a = (tailMay a) `link` (mkMaybe . product)

tailSum :: Num a => [a] -> Maybe a
tailSum a = (tailMay a) `link` (mkMaybe . sum)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f a  = 
    a `link` \a' ->
    mkMaybe $ f a' 

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = (transMaybe product) . tailMay

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = (transMaybe sum) . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing    = Nothing
combine (Just val) = val 

tailMin :: Ord a => [a] -> Maybe a
tailMin = combine . (transMaybe minimumMay) . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = combine . (transMaybe maximumMay) . tailMay





