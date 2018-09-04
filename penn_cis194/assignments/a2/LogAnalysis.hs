{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char     (toLower)
import Data.List     (isInfixOf)
import Control.Monad (liftM2)

-- Parsing helpers
toInt :: String -> Maybe Int
toInt s = case reads s :: [(Int, String)] of
    [(n, "")] -> Just n
    _         -> Nothing

    
parseErrorMsg :: [String] -> LogMessage
parseErrorMsg ss
    | length ss >= 3 = let eCodeInt   = toInt $ ss !! 1
                           tStampInt  = toInt $ ss !! 2
                           parsedInts = liftM2 (,) eCodeInt tStampInt 
                       in 
        case parsedInts of
            Nothing       -> Unknown $ unwords ss
            (Just (e, t)) -> LogMessage (Error e) t $ (unwords . (drop 3)) ss           
    | otherwise = Unknown $ unwords ss


parseNonErrorMsg :: MessageType -> [String] -> LogMessage
parseNonErrorMsg msgType ss
    | length ss >= 2 = let parsedInt = toInt $ ss !! 1 
                       in 
        case parsedInt of
            Nothing  -> Unknown $ unwords ss
            (Just t) -> LogMessage msgType t $ (unwords . (drop 2)) ss           
    | otherwise = Unknown $ unwords ss


-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = let w = words s in
    case w of (x:_) -> case x of "E" -> parseErrorMsg w
                                 "I" -> parseNonErrorMsg Info w
                                 "W" -> parseNonErrorMsg Warning w
                                 _   -> Unknown $ unwords w
              _     -> Unknown $ unwords w 


parse :: String -> [LogMessage]
parse = (map parseMessage) . lines 


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) currentTree = currentTree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ tStamp _) (Node tL lm'@(LogMessage _ tStamp' _) tR)
    | tStamp < tStamp' = Node (insert lm tL) lm' tR
    | tStamp > tStamp' = Node tL lm' (insert lm tR)
    | otherwise        = error "Two messages can't have identical timestamps."


-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf 


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node tL lm tR) = (inOrder tL) ++ [lm] ++ (inOrder tR)
    

-- Exercise 5
getMessageString :: LogMessage -> String
getMessageString (Unknown m)        = m
getMessageString (LogMessage _ _ m) = m


detectError :: Int -> LogMessage -> Bool
detectError n (LogMessage (Error k) _ _) = k >= n
detectError _ _                          = False


getSortFilteredMessages :: (LogMessage -> Bool) -> [LogMessage] -> [String]
getSortFilteredMessages filterFn lms = map getMessageString msgs
    where msgs = inOrder . build . (filter filterFn) $ lms


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getSortFilteredMessages $ detectError 50


-- Exercise 6
detectWord :: String -> LogMessage -> Bool
detectWord s lm = isInfixOf s $ map toLower (getMessageString lm)


getFilteredMessages :: (LogMessage -> Bool) -> [LogMessage] -> [String]
getFilteredMessages filterFn lms = map getMessageString msgs
    where msgs = filter filterFn lms


whatWentMustard :: [LogMessage] -> [String]
whatWentMustard = getFilteredMessages $ detectWord "mustard"


