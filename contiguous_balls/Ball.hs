module Ball where

data Ball = Ball {color :: BallColor} deriving (Show, Eq)
data BallColor = Red | Blue deriving (Show, Eq)
type BallList = [Ball]
