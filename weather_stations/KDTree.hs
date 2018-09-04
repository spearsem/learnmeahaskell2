module KDTree (KDTree(KDEmpty, KDNode), 
               fromList, 
               nearestNeighbor) where

import Data.List (sortBy)

type KDId = Int
type KDDepth = Int
type KDCoordinate = [Float]
type KDPoint = (KDId, KDCoordinate)
type KDPointList = [KDPoint]
data KDTree = KDEmpty 
            | KDNode {idVal      :: KDId,
                      location   :: KDCoordinate,
                      splitVal   :: Float,
                      splitAxis  :: Int,
                      leftChild  :: KDTree, 
                      rightChild :: KDTree}
            deriving (Show, Eq)

type KDLine = (Int, Float)
type KDCircle = (KDCoordinate, Float)
    
-- Recursively create KD Tree from an input.
fromList :: KDPointList -> KDTree
fromList = fromList' 0

-- Find nearest node of a KDTree.
nearestNeighbor :: KDCoordinate -> KDTree -> KDPoint
nearestNeighbor [] _  = undefined

-- Base case when there are no child nodes.
nearestNeighbor _ (KDNode idVal loc _ _ KDEmpty KDEmpty) = (idVal, loc)

-- Base case when there is possible left child but no right child.
nearestNeighbor coord (KDNode iVal loc sVal sAx leftTree KDEmpty) =
    let curNN     = (iVal, loc)
        curCoord  = snd curNN
        line      = (splitAxis leftTree, splitVal leftTree)
        circle    = (curCoord, distance coord curCoord)
    in if possiblyIntersects line circle
        then closest coord curNN (nearestNeighbor coord leftTree)
        else curNN

-- Base case when there is possible right child but no left child.
nearestNeighbor coord (KDNode iVal loc sVal sAx KDEmpty rightTree) =
    let curNN     = (iVal, loc)
        curCoord  = snd curNN
        line      = (splitAxis rightTree, splitVal rightTree)
        circle    = (curCoord, distance coord curCoord)
    in if possiblyIntersects line circle
        then closest coord curNN (nearestNeighbor coord rightTree)
        else curNN

-- General case when there are non-empty left and right child nodes.
nearestNeighbor coord (KDNode iVal loc sVal sAx leftTree rightTree) = 
    let goLeft    = coord !! sAx <= sVal
        curTree   = if goLeft then leftTree else rightTree
        otherTree = if goLeft then rightTree else leftTree
        curNN     = closest coord (iVal, loc) (nearestNeighbor coord curTree)
        curCoord  = snd curNN
        line      = (splitAxis otherTree, splitVal otherTree)
        circle    = (curCoord, distance coord curCoord)
    in if possiblyIntersects line circle
        then closest coord curNN (nearestNeighbor coord otherTree)
        else curNN

{---
 | Hidden helper functions.
 ---}

-- Helper to sort [KDPoint] by a certain axis into the KDCoordinate that is the
-- snd component of the KDPoint.
axisSort :: KDDepth -> KDPoint -> KDPoint -> Ordering
axisSort depth pointA pointB = 
    ((snd pointA) !! depth) `compare` ((snd pointB) !! depth)

-- Helper function that recursively makes the tree. At each level of recursion,
-- if supplies a depth value that determines which axis to use for splitting.
fromList' :: KDDepth -> KDPointList -> KDTree
fromList' _ [] = KDEmpty
fromList' depth pointList =
    let k      = (length . snd . head) pointList
        n      = length pointList
        m      = max 0 (n `div` 2) -- Location of median in sorted list.
        axis   = depth `mod` k -- Which axis to sort by
        sorted = sortBy (axisSort axis) pointList -- Points sorted by axis val.
        median = sorted !! m -- Point which has median axis value.
        front  = take (m) sorted -- Points "to the left" of median.
        back   = drop (m+1) sorted -- Points "to the right" of median.
    in KDNode {idVal      = fst median,
               location   = snd median, 
               splitVal   = (snd median) !! axis,
               splitAxis  = axis,
               leftChild  = fromList' (depth + 1) front,
               rightChild = fromList' (depth + 1) back}

-- Determine if a hypersphere `circle` will be intersected by the line
-- that defines the split at a given node.
possiblyIntersects :: KDLine -> KDCircle -> Bool
possiblyIntersects line circle = 
    let axis    = fst line
        axisVal = snd line
        circVal = (fst circle) !! axis
        radius  = snd circle
    in (circVal - radius) <= axisVal || axisVal <= (circVal + radius)

-- Calculate distance between two coordinates.
distance :: KDCoordinate -> KDCoordinate -> Float
distance c1 c2 = sqrt $ sum $ zipWith (\x y -> (x - y)**2) c1 c2

-- Determine which of two candidates are closer to a base point.
closest :: KDCoordinate -> KDPoint -> KDPoint -> KDPoint
closest baseCoord candidateA candidateB = 
    let distA = distance baseCoord (snd candidateA) -- snd to get coordinates.
        distB = distance baseCoord (snd candidateB) 
    in if distA <= distB
        then candidateA
        else candidateB




