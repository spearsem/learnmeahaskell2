module Main where
import KDTree

main = do
    -- Example KD Tree from [(id, coords)]
    let exampleTree = fromList [(1, [10, 10]), 
                                (2, [2, 4]), 
                                (3, [5, 6]), 
                                (4, [-1, -1])]

    -- Coordinates where to locate nearest station ID
    let testPoint = [-2, -2]

    putStrLn $ "The closest station to " ++ show testPoint ++ " is:" 
    putStrLn $ show $ nearestNeighbor testPoint exampleTree
