import Debug.Trace
import System.Environment
import System.IO
import System.Exit

import Data.List.Split
import Data.Tree
import Data.Tree.Pretty

getChildren :: String -> [(String, String)] -> [String]
getChildren parent pairList = [snd x | x <- pairList, ((fst x) == parent)]

getRemaining :: String -> [(String, String)] -> [(String, String)]
getRemaining parent pairList = [x | x <- pairList, ((fst x) /= parent)]

createTree :: String -> [(String, String)] -> Tree String
createTree root valList
     | length children == 0 = Node root []
     | otherwise = Node root childBranches
     where children = getChildren root valList
           childBranches = [(createTree x (getRemaining root valList)) | x <- children]

totalDist :: Tree String -> Int -> Int
totalDist tree index
    | length (subForest tree) == 0 = index
    | otherwise = lowerSum + index
    where lowerSum = sum [ (totalDist x (index + 1)) | x <- (subForest tree) ]


createOrbitPair :: String -> (String, String)
createOrbitPair stringVal =
      (head splitString, last splitString)
      where splitString = splitOn ")" stringVal

getOrbitPairs :: [String] -> [(String, String)]
getOrbitPairs inputVals = map createOrbitPair inputVals


main = do
    args <- getArgs
    if length args /= 2 then do
        putStrLn "Usage: ./day input.txt (p1|p2|both)"
        exitSuccess
    else do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        let doFunctionTests = True

        let splitLines = lines contents
        let dayVal = args !! 1

        if dayVal == "p1" then do
            if doFunctionTests then do
                let pairList = getOrbitPairs splitLines
                let createdTree = createTree "COM" pairList
                putStrLn (show (pairList))
                putStrLn( show ( getChildren "B" pairList) )
                putStrLn $ drawVerticalTree createdTree
            else do
                let pairList = getOrbitPairs splitLines
                let createdTree = createTree "COM" pairList
                putStrLn (show ( totalDist createdTree 0 ) )
        else if dayVal == "p2" then do
            let createdTree = createTree "COM" (getOrbitPairs splitLines)
            putStrLn $ drawVerticalTree createdTree
        else if dayVal == "both" then do
            putStrLn dayVal
            putStrLn dayVal
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess
