import Debug.Trace
import System.Environment
import System.IO
import System.Exit

import Data.List.Split

strListToInts :: [String] -> [Int]
strListToInts = map read


replaceItemAt :: [Int] -> Int -> Int -> [Int]
replaceItemAt inputList destIndex destVal
     | destIndex == 0                   = [destVal] ++ (drop 1 inputList)
     | destIndex == (length inputList) - 1 = take ((length inputList) - 1) inputList ++ [destVal]
     | otherwise  =   front ++ [destVal] ++ end
     where front = take destIndex inputList
           end = drop (destIndex + 1) inputList


doSingleRound :: [Int] -> Int -> [Int]
doSingleRound inputList index
    | opcode == 99   = inputList
    | opcode == 1    = replaceItemAt inputList dest (r2 + r1)
    | opcode == 2    = replaceItemAt inputList dest (r2 * r1)
    where opcode = inputList !! index
          dest = inputList !! (index + 3)
          r1 = inputList !! (inputList !! (index + 1))
          r2 = inputList !! (inputList !! (index + 2))

-- Part 1
doOpcodeProcessing :: [Int] -> Int -> Int
doOpcodeProcessing inputList index = do
    let resultingList = doSingleRound inputList index
    if index + 4 < ((length inputList) - 4)
         then doOpcodeProcessing resultingList (index + 4) 
         else head resultingList

-- Part 2
findMatchingValue :: Int -> Int -> [Int] -> Int -> [Int]
findMatchingValue x y inputList valueToMatch = do
    let newX =
            if x < 99
            then x + 1
            else x

    let newY =
            if newX == 99 
            then y + 1
            else y

    let finalX =
            if newX == 99 
            then 0
            else newX

    let newList = replaceItemAt inputList 1 newX
    let finalList = replaceItemAt newList 2 newY

    let resultVal = doOpcodeProcessing finalList 0  
    if resultVal /= valueToMatch 
        then findMatchingValue finalX newY inputList valueToMatch
        else [newX, newY]


main = do
    args <- getArgs 
    if length args /= 2 then do
        putStrLn "Usage: ./day input.txt (p1|p2|both)"
        exitSuccess
    else do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        let splitLines = lines contents
        let dayVal = args !! 1
        
        let splitVals = splitOn "," (head splitLines)
        
        if dayVal == "p1" then do
            putStrLn (show (doOpcodeProcessing (strListToInts splitVals) 0))
        else if dayVal == "p2" then do
            let resultList = findMatchingValue 0 0 (strListToInts splitVals) 19690720
            putStrLn( show (100 * (head resultList) + (last resultList)))
        else if dayVal == "both" then do
            putStrLn (show (doOpcodeProcessing (strListToInts splitVals) 0))
            let resultList = findMatchingValue 0 0 (strListToInts splitVals) 19690720
            putStrLn( show (100 * (head resultList) + (last resultList)))
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess

