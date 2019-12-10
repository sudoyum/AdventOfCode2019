import Debug.Trace
import System.Environment
import System.IO
import System.Exit

convertVals :: [String] -> [Int]
convertVals = map read

--Part 1
doFuelMath :: Int -> Int
doFuelMath inputVal = inputVal `div` 3 - 2 

sumAllValues :: [Int] -> Int
sumAllValues inputList = sum (map doFuelMath inputList)

--Part 2
doFuelMathRec :: Int -> Int
doFuelMathRec inputVal
     | inputVal <= 5    = 0
     | otherwise        = fuelResult + (doFuelMathRec fuelResult) 
     where fuelResult = (inputVal `div` 3) - 2

sumAllValuesRec :: [Int] -> Int
sumAllValuesRec inputList = sum (map doFuelMathRec inputList)


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
        
        if dayVal == "p1" then
            putStrLn (show (sumAllValues (convertVals splitLines)))
        else if dayVal == "p2" then
            putStrLn (show (sumAllValuesRec (convertVals splitLines)))
        else if dayVal == "both" then do
            putStrLn (show (sumAllValues (convertVals splitLines)))
            putStrLn (show (sumAllValuesRec (convertVals splitLines)))
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess
