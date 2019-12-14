import Debug.Trace
import System.Environment
import System.IO
import System.Exit

import Data.List.Split
import Data.Char

isWithinRange :: Int -> Int -> Int -> Bool
isWithinRange lowerBound upperBound val = (val <= upperBound && val >= lowerBound)

checkNumDigits :: String -> Bool
checkNumDigits inputStr = (length inputStr == 6)

isDoubleValues :: [Char] -> Int -> Bool
isDoubleValues inputString index
     | index >= (length inputString) - 2 = compareVals 
     | otherwise = (isDoubleValues inputString nextIndex) || compareVals
     where firstVal = (inputString !! index)
           secondVal = (inputString !! nextIndex)
           nextIndex = index + 1
           compareVals = (firstVal == secondVal)

isDecreasingValues :: [Char] -> Int -> Bool
isDecreasingValues inputString index
     | index >= ((length inputString) - 2) = compareVals 
     | otherwise = (isDecreasingValues inputString nextIndex) || compareVals
     where firstVal    = ord (inputString !! index)
           secondVal   = ord (inputString !! nextIndex)
           nextIndex   = index + 1
           compareVals = (firstVal > secondVal)

-- Part 1
checkValuesInRange :: Int -> Int -> Int -> Int 
checkValuesInRange lowerBound upperBound val
     | not (isWithinRange lowerBound upperBound val) = 0
     | otherwise = if criteriaMatch then (1 + doAgain) else doAgain
     where criteriaMatch = (checkNumDigits valStr) && orderingConds
           valStr = show val
           nextVal = val + 1
           orderingConds = (isDoubleValues valStr 0) && (not (isDecreasingValues valStr 0))
           doAgain = (checkValuesInRange lowerBound upperBound nextVal)

-- Part 2
isDoubleNotGroup :: [Char] -> Int -> Bool
isDoubleNotGroup inputString index
     | index == (length inputString) - 2 = cond1 && (firstVal /= prevVal)
     | otherwise = (isDoubleNotGroup inputString nextIndex) || compareVals
     where firstVal = (inputString !! index)
           secondVal = (inputString !! nextIndex)
           thirdVal = (inputString !! nNextIndex)
           nextIndex = index + 1
           nNextIndex = index + 2
           compareVals = cond1 && (secondVal /= thirdVal) && (firstVal /= prevVal)
           cond1 = (firstVal == secondVal) 
           prevVal = if index == 0 then 'c' else (inputString !! (index - 1)) 
           

checkValuesInRangeEx :: Int -> Int -> Int -> Int 
checkValuesInRangeEx lowerBound upperBound val
     | not (isWithinRange lowerBound upperBound val) = 0
     | otherwise = if criteriaMatch then (1 + doAgain) else doAgain
     where criteriaMatch = (checkNumDigits valStr) && orderingConds
           valStr = show val
           nextVal = val + 1
           orderingConds = (isDoubleNotGroup valStr 0) && (not (isDecreasingValues valStr 0))
           doAgain = (checkValuesInRangeEx lowerBound upperBound nextVal)

main = do
    args <- getArgs 
    if length args /= 2 then do
        putStrLn "Usage: ./day input.txt (p1|p2|both)"
        exitSuccess
    else do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        let doTests = False

        let splitLines = lines contents
        let dayVal = args !! 1

        let inputRange = splitOn "-" (head splitLines)
        let lowerBound = read (head inputRange)
        let upperBound = read (last inputRange)
        
        if dayVal == "p1" then do
            if doTests then do
                putStrLn (show (checkNumDigits "122345"))
                putStrLn (show (checkNumDigits "12234"))
                putStrLn (show (isWithinRange lowerBound upperBound 143234))
                putStrLn (show (isWithinRange lowerBound upperBound 652526))
                putStrLn (show (isDoubleValues "122345" 0))
                putStrLn (show (isDoubleValues "123456" 0))
                putStrLn (show (isDecreasingValues "123452" 0))
                putStrLn (show (isDecreasingValues "123456" 0))
                putStrLn (show (isDecreasingValues "923456" 0))
                putStrLn (show (isDecreasingValues "129456" 0))
            else
                putStrLn (show (checkValuesInRange lowerBound upperBound lowerBound))
        else if dayVal == "p2" then do
            if doTests then do
                putStrLn (show (isDoubleNotGroup "112233" 0))
                putStrLn (show (isDoubleNotGroup "123444" 0))
                putStrLn (show (isDoubleNotGroup "122222" 0))
                putStrLn (show (isDoubleNotGroup "111122" 0))
            else
                putStrLn (show (checkValuesInRangeEx lowerBound upperBound lowerBound))
        else if dayVal == "both" then do
            putStrLn (show (checkValuesInRange lowerBound upperBound lowerBound))
            putStrLn (show (checkValuesInRangeEx lowerBound upperBound lowerBound))
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess
