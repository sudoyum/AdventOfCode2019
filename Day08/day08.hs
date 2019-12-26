import Debug.Trace
import System.Environment
import System.IO
import System.Exit

import Data.Ord

makeLayers :: Int -> Int -> String -> [String]
makeLayers w h inputStream
    | length inputStream >= (w*h*2) = [focus] ++ (makeLayers w h remaining)
    | length inputStream <= (w*h)   = [inputStream]
    where splitList = splitAt (w*h) inputStream
          focus = fst splitList
          remaining = snd splitList

countZeroes :: String -> Int
countZeroes inputStr = length ( filter(=='0') inputStr)

getMinZeroes :: [String] -> Int
getMinZeroes layerList = minimum (map countZeroes layerList)

getMinZeroesList :: [String] -> [Int]
getMinZeroesList layerList = map countZeroes layerList

getZippedList :: [String] -> [(String, Int)]
getZippedList inputList = zip inputList (getMinZeroesList inputList)

calcP1Answer :: String -> Int
calcP1Answer layer = numOnes * numTwos
    where numOnes = length( filter(=='1') layer)
          numTwos = length( filter(=='2') layer)

getMinimumLayer :: [(String, Int)] -> Int -> String
getMinimumLayer (x:xs) minVal = if (snd x) == minVal then (fst x) else getMinimumLayer xs minVal

getValsAtIndex :: [String] -> Int -> String
getValsAtIndex layers index = [x !! index | x <- layers]

getLayerVal :: String -> Char
getLayerVal (x:xs)
    | x == '0' = ' '
    | x == '1' = x
    | x == '2' = getLayerVal xs

decodeP2Image :: [String] -> Int -> String
decodeP2Image layers index
    | nextIndex < 25*6 = currentLayer ++ (decodeP2Image layers nextIndex)
    | otherwise = currentLayer
    where currentLayer = [getLayerVal (getValsAtIndex layers index)]
          nextIndex    = index + 1

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

        let doFunctionTests = False

        let pixelWidth = 25
        let pixelHeight = 6
        let inputStream = splitLines !! 0

        if dayVal == "p1" then do
                let layers = makeLayers pixelWidth pixelHeight inputStream
                let zippedList = getZippedList layers
                putStrLn ( show (calcP1Answer (getMinimumLayer zippedList (getMinZeroes layers) )))
        else if dayVal == "p2" then do
            let layers = makeLayers pixelWidth pixelHeight inputStream
            let decodedImage = makeLayers 25 1 (decodeP2Image layers 0)
            mapM_ (putStrLn . init . tail . show) decodedImage
        else if dayVal == "both" then do
            let layers = makeLayers pixelWidth pixelHeight inputStream
            let zippedList = getZippedList layers
            putStrLn ( show (calcP1Answer (getMinimumLayer zippedList (getMinZeroes layers) )))
            let decodedImage = makeLayers 25 1 (decodeP2Image layers 0)
            mapM_ (putStrLn . init . tail . show) decodedImage
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess
