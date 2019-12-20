import Debug.Trace
import System.Environment
import System.IO
import System.Exit

import Data.List.Split

data WireSeg = WireSeg { deltaX :: Bool
                       , xStart :: Int
                       , xEnd   :: Int
                       , yStart :: Int
                       , yEnd   :: Int
                       , tDist   :: Int
                       } deriving (Eq, Show)

xSegDist :: WireSeg -> Int
xSegDist seg = abs((xEnd seg) - (xStart seg))

ySegDist :: WireSeg -> Int
ySegDist seg = abs((yEnd seg) - (yStart seg))

data Point = Point { x :: Int
                   , y :: Int
                   , totDist :: Int
                   } deriving (Show)

manhattanDist :: Point -> Point -> Int
manhattanDist p1 p2 = abs ((x p1) - (x p2)) + abs ((y p1) - (y p2))

manhattanDistOrig :: Point -> Int
manhattanDistOrig p1 = abs (x p1) + abs (y p1)

doSegment :: String -> Int -> Int -> Int -> WireSeg 
doSegment wireStr x y dist
    | start == 'U'   = WireSeg False x x y (y + numMov) newDist
    | start == 'D'   = WireSeg False x x y (y - numMov) newDist
    | start == 'R'   = WireSeg True x (x + numMov) y y newDist
    | start == 'L'   = WireSeg True x (x - numMov) y y newDist
    where start  = head wireStr
          numMov = read(tail wireStr)
          newDist = dist + numMov

makeListFromRange :: Int -> Int -> [Int]
makeListFromRange x1 x2
    | x1 == x2 = [x1]
    | x1 > x2  = [x2..x1]
    | x2 > x1  = [x1..x2]

makeSegList :: [String] -> Int -> Int -> Int -> Int -> [WireSeg]
makeSegList wireDefList startX startY index dist
    | index == (length wireDefList - 1) = [doSeg]
    | otherwise =  [doSeg] ++ doNextSeg
    where doSeg     =  doSegment (wireDefList !! index) startX startY dist
          doNextSeg =  makeSegList wireDefList (xEnd doSeg) (yEnd doSeg) (index + 1) (tDist doSeg)

findSmallestDist :: [Point] -> Int
findSmallestDist inputPoints = minimum (map manhattanDistOrig inputPoints)

findShortestPath :: [Point] -> Int
findShortestPath inputPoints = minimum (map totDist inputPoints)

getIntersectPoint :: WireSeg -> WireSeg -> Point
getIntersectPoint seg1 seg2 = do
    if (deltaX seg1) && not(deltaX seg2) then do
        let totalDist = (tDist seg1) + (tDist seg2) - abs(xEnd seg1 - xEnd seg2) - abs(yEnd seg2 - yEnd seg1)
        Point { x = (xEnd seg2), y = (yEnd seg1), totDist = totalDist }
    else do
        let totalDist = (tDist seg1) + (tDist seg2) - abs(xEnd seg2 - xEnd seg1) - abs(yEnd seg1 - yEnd seg2)
        Point { x = (xEnd seg1), y = (yEnd seg2), totDist = totalDist }


isInRange :: Int -> Int -> Int -> Bool
isInRange val limit1 limit2
    | limit1 == limit2 = val == limit1
    | limit2 > limit1 = (val <= limit2) && (val >= limit1)
    | limit2 < limit1 = (val <= limit1) && (val >= limit2)


hasIntersectingPoint :: WireSeg -> WireSeg -> Bool
hasIntersectingPoint seg1 seg2 = do
    let seg1xRange = isInRange (xEnd seg2) (xStart seg1) (xEnd seg1)
    let seg2yRange = isInRange (yEnd seg1) (yStart seg2) (yEnd seg2)

    let seg2xRange = isInRange (xEnd seg1) (xStart seg2) (xEnd seg2)
    let seg1yRange = isInRange (yEnd seg2) (yStart seg1) (yEnd seg1)

    if (deltaX seg1) && not(deltaX seg2) then do
        seg1xRange && seg2yRange
    else if (deltaX seg2) && not(deltaX seg1) then do
        seg2xRange && seg1yRange
    else
        False


doAllIntersectingPoints :: [(WireSeg, WireSeg)] -> Int -> [Point]
doAllIntersectingPoints segsList index
     | index == (length segsList - 1) = retList
     | otherwise =  retList ++ (doAllIntersectingPoints segsList nextIndex)
     where nextIndex = index + 1
           seg1      = fst segPair
           seg2      = snd segPair
           segPair   = segsList !! index
           hasIntersect = hasIntersectingPoint seg1 seg2
           retList = if hasIntersect then [getIntersectPoint seg1 seg2] else []

crossingSegs :: WireSeg -> WireSeg -> Bool
crossingSegs sls1 sls2 =
    ((deltaX sls1) && not(deltaX sls2)) || ((deltaX sls2) && not(deltaX sls1))

findAllIntersectingPoints :: [WireSeg] -> [WireSeg] -> [Point]
findAllIntersectingPoints segList1 segList2 =  do
      let allCombinations = [ (sls1, sls2) | sls1<-segList1, sls2<-segList2, crossingSegs sls1 sls2 ]
      let unfiltList = doAllIntersectingPoints allCombinations 0
      [pList | pList<-unfiltList, not((x pList) == 0 && (y pList) == 0)]


main = do
    args <- getArgs 
    if length args /= 2 then do
        putStrLn "Usage: ./day input.txt (p1|p2|both)"
        exitSuccess
    else do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle

        let doTest = False
        let splitLines = lines contents
        let dayVal = args !! 1

        let wire1 = splitOn "," (head splitLines)
        let wire2 = splitOn "," (last splitLines)

        let originX = 0
        let originY = 0
        
        let segList1 = makeSegList wire1 originX originY 0 0
        let segList2 = makeSegList wire2 originX originY 0 0

        if dayVal == "p1" then do 
            if doTest then do
                let testPoint1 = Point { x = 21, y = 28, totDist = 0 }
                let testPoint2 = Point { x = 17, y = 49, totDist = 0 }
                let testPoint3 = Point { x = 7, y = 16, totDist = 0 }
                let testPointList = [testPoint1, testPoint2, testPoint3]
                -- Expect 25
                putStrLn(show (manhattanDist testPoint1 testPoint2))
                putStrLn(show (doSegment "R8" originX originY 0))
                putStrLn(show (makeSegList wire1 originX originY 0 0))
                -- Expect 23
                putStrLn(show (findSmallestDist testPointList))
                let testSeg1 = doSegment "L4" 6 3 0
                let testSeg2 = doSegment "D4" 3 6 0
                let x1s = makeListFromRange (xStart testSeg1) (xEnd testSeg1)
                putStrLn(show (xStart testSeg1))
                putStrLn(show (xEnd testSeg1))
                putStrLn(show (x1s))
                --putStrLn(show (getIntersectPoint testSeg1 testSeg2))
                putStrLn ( show ( isInRange 4 7 (-7)))
                putStrLn ( show ( isInRange 10 7 (-7)))
                let testSeg1 = doSegment "L4" 6 3 0
                let testSeg2 = doSegment "D4" 3 6 0
                let testSeg3 = doSegment "R4" 1 1 0
                let testSeg4 = doSegment "D4" 7 3 0
                putStrLn ( show ( hasIntersectingPoint testSeg1 testSeg2))
                putStrLn ( show ( hasIntersectingPoint testSeg3 testSeg4))
            else do
                putStrLn(show (findSmallestDist (findAllIntersectingPoints segList1 segList2)))
        else if dayVal == "p2" then do
            putStrLn(show (findShortestPath (findAllIntersectingPoints segList1 segList2)))
        else if dayVal == "both" then do
            putStrLn(show (findSmallestDist (findAllIntersectingPoints segList1 segList2)))
            putStrLn(show (findShortestPath (findAllIntersectingPoints segList1 segList2)))
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess
