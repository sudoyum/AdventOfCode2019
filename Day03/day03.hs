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

data Point   = Point { x :: Int
                     , y :: Int
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
    | index == (length wireDefList -1) = [doSeg]
    | otherwise =  [doSeg] ++ doNextSeg
    where doSeg     =  doSegment (wireDefList !! index) startX startY dist
          doNextSeg =  makeSegList wireDefList (xEnd doSeg) (yEnd doSeg) (index + 1) (dist + (tDist doSeg))

findSmallestDist :: [Point] -> Int
findSmallestDist inputPoints = minimum (map manhattanDistOrig inputPoints)

getIntersectPoint :: WireSeg -> WireSeg -> Point
getIntersectPoint seg1 seg2 = do
    let x1s = makeListFromRange (xStart seg1) (xEnd seg1)
    let x2s = makeListFromRange (xStart seg2) (xEnd seg2)
    let y1s = makeListFromRange (yStart seg1) (yEnd seg1)
    let y2s = makeListFromRange (yStart seg2) (yEnd seg2)
    let matchingP = [(x1, y1) | x1<-x1s, x2<-x2s, y1<-y1s, y2<-y2s, x1==x2, y1==y2]
    if length matchingP > 0 then do
        Point { x = (fst (head matchingP)), y = (snd (head matchingP)) }
    else
        Point { x = 0, y = 0 }

{-
findIntersectingPoints :: WireSeg -> WireSeg -> Point
findIntersectingPoints seg1 seg2
    | trace ("inrange=" ++ show (yInRange && xInRange)) False = undefined
    | intersect = getIntersectPoint seg1 seg2 
    | otherwise = Point { x = 0, y = 0 }
    where seg2XRange = makeListFromRange (xStart seg2) (xEnd seg2)
          seg2YRange = makeListFromRange (yStart seg2) (yEnd seg2)
          intersect = ((seg1 deltaX) && (seg2 y `elem` seg2XRange) || (not(seg1 deltaX) && 
-}
         

findAllIntersectingPoints :: [WireSeg] -> [WireSeg] -> [Point]
findAllIntersectingPoints segList1 segList2 =  do
      let unfiltList = [(getIntersectPoint sls1 sls2) | sls1<-segList1, sls2<-segList2] 
      --let unfiltList = [(findIntersectionPoints sls1 sls2) | sls1<-segList1, sls2<-segList2] 
      [pList | pList<-unfiltList, not((x pList) == 0 && (y pList) == 0)]


main = do
    args <- getArgs 
    if length args /= 2 then do
        putStrLn "Usage: ./day input.txt (p1|p2|both)"
        exitSuccess
    else do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        --let doTest = True
        let doTest = False
        let splitLines = lines contents
        let dayVal = args !! 1

        let wire1 = splitOn "," (head splitLines)
        let wire2 = splitOn "," (last splitLines)

        let originX = 0
        let originY = 0
        
        if dayVal == "p1" then do 
            if doTest then do
                let testPoint1 = Point { x = 21, y = 28}
                let testPoint2 = Point { x = 17, y = 49 }
                let testPoint3 = Point { x = 7, y = 16}
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
                
            else do
                --findAllIntersectingPoints segList1 segList2 =  do
                let segList1 = makeSegList wire1 originX originY 0 0
                let segList2 = makeSegList wire2 originX originY 0 0
                putStrLn(show (findSmallestDist (findAllIntersectingPoints segList1 segList2)))
        else if dayVal == "p2" then
            putStrLn dayVal
        else if dayVal == "both" then do
            putStrLn dayVal
            putStrLn dayVal
        else do
            putStrLn "Usage: ./day input.txt (p1|p2|both)"
            exitSuccess
