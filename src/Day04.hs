module Day04 where 


dataFile04 :: FilePath
dataFile04 = "data/04_day04.txt"

test04 :: String
test04 = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\n"

type RMin = Int 
type RMax = Int 

data MyRange = MyRange {
  rmin :: RMin, 
  rmax :: RMax
} deriving (Eq, Ord, Show)

data Parts = Part1 | Part2

class Contained a where 
  (|=|) :: a -> a -> Bool 

instance Contained MyRange where 
  a |=| b = rmin a >= rmin b && rmax a <= rmax b

class Overlapped a where 
  (|~|) :: a -> a -> Bool 

instance Overlapped MyRange where 
  a |~| b = rmin b >= rmin a && rmin b <= rmax a

splitAtChar :: Char -> String -> [String]
splitAtChar c w = case dropWhile (== c) w of
                    "" -> []
                    s -> w1 : splitAtChar c s1
                          where (w1, s1) = break(== c) s

listToTuple :: [a] -> (a, a)
listToTuple [x1,x2] = (x1, x2)

listToMyRange :: [Int] -> MyRange
listToMyRange [x1,x2] = MyRange x1 x2 

splitDash :: String -> [String]
splitDash = splitAtChar '-'

stringToSplittedInts :: String -> [Int]
stringToSplittedInts s = map read $ splitDash s

intSplitted :: (String -> [Int]) -> String -> (Int, Int)
intSplitted f s = listToTuple $ f s

rangeSplitted :: (String -> [Int]) -> String -> MyRange
rangeSplitted f s = listToMyRange $ f s

contained :: [(MyRange, MyRange)] -> [(MyRange, MyRange)]
contained = filter (\(l, r) -> (l |=| r) || (r |=| l))

overlapped :: [(MyRange, MyRange)] -> [(MyRange, MyRange)]
overlapped = filter (\(l, r) -> (l |~| r) || (r |~| l))

workFlow :: String -> [(MyRange, MyRange)]
workFlow s = 
  let
    ls :: [String]
    ls = lines s
    couples :: [[String]]
    couples = map (splitAtChar ',') ls
    ranges :: [[MyRange]]
    ranges  = map ( map (rangeSplitted stringToSplittedInts)) couples
  in 
    map listToTuple ranges  

parts :: Parts -> String -> Int
parts p s = length res 
            where
              res :: [(MyRange, MyRange)]
              res = 
                case p of 
                  Part1 -> contained listJobs
                  Part2 -> overlapped listJobs
              listJobs = workFlow s 

day04 :: FilePath -> IO () 
day04 fp = 
  do 
    contents <- readFile fp
    let part1 = parts Part1 contents
    let part2 = parts Part2 contents
    print $ "Part1 = " <> show part1 <> ". Part2 = " <> show part2

-- Part1 = 538. Part2 = 792