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

class Contained a where 
  (|=|) :: a -> a -> Bool 

instance Contained MyRange where 
  a |=| b = rmin a >= rmin b && rmax a <= rmax b

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


part1 :: String -> Int
part1 s = 
  let
    ls :: [String]
    ls = lines s
    couples :: [[String]]
    couples = map (splitAtChar ',') ls
    ranges :: [[MyRange]]
    ranges  = map ( map (rangeSplitted stringToSplittedInts)) couples
    rts :: [(MyRange, MyRange)]
    rts     = map listToTuple ranges  
  in 
    length $ contained rts 

day04 :: FilePath -> IO () 
day04 fp = 
  do 
    contents <- readFile fp
    print $ part1 contents