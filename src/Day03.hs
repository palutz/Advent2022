module Day03 where

import Data.Containers.ListUtils(nubOrd)

dataFile03 :: FilePath
dataFile03 = "data/03_day03.txt"

data Workflow = Part1WF | Part2WF

test03 :: String
test03 = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

lowerL ::[(Char, Int)]
lowerL = zip ['a'..'z'] [1..26]

upperL :: [(Char, Int)]
upperL = zip ['A'..'Z'] [27..52]

allLetters :: [(Char, Int)]
allLetters = lowerL ++ upperL

-- search for the priority value of the letter
priorValue :: [(Char, Int)] -> Char -> Int 
priorValue letters c = case lookup c letters of
                          Just a -> a 
                          _ -> 0

splitString :: String -> (String, String)
splitString s = splitAt halfL s
                where halfL = length s `div` 2

-- findDuplicates: find char in common with the other list and store only if they are not already
-- in the bag. It works ONLY if the first list is sorted!
findCommon :: (String, String) -> [Char]
findCommon (s1, s2) = nubOrd $            -- snd the nubOrd will remove the duplicated elem in the list
                foldr (\a xs -> if a `elem` s1 then a:xs else xs) [] s2 -- first the fold will return a list of all the elements in common between 2 lists

splitIn3 :: [String] -> [(String, String, String)]
splitIn3 [] = [] 
splitIn3 [x] = [(x, "", "")]
splitIn3 [x1,x2] = [(x1,x2, "")]
splitIn3 (x1:x2:x3:xs) = (x1,x2,x3) : splitIn3 xs

--part2 Workflow
part2WF :: String -> Int 
part2WF content = 
    let rows = lines content
        tuples =splitIn3 rows -- [String] -> [(String, String, String)]
        allComm = concatMap (\(t1,t2,t3) -> findCommon(t1, findCommon(t2, t3))) tuples
    in 
        sum $ map (priorValue allLetters) allComm

-- part1 WorkFlow
part1WF :: String -> Int 
part1WF content = 
    let rows = lines content
        allComm = concatMap (findCommon . splitString) rows -- split, find common and putting all duplicated chars together in one string, from [[]Char]] to [Char]
    in 
        sum $ map (priorValue allLetters) allComm

day3Part1 :: FilePath -> IO() 
day3Part1 = day3 Part1WF

day3Part2 :: FilePath -> IO() 
day3Part2 = day3 Part2WF

day3 :: Workflow -> FilePath -> IO ()
day3 wf fData = 
  do 
    content <- readFile fData
    case wf of 
      Part1WF -> print $ part1WF content
      Part2WF -> print $ part2WF content
    