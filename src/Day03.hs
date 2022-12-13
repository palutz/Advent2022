module Day03 where

import Data.Containers.ListUtils(nubOrd)

dataFile03 :: FilePath
dataFile03 = "data/03_day03.txt"

test1 :: String
test1 = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

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

day3 :: FilePath -> IO ()
day3 fData = 
  do 
    content <- readFile fData
    let rows = lines content
    let comms = map (findCommon . splitString) rows
    let allComm = concat comms -- from [String] to a String (or [Char])
    let rs = map (priorValue allLetters) allComm
    print (sum rs) 
    