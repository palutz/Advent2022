module Day01a where

import Text.Read (readEither)
import Data.List (sort)

dataFile01 :: FilePath 
dataFile01 = "data/01_data.txt"

-- split the list of calories carried by one Elf
splitCalories :: [String] -> [[String]]
splitCalories [] = []
splitCalories ("":xs) = splitCalories xs
splitCalories s = asplit : splitCalories rest 
                  where (asplit, rest) = break (== "") s -- break = split in a tuple: the left takes all elem 

-- parse the string - return the number or 0 if there is a problem
parseInt :: String -> Int 
parseInt s = 
  case readEither s of 
    Right n -> n 
    Left _ -> 0

-- take all the calories for one Elf and retunr the sum  
caloriesTotInt :: [String] -> Int
caloriesTotInt = sum . map parseInt

readLines :: FilePath -> IO [Int]
readLines fp = do 
                f <- readFile fp
                let ls = (splitCalories . lines) f 
                let is = map caloriesTotInt ls
                let order = reverse . sort
                return (order is)

-- return the tot of calories for n Elfs           
totNCalories :: Int -> IO [Int] -> IO ()
totNCalories n ioCals = 
    do
      cals <- ioCals
      let totCal = (sum . take n) cals
      print totCal

maxCalFirstElf :: IO [Int] -> IO ()
maxCalFirstElf = totNCalories 1

totCalTop3Elfs :: IO [Int] -> IO ()
totCalTop3Elfs = totNCalories 3