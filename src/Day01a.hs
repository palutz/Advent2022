module Day01a where

import Text.Read (readEither)

dataFile01 :: FilePath 
dataFile01 = "/Users/Stefano.Paluello/Projects/haskell/Advent2022/01_data.txt"

readLines :: FilePath -> IO ()
readLines fp = do 
                f <- readFile fp
                let ls = (splitCalories . lines) f 
                let is = map caloriesTotInt ls
                print (maximum is)
                

-- split the list of calories carried by one Elf
splitCalories :: [String] -> [[String]]
splitCalories [] = []
splitCalories ("":xs) = splitCalories xs
splitCalories s = asplit : splitCalories rest 
                  where (asplit, rest) = break (== "") s

-- parse the string - return the number or 0 if there is a problem
parseInt :: String -> Int 
parseInt s = 
  case readEither s of 
    Right n -> n 
    Left _ -> 0

-- take all the calories for one Elf and retunr the sum  
caloriesTotInt :: [String] -> Int
caloriesTotInt = sum . map parseInt
