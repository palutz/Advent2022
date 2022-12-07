{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 where

dataFile02 :: FilePath
dataFile02 = "data/02_day02.txt"

dataFile02test :: FilePath
dataFile02test = "data/02_day02test.txt"

data Hand = Rock | Paper| Scissors  
            deriving (Show, Eq)

decryptHand :: (String, String) -> (Hand, Hand)
decryptHand (e1, e2) = (decInput, decOut)
            where decInput = 
                    case e1 of 
                      "A" -> Rock
                      "B" -> Paper
                      "C" -> Scissors
                  decOut = 
                      case e2 of 
                        "X" -> Rock
                        "Y" -> Paper
                        "Z" -> Scissors

splitIn2 :: String -> (String, String)
splitIn2 s = (w1 ws, w2 ws)
          where ws = words s
                w1 :: [String] -> String
                w1 = head
                w2 :: [String] -> String
                w2 = (!! 1)

winLoseHand :: (Hand, Hand) -> Int
winLoseHand (Rock,  Scissors) = 0 
winLoseHand (Paper, Rock) = 0
winLoseHand (Scissors, Paper) = 0
winLoseHand (Rock, Paper) = 6
winLoseHand (Paper, Scissors) = 6
winLoseHand (Scissors, Rock) = 6
winLoseHand (h1, h2) | h1 == h2 = 3

valueHand :: Hand -> Int 
valueHand Rock = 1
valueHand Paper = 2
valueHand Scissors = 3

calculateHand :: (Hand, Hand) -> Int
calculateHand (h1, h2) = winLoseHand (h1, h2) + valueHand h2

totHands :: [String] -> Int
totHands = sum . map (calculateHand . decryptHand . splitIn2)

totH2 :: [(String, String)] -> Int
totH2 = sum . map (calculateHand . decryptHand)

tData :: [(String,String)]
tData = [("A", "Y"),("B", "X"),("C", "Z")]

dataTest2 :: [(String, String)] -> Int
dataTest2 = totH2

dataTest :: FilePath -> IO () 
dataTest fp = 
    do
      contents <- readFile fp 
      let ls = totHands $ lines contents
      print ls

