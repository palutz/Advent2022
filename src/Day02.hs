-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 where

dataFile02 :: FilePath
dataFile02 = "data/02_day02.txt"

dataFile02test :: FilePath
dataFile02test = "data/02_day02test.txt"

data Hand = Rock | Paper| Scissors  
            deriving (Show, Eq)

data Result = Win | Draw | Lose
              deriving (Show, Eq)

data HandsResult = HandsResult {
  otherP :: Hand,
  myHand :: Hand,
  outcome :: Result
} deriving (Show)

data Parts = Part1 | Part2 
            deriving (Show)

data HandParts  =
  HandPart1 (Hand, Hand)
  | HandPart2 (Hand, Result)
  deriving (Show)

mapInpToHand :: String -> Hand 
mapInpToHand "A" =  Rock
mapInpToHand "B" = Paper
mapInpToHand "C" = Scissors
mapInpToHand "X" = Rock
mapInpToHand "Y" = Paper
mapInpToHand "Z" = Scissors

mapInpToResult :: String -> Result
mapInpToResult "X" = Lose 
mapInpToResult "Y" = Draw
mapInpToResult "Z" = Win

decryptHand :: (String, String) -> (Hand, Hand)
decryptHand (e1, e2) = (decInput, decOut)
            where decInput = mapInpToHand e1
                  decOut = mapInpToHand e2

decryptHand2 :: (String, String) -> (Hand, Result)
decryptHand2 (e1, e2) = (decInput, decOutput)
            where decInput = mapInpToHand e1
                  decOutput = mapInpToResult e2

splitIn2 :: String -> (String, String)
splitIn2 s = (w1 wordS, w2 wordS)
          where wordS = words s  -- split the string with the 2 comands
                w1 :: [String] -> String
                w1 = head
                w2 :: [String] -> String
                w2 = (!! 1)

allHandsRSP :: [HandsResult] -- all possible results for Rock Scissors Paper
allHandsRSP = 
    [
      HandsResult Rock Scissors Lose,
      HandsResult Rock Rock Draw,
      HandsResult Rock Paper Win,
      HandsResult Paper Rock Lose,
      HandsResult Paper Paper Draw,
      HandsResult Paper Scissors Win,
      HandsResult Scissors Paper Lose,
      HandsResult Scissors Scissors Draw,
      HandsResult Scissors Rock Win
    ]

resultHand :: (HandsResult -> Bool) -> [HandsResult] -> HandsResult
resultHand predicate = head . filter predicate
--                 filtered = filter (\x -> h1 == otherP x && h2 == myHand x)

resultToInt :: Result -> Int
resultToInt Win = 6
resultToInt Draw = 3 
resultToInt Lose = 0

valueHand :: Hand -> Int 
valueHand Rock = 1
valueHand Paper = 2
valueHand Scissors = 3

-- find the handresult record for the game with the 2 hands as paramenter
findHandPart1 :: (Hand, Hand) -> HandsResult -> Bool
findHandPart1 (h1, h2) x = (h1 == otherP x && h2 == myHand x)

-- find the handresult record for the game with the 1st hands and the outcome as paramenter
findHandPart2 :: (Hand, Result) -> HandsResult -> Bool 
findHandPart2 (h1, res) x = (h1 == otherP x && res == outcome x)

winLoseRSP :: HandParts -> HandsResult
-- calculate hand for the first part. Just checking with hand win or not
winLoseRSP (HandPart1 (h1, h2)) = resultHand fp1 allHandsRSP
                  where fp1 = findHandPart1 (h1, h2)
-- calculate Hand for the second part in which we need to manage the outcome of the hand in another way ...
-- "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, 
-- and Z means you need to win. 
winLoseRSP (HandPart2 (h1, r)) = resultHand fp2 allHandsRSP
                  where fp2 = findHandPart2 (h1, r)


-- calculateHand :: ((Hand, Hand) -> Result) -> (Hand, Hand) -> Int
-- calculateHand fCalcHand (h1, h2) = (resultToInt . fCalcHand) (h1, h2) + valueHand h2
calculateHand :: HandParts -> (HandParts -> HandsResult) -> Int
calculateHand (HandPart1 (h1, h2)) f = resultToInt (outcome handRes1) + valueHand h2
                                      where 
                                        handRes1 :: HandsResult 
                                        handRes1 = f (HandPart1 (h1, h2))
calculateHand (HandPart2 (h1, r))  f = resultToInt (outcome handRes2) + valueHand (myHand handRes2)
                                      where 
                                        handRes2 :: HandsResult 
                                        handRes2 = f (HandPart2 (h1, r))

calculatePart :: HandParts -> Int
calculatePart hp = calculateHand hp winLoseRSP

totHands :: Parts ->[String] -> Int 
totHands part = sum . map (calculatePart . decryptPart)
                where 
                  decryptPart :: String -> HandParts
                  decryptPart s = 
                    case part of 
                      Part1 -> HandPart1 (decryptHand . splitIn2 $ s)
                      Part2 -> HandPart2 (decryptHand2 . splitIn2 $ s)

dataTest :: FilePath -> IO () 
dataTest fp = 
    do
      contents <- readFile fp 
      let ls  = totHands Part1 $ lines contents
      let ls2 = totHands Part2 $ lines contents
      print ("First part= " <> show ls <> ". Second part= " <> show ls2)

-- tData :: [(String,String)]
-- tData = [("A", "Y"),("B", "X"),("C", "Z")]

-- totH2 :: [(String, String)] -> Int
-- totH2 = sum . map (calculatePart2 . decryptHand)
-- 
-- dataTest2 :: [(String, String)] -> Int
-- dataTest2 = totH2