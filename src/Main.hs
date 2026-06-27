module Main where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Environment (getArgs)
import Generator
import CSV (AnkiCard, polynomialCards, writeCSVFile, deduplicateCards)
import Types

defaultCount :: Int
defaultCount = 10

main :: IO ()
main = do
  args <- getArgs
  let (difficulty, count) = parseArgs args
  now <- getCurrentTime
  let seed = round (utctDayTime now * 1000) :: Integer
  let rng  = mkRNG seed
  let cards = generateCards rng difficulty count
  let filename = "mathdeck_output.csv"
  writeCSVFile filename cards
  let nFunctions = count
      nCards     = length cards
  putStrLn $ "MathDeck: " ++ show nFunctions ++ " functions → " ++ show nCards ++ " cards -> " ++ filename

parseArgs :: [String] -> (Difficulty, Int)
parseArgs []      = (Mixed, defaultCount)
parseArgs [d]     = (parseDifficulty d, defaultCount)
parseArgs (d:n:_) = (parseDifficulty d, read n)

parseDifficulty :: String -> Difficulty
parseDifficulty "easy"   = Easy
parseDifficulty "medium" = Medium
parseDifficulty "hard"   = Hard
parseDifficulty _        = Mixed

generateCards :: RNG -> Difficulty -> Int -> [AnkiCard]
generateCards rng difficulty count =
  deduplicateCards $ concatMap generate (take count (generatePolys rng difficulty))
  where
    generate poly = polynomialCards poly (difficultyTags difficulty)

generatePolys :: RNG -> Difficulty -> [Polynomial]
generatePolys rng difficulty = go rng
  where
    go g =
      let (result, g') = generateByDifficulty difficulty g
      in case result of
        Left poly -> poly : go g'
        Right _   -> go g'

difficultyTags :: Difficulty -> [String]
difficultyTags Easy   = ["easy", "quadratic"]
difficultyTags Medium = ["medium"]
difficultyTags Hard   = ["hard", "rational"]
difficultyTags Mixed  = ["mixed"]
