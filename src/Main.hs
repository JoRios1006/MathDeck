module Main where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Generator
import CSV (AnkiCard, polynomialCard, writeCSVFile, deduplicateCards)
import Types

defaultCount :: Int
defaultCount = 10

main :: IO ()
main = do
  args <- getArgs
  let (difficulty, count) = parseArgs args
  now <- getCurrentTime
  let seed = round (utctDayTime now * 1000) :: Integer
  let rng = mkRNG seed
  let cards = generateCards rng difficulty count
  let filename = "mathdeck_output.csv"
  writeCSVFile filename cards
  putStrLn $ "MathDeck: Generated " ++ show (length cards) ++ " cards -> " ++ filename

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
generateCards rng difficulty count = deduplicateCards (go rng count [])
  where
    go _ 0 acc = acc
    go g n acc =
      let (result, g') = generateByDifficulty difficulty g
      in case result of
        Left poly ->
          case polynomialCard poly (difficultyTags difficulty) of
            Just card -> go g' (n-1) (card:acc)
            Nothing   -> go g' n acc
        Right _ ->
          go g' (n-1) acc

difficultyTags :: Difficulty -> [String]
difficultyTags Easy   = ["easy", "quadratic"]
difficultyTags Medium = ["medium"]
difficultyTags Hard   = ["hard", "rational"]
difficultyTags Mixed  = ["mixed"]
