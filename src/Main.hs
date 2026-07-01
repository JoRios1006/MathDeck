module Main where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Environment (getArgs)
import Generator
import CSV (AnkiCard, polynomialCards, rationalFuncCards, writeCSVFile, deduplicateCards)
import Types

defaultCount :: Int
defaultCount = 10

main :: IO ()
main = do
  args <- getArgs
  let (difficulty, count) = parseArgs args
  now <- getCurrentTime
  let seed  = round (utctDayTime now * 1000) :: Integer
  let rng   = mkRNG seed
  let cards = generateCards rng difficulty count
  let filename = "mathdeck_output.csv"
  writeCSVFile filename cards
  putStrLn $ "MathDeck: " ++ show count ++ " functions \x2192 "
          ++ show (length cards) ++ " cards \x2192 " ++ filename

parseArgs :: [String] -> (Difficulty, Int)
parseArgs []      = (Mixed, defaultCount)
parseArgs [d]     = (parseDifficulty d, defaultCount)
parseArgs (d:n:_) = (parseDifficulty d, read n)

parseDifficulty :: String -> Difficulty
parseDifficulty "easy"   = Easy
parseDifficulty "medium" = Medium
parseDifficulty "hard"   = Hard
parseDifficulty _        = Mixed

-- Both polynomials and rational functions are now first-class
generateCards :: RNG -> Difficulty -> Int -> [AnkiCard]
generateCards rng difficulty count =
  deduplicateCards $
    concatMap makeCards (take count (generateFunctions rng difficulty))
  where
    makeCards (Left  poly) = polynomialCards   poly (difficultyTags difficulty)
    makeCards (Right rf)   = rationalFuncCards rf   (difficultyTags difficulty)

generateFunctions :: RNG -> Difficulty -> [Either Polynomial RationalFunc]
generateFunctions rng difficulty = go rng
  where
    go g =
      let (result, g') = generateByDifficulty difficulty g
      in result : go g'

difficultyTags :: Difficulty -> [String]
difficultyTags Easy   = ["easy",  "quadratic"]
difficultyTags Medium = ["medium"]
difficultyTags Hard   = ["hard"]
difficultyTags Mixed  = ["mixed"]
