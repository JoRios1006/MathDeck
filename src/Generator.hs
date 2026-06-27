module Generator where

import Types
import Polynomial

newtype RNG = RNG Integer deriving (Show)

mkRNG :: Integer -> RNG
mkRNG seed = RNG (seed `mod` 2147483647)

nextRNG :: RNG -> (Integer, RNG)
nextRNG (RNG s) =
  let s' = (s * 1664525 + 1013904223) `mod` 2147483647
  in (s', RNG s')

randomIntR :: RNG -> Int -> Int -> (Int, RNG)
randomIntR rng lo hi =
  let (n, rng') = nextRNG rng
      range = fromIntegral (hi - lo + 1)
      val = lo + fromIntegral (n `mod` range)
  in (val, rng')

randomRoots :: RNG -> Int -> ([Rational], RNG)
randomRoots rng count =
  foldl (\(acc, g) _ ->
      let (n, g') = randomIntR g (-5) 5
      in (fromIntegral n : acc, g'))
    ([], rng)
    [1..count]

generateQuadratic :: RNG -> (Polynomial, RNG)
generateQuadratic rng =
  let (roots, rng') = randomRoots rng 2
  in (fromRoots roots, rng')

generateCubic :: RNG -> (Polynomial, RNG)
generateCubic rng =
  let (roots, rng') = randomRoots rng 3
  in (fromRoots roots, rng')

generateQuartic :: RNG -> (Polynomial, RNG)
generateQuartic rng =
  let (roots, rng') = randomRoots rng 4
  in (fromRoots roots, rng')

generateRational :: RNG -> (RationalFunc, RNG)
generateRational rng =
  let (numRoots, rng')  = randomRoots rng 2
      (denRoots, rng'') = randomRoots rng' 1
  in (RationalFunc (fromRoots numRoots) (fromRoots denRoots), rng'')

generateByDifficulty :: Difficulty -> RNG -> (Either Polynomial RationalFunc, RNG)
generateByDifficulty Easy   rng = let (p, g) = generateQuadratic rng in (Left p, g)
generateByDifficulty Medium rng =
  let (n, rng') = randomIntR rng 0 1
  in if n == 0
     then let (p, g) = generateCubic rng' in (Left p, g)
     else let (p, g) = generateQuartic rng' in (Left p, g)
generateByDifficulty Hard   rng = let (r, g) = generateRational rng in (Right r, g)
generateByDifficulty Mixed  rng =
  let (n, rng') = randomIntR rng 0 2
  in case n of
    0 -> let (p, g) = generateQuadratic rng' in (Left p, g)
    1 -> let (p, g) = generateCubic rng' in (Left p, g)
    _ -> let (r, g) = generateRational rng' in (Right r, g)
