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

generateDistinctInts :: RNG -> Int -> [Int] -> ([Int], RNG)
generateDistinctInts rng 0 _       = ([], rng)
generateDistinctInts rng n exclude =
  let (v, rng') = randomIntR rng (-4) 4
  in if v `elem` exclude
     then generateDistinctInts rng' n exclude
     else let (rest, rng'') = generateDistinctInts rng' (n-1) (v:exclude)
          in (v:rest, rng'')

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
  let (numIs, rng')  = generateDistinctInts rng  2 []
      (denIs, rng'') = generateDistinctInts rng' 2 numIs
      numRs = map fromIntegral numIs
      denRs = map fromIntegral denIs
  in (RationalFunc (fromRoots numRs) (fromRoots denRs), rng'')

generateRationalHole :: RNG -> (RationalFunc, RNG)
generateRationalHole rng =
  let (abIs, rng')  = generateDistinctInts rng  2 []
      a = head abIs
      b = abIs !! 1
      (cIs, rng'') = generateDistinctInts rng' 1 [a, b]
      c = head cIs
      numRs = map fromIntegral [a, b]
      denRs = map fromIntegral [b, c]
  in (RationalFunc (fromRoots numRs) (fromRoots denRs), rng'')

generateByDifficulty :: Difficulty -> RNG -> (Either Polynomial RationalFunc, RNG)
generateByDifficulty Easy   rng = let (p, g) = generateQuadratic rng in (Left p, g)
generateByDifficulty Medium rng =
  let (n, rng') = randomIntR rng 0 2
  in case n of
    0 -> let (p, g) = generateCubic   rng' in (Left p, g)
    1 -> let (p, g) = generateQuartic rng' in (Left p, g)
    _ -> let (p, g) = generateCubic   rng' in (Left p, g)
generateByDifficulty Hard rng =
  let (n, rng') = randomIntR rng 0 1
  in if n == 0
     then let (r, g) = generateRational     rng' in (Right r, g)
     else let (r, g) = generateRationalHole rng' in (Right r, g)
generateByDifficulty Mixed rng =
  let (n, rng') = randomIntR rng 0 3
  in case n of
    0 -> let (p, g) = generateQuadratic     rng' in (Left p, g)
    1 -> let (p, g) = generateCubic         rng' in (Left p, g)
    2 -> let (p, g) = generateQuartic       rng' in (Left p, g)
    _ -> let (r, g) = generateRational      rng' in (Right r, g)
