module SignChart where

import Types
import Polynomial
import qualified Data.Ratio as R

findRoots :: Polynomial -> [Rational]
findRoots (Polynomial coeffs) =
  let candidates = concatMap (\n -> [fromIntegral n, fromIntegral (-n)]) [1..20] ++ [0]
  in filter (\r -> evaluate (Polynomial coeffs) r == 0) candidates

buildSignChart :: Polynomial -> SignChart
buildSignChart p =
  let uniqueRoots = findRoots p
      sorted      = foldr insertSorted [] uniqueRoots
      mults       = map (\r -> (r, multiplicity p r)) sorted
      ivs         = buildIntervals p sorted
  in SignChart mults ivs

insertSorted :: Rational -> [Rational] -> [Rational]
insertSorted x [] = [x]
insertSorted x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertSorted x ys

buildIntervals :: Polynomial -> [Rational] -> [(SignInterval, String)]
buildIntervals p [] =
  let sign = if evaluate p 0 > 0 then Positive else Negative
  in [(sign, "(-\\infty,\\infty)")]
buildIntervals p sortedRoots =
  let testPoints = (-1000) : midpoints sortedRoots ++ [1000]
      boundaries = zip (Nothing : map Just sortedRoots)
                       (map Just sortedRoots ++ [Nothing])
  in zipWith3 makeInterval
       (map fst boundaries)
       (map snd boundaries)
       testPoints
  where
    midpoints []       = []
    midpoints [_]      = []
    midpoints (a:b:xs) = (a + b) / 2 : midpoints (b:xs)

    makeInterval lo hi testPt =
      let sign  = if evaluate p testPt > 0 then Positive else Negative
      in (sign, intervalLabel lo hi)

    intervalLabel Nothing  (Just b) = "(-\\infty," ++ showR b ++ ")"
    intervalLabel (Just a) Nothing  = "(" ++ showR a ++ ",\\infty)"
    intervalLabel (Just a) (Just b) = "(" ++ showR a ++ "," ++ showR b ++ ")"
    intervalLabel Nothing  Nothing  = "(-\\infty,\\infty)"

showR :: Rational -> String
showR r
  | R.denominator r == 1 = show (R.numerator r)
  | otherwise            = show (R.numerator r) ++ "/" ++ show (R.denominator r)
