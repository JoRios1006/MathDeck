module SignChart where

import Types
import Polynomial

findRoots :: Polynomial -> [Rational]
findRoots (Polynomial coeffs) =
  let candidates = concatMap (\n -> [fromIntegral n, fromIntegral (-n)]) [1..20] ++ [0]
  in filter (\r -> evaluate (Polynomial coeffs) r == 0) candidates

buildSignChart :: Polynomial -> SignChart
buildSignChart p =
  let rs = findRoots p
      sorted = foldr insertSorted [] rs
      intervals = buildIntervals p sorted
  in SignChart sorted intervals

insertSorted :: Rational -> [Rational] -> [Rational]
insertSorted x [] = [x]
insertSorted x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertSorted x ys

buildIntervals :: Polynomial -> [Rational] -> [(SignInterval, String)]
buildIntervals p [] =
  let testVal = 0
      sign = if evaluate p testVal > 0 then Positive else Negative
  in [(sign, "(-∞,∞)")]
buildIntervals p roots =
  let allPoints = roots
      testPoints = (-1000) : map (\(a,b) -> (a+b)/2) (zip allPoints (tail allPoints)) ++ [1000]
      labeledIntervals = zipWith3 makeInterval
        (Nothing : map Just allPoints)
        (map Just allPoints ++ [Nothing])
        testPoints
  in labeledIntervals
  where
    makeInterval lo hi testPt =
      let sign = if evaluate p testPt > 0 then Positive else Negative
          label = intervalLabel lo hi
      in (sign, label)
    intervalLabel Nothing (Just b) = "(-∞," ++ showR b ++ ")"
    intervalLabel (Just a) Nothing  = "(" ++ showR a ++ ",∞)"
    intervalLabel (Just a) (Just b) = "(" ++ showR a ++ "," ++ showR b ++ ")"
    intervalLabel Nothing Nothing   = "(-∞,∞)"

showR :: Rational -> String
showR r =
  let n = numerator r
      d = denominator r
  in if d == 1 then show n else show n ++ "/" ++ show d
  where
    numerator rat = floor (rat * fromIntegral (denom rat))
    denominator = denom
    denom rat =
      let candidates = [1..100] :: [Integer]
      in head $ filter (\d -> rat * fromIntegral d == fromIntegral (round (rat * fromIntegral d) :: Integer)) candidates
