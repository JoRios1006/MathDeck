module Polynomial where

import Types

degree :: Polynomial -> Int
degree (Polynomial coeffs) = length coeffs - 1

evaluate :: Polynomial -> Rational -> Rational
evaluate (Polynomial coeffs) x =
  sum $ zipWith (\c e -> c * x ^ e) coeffs (reverse [0 .. length coeffs - 1])

addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Polynomial a) (Polynomial b) =
  let la = length a
      lb = length b
      padded_a = replicate (lb - la) 0 ++ a
      padded_b = replicate (la - lb) 0 ++ b
  in Polynomial $ zipWith (+) padded_a padded_b

scalePoly :: Rational -> Polynomial -> Polynomial
scalePoly s (Polynomial coeffs) = Polynomial (map (* s) coeffs)

multiplyPoly :: Polynomial -> Polynomial -> Polynomial
multiplyPoly (Polynomial a) (Polynomial b) =
  let n = length a + length b - 1
      result = foldl addPoly (Polynomial (replicate n 0))
                 [ scalePoly ca (Polynomial (replicate i 0 ++ [1] ++ replicate (length b - 1) 0))
                 | (ca, i) <- zip a [length b - 1, length b - 2 .. 0]
                 ]
  in result

fromRoots :: [Rational] -> Polynomial
fromRoots [] = Polynomial [1]
fromRoots (r:rs) =
  multiplyPoly (Polynomial [1, -r]) (fromRoots rs)

trimPoly :: Polynomial -> Polynomial
trimPoly (Polynomial coeffs) =
  let trimmed = dropWhile (== 0) coeffs
  in if null trimmed then Polynomial [0] else Polynomial trimmed
