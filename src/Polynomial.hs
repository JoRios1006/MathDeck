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
  let na = length a
      nb = length b
      n  = na + nb - 1
      coeffs = [ sum [ a !! i * b !! j
                     | i <- [0 .. na - 1]
                     , j <- [0 .. nb - 1]
                     , i + j == k ]
               | k <- [0 .. n - 1] ]
  in Polynomial coeffs

fromRoots :: [Rational] -> Polynomial
fromRoots [] = Polynomial [1]
fromRoots (r:rs) =
  multiplyPoly (Polynomial [1, -r]) (fromRoots rs)

trimPoly :: Polynomial -> Polynomial
trimPoly (Polynomial coeffs) =
  let trimmed = dropWhile (== 0) coeffs
  in if null trimmed then Polynomial [0] else Polynomial trimmed

isZeroPoly :: Polynomial -> Bool
isZeroPoly p = trimPoly p == Polynomial [0]

synthDiv :: Polynomial -> Rational -> Polynomial
synthDiv (Polynomial []) _ = Polynomial []
synthDiv (Polynomial coeffs) r =
  let (quotCoeffs, _) = foldl
        (\(qs, carry) c ->
          let carry' = carry * r + c
          in (qs ++ [carry'], carry'))
        ([], 0)
        coeffs
  in trimPoly (Polynomial (init quotCoeffs))

multiplicity :: Polynomial -> Rational -> Int
multiplicity p r
  | evaluate p r /= 0 = 0
  | otherwise         = 1 + multiplicity (synthDiv p r) r
