module Calculus where

import Types
import Polynomial

differentiate :: Polynomial -> Polynomial
differentiate (Polynomial coeffs) =
  let n = length coeffs - 1
      pairs = zip coeffs [n, n-1 .. 1]
      derived = map (\(c, e) -> c * fromIntegral e) pairs
  in trimPoly (Polynomial derived)

integrate :: Polynomial -> Polynomial
integrate (Polynomial coeffs) =
  let n = length coeffs - 1
      integrated = zipWith (\c e -> c / fromIntegral (e + 1)) coeffs [n, n-1 .. 0]
  in Polynomial (integrated ++ [0])

definiteIntegral :: Polynomial -> Rational -> Rational -> Rational
definiteIntegral p a b =
  let antideriv = integrate p
  in evaluate antideriv b - evaluate antideriv a
