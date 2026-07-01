module PrettyPrint where

import Types hiding (numerator, denominator)
import Data.List (intercalate)
import qualified Data.Ratio as R

fracLatex :: Integer -> Integer -> String
fracLatex n 1 = show n
fracLatex n d = "\\frac{" ++ show n ++ "}{" ++ show d ++ "}"

showRationalLatex :: Rational -> String
showRationalLatex r =
  let n = R.numerator r
      d = R.denominator r
  in if n < 0
     then "-" ++ fracLatex (abs n) d
     else fracLatex n d

showTermLatex :: Rational -> Int -> String
showTermLatex coeff 0 = showRationalLatex coeff
showTermLatex coeff 1
  | coeff == 1  = "x"
  | coeff == -1 = "-x"
  | otherwise   = showRationalLatex coeff ++ "x"
showTermLatex coeff e
  | coeff == 1  = "x^{" ++ show e ++ "}"
  | coeff == -1 = "-x^{" ++ show e ++ "}"
  | otherwise   = showRationalLatex coeff ++ "x^{" ++ show e ++ "}"

showPolyLatex :: Polynomial -> String
showPolyLatex (Polynomial []) = "0"
showPolyLatex (Polynomial coeffs) =
  let n     = length coeffs - 1
      pairs = filter (\(c, _) -> c /= 0) $ zip coeffs [n, n-1 .. 0]
  in case pairs of
    [] -> "0"
    (first:rest) ->
      showTermLatex (fst first) (snd first) ++
      concatMap (\(c, e) ->
        if c > 0
          then " + " ++ showTermLatex c e
          else " - " ++ showTermLatex (abs c) e
      ) rest

showIntegralLatex :: Polynomial -> String
showIntegralLatex p = showPolyLatex p ++ " + C"

showR :: Rational -> String
showR = showRationalLatex

showRootLatex :: (Rational, Int) -> String
showRootLatex (r, 1) = showRationalLatex r
showRootLatex (r, m) = showRationalLatex r ++ "\\;(\\times " ++ show m ++ ")"

showRootsLatex :: [(Rational, Int)] -> String
showRootsLatex [] = "\\varnothing"
showRootsLatex rs = intercalate ", \\;" (map showRootLatex rs)

showIntervalsLatex :: [(SignInterval, String)] -> SignInterval -> String
showIntervalsLatex ivs sign =
  let matching = [l | (s, l) <- ivs, s == sign]
  in if null matching
     then "\\varnothing"
     else intercalate " \\cup " matching
