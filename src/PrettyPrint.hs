module PrettyPrint where

import Types hiding (numerator, denominator)
import Data.List (intercalate)
import qualified Data.Ratio as R

showRational :: Rational -> String
showRational r
  | R.denominator r == 1 = show (R.numerator r)
  | otherwise            = show (R.numerator r) ++ "/" ++ show (R.denominator r)

superscript :: Int -> String
superscript 0 = ""
superscript 1 = ""
superscript 2 = "²"
superscript 3 = "³"
superscript 4 = "⁴"
superscript 5 = "⁵"
superscript n = "^" ++ show n

showTerm :: Rational -> Int -> String
showTerm coeff 0 = showRational coeff
showTerm coeff 1
  | coeff == 1  = "x"
  | coeff == -1 = "-x"
  | otherwise   = showRational coeff ++ "x"
showTerm coeff e
  | coeff == 1  = "x" ++ superscript e
  | coeff == -1 = "-x" ++ superscript e
  | otherwise   = showRational coeff ++ "x" ++ superscript e

showPolynomial :: Polynomial -> String
showPolynomial (Polynomial []) = "0"
showPolynomial (Polynomial coeffs) =
  let n     = length coeffs - 1
      pairs = filter (\(c, _) -> c /= 0) $ zip coeffs [n, n-1 .. 0]
  in case pairs of
    []           -> "0"
    (first:rest) ->
      showTerm (fst first) (snd first) ++
      concatMap (\(c, e) ->
        if c > 0
          then " + " ++ showTerm c e
          else " - " ++ showTerm (abs c) e
      ) rest

showIntegral :: Polynomial -> String
showIntegral p = showPolynomial p ++ " + C"

showR :: Rational -> String
showR r = showRational r

showSignChart :: Types.SignChart -> String
showSignChart (Types.SignChart rs ivs) =
  let posIntervals = [l | (Positive, l) <- ivs]
      negIntervals = [l | (Negative, l) <- ivs]
      posStr = if null posIntervals then "(none)" else intercalate "\n" posIntervals
      negStr = if null negIntervals then "(none)" else intercalate "\n" negIntervals
  in "Positive\n\n" ++ posStr ++ "\n\nNegative\n\n" ++ negStr

showEndBehavior :: String -> String -> String
showEndBehavior posInf negInf =
  "x→∞: " ++ posInf ++ "\nx→−∞: " ++ negInf
