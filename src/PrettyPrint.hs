module PrettyPrint where

import Types
import Polynomial
import Data.List (intercalate)

showRational :: Rational -> String
showRational r
  | denominator r == 1 = show (numerator r)
  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)
  where
    numerator x = floor x
    denominator x =
      head $ filter (\d -> x * fromIntegral d == fromIntegral (round (x * fromIntegral d) :: Integer)) [1..1000 :: Integer]

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
  let n = length coeffs - 1
      pairs = filter (\(c,_) -> c /= 0) $ zip coeffs [n, n-1 .. 0]
  in case pairs of
    [] -> "0"
    (first:rest) ->
      showTerm (fst first) (snd first) ++
      concatMap (\(c, e) ->
        if c > 0
          then " + " ++ showTerm c e
          else " - " ++ showTerm (abs c) e
      ) rest

showIntegral :: Polynomial -> String
showIntegral p = showPolynomial p ++ " + C"

showSignChart :: Types.SignChart -> String
showSignChart (Types.SignChart rs ivs) =
  "Roots\n\n" ++ intercalate "\n" (map showR rs) ++
  "\n\nPositive\n\n" ++ intercalate "\n" [l | (Positive, l) <- ivs] ++
  "\n\nNegative\n\n" ++ intercalate "\n" [l | (Negative, l) <- ivs]

showEndBehavior :: Types.EndBehavior -> String
showEndBehavior (Types.EndBehavior pos neg) =
  "x→∞\n\n" ++ pos ++ "\n\nx→−∞\n\n" ++ neg

showR :: Rational -> String
showR r =
  let n = floor r :: Integer
      d = 1 :: Integer
  in if fromIntegral n == r then show n else show (fromRational r :: Double)
