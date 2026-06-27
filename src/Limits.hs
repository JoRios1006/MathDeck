module Limits where

import Types
import Polynomial

data LimitResult
  = Finite Rational
  | PosInfinity
  | NegInfinity
  | DoesNotExist
  deriving (Show, Eq)

limitAt :: Polynomial -> Rational -> LimitResult
limitAt p x = Finite (evaluate p x)

limitRationalAt :: RationalFunc -> Rational -> LimitResult
limitRationalAt (RationalFunc num den) x =
  let numVal = evaluate num x
      denVal = evaluate den x
  in if denVal /= 0
     then Finite (numVal / denVal)
     else if numVal == 0
          then removableDiscontinuity (RationalFunc num den) x
          else DoesNotExist

removableDiscontinuity :: RationalFunc -> Rational -> LimitResult
removableDiscontinuity (RationalFunc num den) x =
  let factor = Polynomial [1, -x]
      num' = cancelFactor num factor
      den' = cancelFactor den factor
      denVal = evaluate den' x
  in if denVal /= 0
     then Finite (evaluate num' x / denVal)
     else DoesNotExist

cancelFactor :: Polynomial -> Polynomial -> Polynomial
cancelFactor (Polynomial coeffs) _ = Polynomial coeffs

endLimit :: Polynomial -> String
endLimit p@(Polynomial coeffs)
  | null coeffs = "0"
  | head coeffs > 0 = "∞"
  | otherwise = "-∞"

endLimitNeg :: Polynomial -> String
endLimitNeg p@(Polynomial coeffs)
  | null coeffs = "0"
  | even (length coeffs - 1) =
      if head coeffs > 0 then "∞" else "-∞"
  | otherwise =
      if head coeffs > 0 then "-∞" else "∞"
