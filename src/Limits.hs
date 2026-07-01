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
  let num' = cancelFactor num (Polynomial [1, -x])
      den' = cancelFactor den (Polynomial [1, -x])
      denVal = evaluate den' x
  in if denVal /= 0
     then Finite (evaluate num' x / denVal)
     else DoesNotExist

cancelFactor :: Polynomial -> Polynomial -> Polynomial
cancelFactor (Polynomial coeffs) _ = Polynomial coeffs

endLimit :: Polynomial -> String
endLimit (Polynomial coeffs)
  | null coeffs    = "0"
  | head coeffs > 0 = "\\infty"
  | otherwise       = "-\\infty"

endLimitNeg :: Polynomial -> String
endLimitNeg (Polynomial coeffs)
  | null coeffs = "0"
  | even (length coeffs - 1) =
      if head coeffs > 0 then "\\infty" else "-\\infty"
  | otherwise =
      if head coeffs > 0 then "-\\infty" else "\\infty"
