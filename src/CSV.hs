module CSV where

import Data.List (nubBy)
import Data.Maybe (catMaybes)
import Types (Polynomial, roots, SignChart(..))
import PrettyPrint
import Calculus (differentiate, integrate, definiteIntegral)
import SignChart (buildSignChart)
import Limits (endLimit, endLimitNeg)
import Polynomial (isZeroPoly, degree)

data AnkiCard = AnkiCard
  { front :: String
  , back  :: String
  , tags  :: [String]
  } deriving (Show, Eq)

cardToCSV :: AnkiCard -> String
cardToCSV (AnkiCard f b ts) =
  "\"" ++ escape f ++ "\",\"" ++ escape b ++ "\",\"" ++ unwords ts ++ "\""
  where
    escape = concatMap (\c -> if c == '"' then "\"\"" else [c])

cardsToCSV :: [AnkiCard] -> String
cardsToCSV cards =
  "Front,Back,Tags\n" ++ unlines (map cardToCSV cards)

polynomialCards :: Polynomial -> [String] -> [AnkiCard]
polynomialCards p extraTags
  | isZeroPoly p = []
  | otherwise    = catMaybes
      [ fullAnalysisCard p extraTags
      , derivativeCard   p extraTags
      , zerosCard        p extraTags
      , integralCard     p extraTags
      , signCard         p extraTags
      , definiteIntCard  p extraTags
      ]

funcStr :: Polynomial -> String
funcStr p = "f(x) = " ++ showPolynomial p

fullAnalysisCard :: Polynomial -> [String] -> Maybe AnkiCard
fullAnalysisCard p tags =
  let sc      = buildSignChart p
      rs      = roots sc
      deriv   = differentiate p
      integ   = integrate p
  in Just $ AnkiCard
      (funcStr p)
      (unlines
        [ "Zeros"
        , ""
        , showRoots rs
        , ""
        , showSignChart sc
        , ""
        , "Derivative"
        , ""
        , showPolynomial deriv
        , ""
        , "Integral"
        , ""
        , showIntegral integ
        , ""
        , "End behavior"
        , ""
        , showEndBehavior (endLimit p) (endLimitNeg p)
        , ""
        , "Degree: " ++ show (degree p)
        ])
      ("polynomial" : "full-analysis" : tags)

derivativeCard :: Polynomial -> [String] -> Maybe AnkiCard
derivativeCard p tags =
  let deriv = differentiate p
  in Just $ AnkiCard
      (funcStr p ++ "\n\nFind f'(x)")
      ("f'(x) = " ++ showPolynomial deriv)
      ("polynomial" : "derivative" : tags)

zerosCard :: Polynomial -> [String] -> Maybe AnkiCard
zerosCard p tags =
  let sc = buildSignChart p
      rs = roots sc
  in Just $ AnkiCard
      (funcStr p ++ "\n\nFind all zeros of f(x)")
      (if null rs then "No real zeros" else showRoots rs)
      ("polynomial" : "zeros" : tags)

integralCard :: Polynomial -> [String] -> Maybe AnkiCard
integralCard p tags =
  let integ = integrate p
  in Just $ AnkiCard
      (funcStr p ++ "\n\nFind ∫ f(x) dx")
      (showIntegral integ)
      ("polynomial" : "integral" : tags)

signCard :: Polynomial -> [String] -> Maybe AnkiCard
signCard p tags =
  let sc = buildSignChart p
  in Just $ AnkiCard
      (funcStr p ++ "\n\nWhere is f(x) > 0? Where is f(x) < 0?")
      (showSignChart sc)
      ("polynomial" : "sign-chart" : tags)

definiteIntCard :: Polynomial -> [String] -> Maybe AnkiCard
definiteIntCard p tags =
  let sc          = buildSignChart p
      distinctRs  = map fst (roots sc)
  in case distinctRs of
    (a:b:_) ->
      let result = definiteIntegral p a b
          front  = funcStr p ++ "\n\nEvaluate ∫_{" ++ showR a ++ "}^{" ++ showR b ++ "} f(x) dx"
          back   = showRational result
      in Just $ AnkiCard front back ("polynomial" : "definite-integral" : tags)
    _ -> Nothing

deduplicateCards :: [AnkiCard] -> [AnkiCard]
deduplicateCards = nubBy (\a b -> front a == front b)

writeCSVFile :: FilePath -> [AnkiCard] -> IO ()
writeCSVFile path cards = writeFile path (cardsToCSV cards)
