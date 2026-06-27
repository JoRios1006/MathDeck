module CSV where

import Data.List (nubBy)
import Types (Polynomial, roots, SignChart(..))
import PrettyPrint (showPolynomial, showSignChart, showIntegral, showR, showEndBehavior)
import Calculus (differentiate, integrate)
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

polynomialCard :: Polynomial -> [String] -> Maybe AnkiCard
polynomialCard p extraTags
  | isZeroPoly p = Nothing
  | otherwise =
      let frontStr = "f(x) = " ++ showPolynomial p
          deriv    = differentiate p
          integ    = integrate p
          sc       = buildSignChart p
          rs       = roots sc
          backStr  = unlines
            [ "Roots"
            , ""
            , concatMap (\r -> showR r ++ "\n") rs
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
            ]
          ts = ["polynomial"] ++ extraTags
      in Just (AnkiCard frontStr backStr ts)

deduplicateCards :: [AnkiCard] -> [AnkiCard]
deduplicateCards = nubBy (\a b -> front a == front b)

writeCSVFile :: FilePath -> [AnkiCard] -> IO ()
writeCSVFile path cards = writeFile path (cardsToCSV cards)
