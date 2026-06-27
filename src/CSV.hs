module CSV where

import Types (Polynomial, roots, SignChart(..))
import PrettyPrint (showPolynomial, showSignChart, showIntegral, showR)
import Calculus (differentiate, integrate)
import SignChart (buildSignChart)

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

polynomialCard :: Polynomial -> [String] -> AnkiCard
polynomialCard p extraTags =
  let frontStr = "f(x) = " ++ showPolynomial p
      deriv = differentiate p
      integ = integrate p
      sc    = buildSignChart p
      backStr = unlines
        [ "Roots"
        , ""
        , concatMap (\r -> showR r ++ "\n") (roots sc)
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
        ]
      ts = ["polynomial"] ++ extraTags
  in AnkiCard frontStr backStr ts

writeCSVFile :: FilePath -> [AnkiCard] -> IO ()
writeCSVFile path cards = writeFile path (cardsToCSV cards)
