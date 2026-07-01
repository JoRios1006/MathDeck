module CSV where

import Data.List (nubBy)
import Data.Maybe (catMaybes)
import Types (Polynomial, roots, SignChart(..), SignInterval(..))
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

div' :: String -> String -> String
div' style content = "<div style='" ++ style ++ "'>" ++ content ++ "</div>"

h3' :: String -> String
h3' t = "<h3 style='font-size:15px;color:#555;margin:8px 0 4px;'>" ++ t ++ "</h3>"

p' :: String -> String
p' t = "<p style='margin:0 0 8px;'>" ++ t ++ "</p>"

hr' :: String
hr' = "<hr style='border:none;border-top:1px solid #ddd;margin:10px 0;'>"

inlineMath :: String -> String
inlineMath s = "\\(" ++ s ++ "\\)"

displayMath :: String -> String
displayMath s = "\\[" ++ s ++ "\\]"

frontBase :: String -> String
frontBase tex =
  div' "font-family:Georgia;text-align:center;padding:20px;font-size:22px;"
    (displayMath tex)

frontQuestion :: String -> String -> String
frontQuestion tex question =
  div' "font-family:Georgia;text-align:center;padding:20px;"
    ( div' "font-size:22px;" (displayMath tex)
    ++ hr'
    ++ div' "font-size:17px;color:#555;margin-top:8px;" question
    )

backSimple :: String -> String
backSimple tex =
  div' "font-family:Georgia;text-align:center;padding:20px;font-size:18px;"
    (displayMath tex)

backList :: [(String, String)] -> String -> String
backList sections footer =
  div' "font-family:Georgia;padding:15px;font-size:14px;line-height:1.6;"
    (  concatMap renderSection sections
    ++ if null footer then "" else
         "<p style='font-size:12px;color:#999;margin:8px 0 0;'>" ++ footer ++ "</p>"
    )
  where
    renderSection (title, body) = h3' title ++ p' body ++ hr'

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

polyTex :: Polynomial -> String
polyTex = showPolyLatex

fullAnalysisCard :: Polynomial -> [String] -> Maybe AnkiCard
fullAnalysisCard p tags =
  let sc    = buildSignChart p
      rs    = roots sc
      deriv = differentiate p
      integ = integrate p
      posIv = showIntervalsLatex (intervals sc) Positive
      negIv = showIntervalsLatex (intervals sc) Negative
      sections =
        [ ("Zeros",                  inlineMath (showRootsLatex rs))
        , (inlineMath "f(x) > 0",   inlineMath posIv)
        , (inlineMath "f(x) < 0",   inlineMath negIv)
        , ("Derivative",             displayMath ("f'(x) = " ++ polyTex deriv))
        , ("Integral",               displayMath ("\\int f(x)\\,dx = " ++ showIntegralLatex integ))
        , ("End behavior",           inlineMath ("x \\to +\\infty: " ++ endLimit p)
                                  ++ "<br>" ++
                                     inlineMath ("x \\to -\\infty: " ++ endLimitNeg p))
        ]
  in Just $ AnkiCard
      (frontBase ("f(x) = " ++ polyTex p))
      (backList sections ("Degree: " ++ show (degree p)))
      ("polynomial" : "full-analysis" : tags)

derivativeCard :: Polynomial -> [String] -> Maybe AnkiCard
derivativeCard p tags =
  let deriv = differentiate p
  in Just $ AnkiCard
      (frontQuestion ("f(x) = " ++ polyTex p) ("Find " ++ inlineMath "f'(x)"))
      (backSimple ("f'(x) = " ++ polyTex deriv))
      ("polynomial" : "derivative" : tags)

zerosCard :: Polynomial -> [String] -> Maybe AnkiCard
zerosCard p tags =
  let sc = buildSignChart p
      rs = roots sc
      answer = if null rs
               then "No real zeros"
               else displayMath (showRootsLatex rs)
  in Just $ AnkiCard
      (frontQuestion ("f(x) = " ++ polyTex p) ("Find all zeros of " ++ inlineMath "f(x)"))
      (div' "font-family:Georgia;text-align:center;padding:20px;font-size:18px;" answer)
      ("polynomial" : "zeros" : tags)

integralCard :: Polynomial -> [String] -> Maybe AnkiCard
integralCard p tags =
  let integ = integrate p
  in Just $ AnkiCard
      (frontQuestion ("f(x) = " ++ polyTex p) ("Find " ++ inlineMath "\\int f(x)\\,dx"))
      (backSimple (showIntegralLatex integ))
      ("polynomial" : "integral" : tags)

signCard :: Polynomial -> [String] -> Maybe AnkiCard
signCard p tags =
  let sc    = buildSignChart p
      posIv = showIntervalsLatex (intervals sc) Positive
      negIv = showIntervalsLatex (intervals sc) Negative
      sections =
        [ (inlineMath "f(x) > 0", inlineMath posIv)
        , (inlineMath "f(x) < 0", inlineMath negIv)
        ]
  in Just $ AnkiCard
      (frontQuestion ("f(x) = " ++ polyTex p)
        ("Where is " ++ inlineMath "f(x) > 0" ++ "? Where is " ++ inlineMath "f(x) < 0" ++ "?"))
      (backList sections "")
      ("polynomial" : "sign-chart" : tags)

definiteIntCard :: Polynomial -> [String] -> Maybe AnkiCard
definiteIntCard p tags =
  let sc         = buildSignChart p
      distinctRs = map fst (roots sc)
  in case distinctRs of
    (a:b:_) ->
      let result = definiteIntegral p a b
          aTex   = showRationalLatex a
          bTex   = showRationalLatex b
          qTex   = "\\int_{" ++ aTex ++ "}^{" ++ bTex ++ "} f(x)\\,dx"
          ansTex = showRationalLatex result
      in Just $ AnkiCard
          (frontQuestion ("f(x) = " ++ polyTex p) ("Evaluate " ++ inlineMath qTex))
          (backSimple ansTex)
          ("polynomial" : "definite-integral" : tags)
    _ -> Nothing

deduplicateCards :: [AnkiCard] -> [AnkiCard]
deduplicateCards = nubBy (\a b -> front a == front b)

writeCSVFile :: FilePath -> [AnkiCard] -> IO ()
writeCSVFile path cards = writeFile path (cardsToCSV cards)
