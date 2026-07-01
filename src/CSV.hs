module CSV where

import Data.List (intercalate, nubBy, sort, nub)
import Data.Maybe (catMaybes)
import Types (Polynomial(..), RationalFunc(..), roots, SignChart(..), SignInterval(..))
import PrettyPrint
import Calculus (differentiate, integrate, definiteIntegral)
import SignChart (buildSignChart, findRoots)
import Limits (endLimit, endLimitNeg)
import Polynomial (isZeroPoly, degree, evaluate, synthDiv)

-- ── Types ─────────────────────────────────────────────────────────────────────

data AnkiCard = AnkiCard
  { front :: String
  , back  :: String
  , tags  :: [String]
  } deriving (Show, Eq)

-- ── CSV output ────────────────────────────────────────────────────────────────

cardToCSV :: AnkiCard -> String
cardToCSV (AnkiCard f b ts) =
  "\"" ++ escape f ++ "\",\"" ++ escape b ++ "\",\"" ++ unwords ts ++ "\""
  where escape = concatMap (\c -> if c == '"' then "\"\"" else [c])

cardsToCSV :: [AnkiCard] -> String
cardsToCSV cards = "Front,Back,Tags\n" ++ unlines (map cardToCSV cards)

-- ── CSS constants ─────────────────────────────────────────────────────────────

mathFont :: String
mathFont = "\"STIX Two Text\",\"Cambria Math\",serif"

bodyStyle :: String
bodyStyle = "font-family:" ++ mathFont ++ ";padding:24px;font-size:14px;line-height:1.7;"

centerStyle :: String
centerStyle = "font-family:" ++ mathFont ++ ";text-align:center;padding:24px;"

-- ── HTML primitives ───────────────────────────────────────────────────────────

div' :: String -> String -> String
div' style content = "<div style='" ++ style ++ "'>" ++ content ++ "</div>"

h3' :: String -> String
h3' t = "<h3 style='font-size:15px;color:#2563eb;margin:10px 0 4px;'>" ++ t ++ "</h3>"

p' :: String -> String
p' t = "<p style='margin:0 0 8px;'>" ++ t ++ "</p>"

hr' :: String
hr' = "<hr style='border:none;border-top:1px solid #e8e8e8;margin:10px 0;'>"

inlineMath :: String -> String
inlineMath s = "\\(" ++ s ++ "\\)"

displayMath :: String -> String
displayMath s = "\\[" ++ s ++ "\\]"

-- ── Card layout ───────────────────────────────────────────────────────────────

-- Prompt first, separator, then big function (matches natural reading order)
frontQ :: String -> String -> String
frontQ question funcTex =
  div' centerStyle
    ( div' "font-size:18px;color:#666;margin-bottom:12px;" question
    ++ hr'
    ++ div' "font-size:30px;" (displayMath funcTex)
    )

-- Full-analysis front: just the function at 30px (function IS the prompt)
frontF :: String -> String
frontF funcTex =
  div' centerStyle
    (div' "font-size:30px;" (displayMath funcTex))

-- Simple centered answer with footer
backS :: String -> [String] -> String -> String
backS tex tgs cardType =
  div' (centerStyle ++ "font-size:20px;")
    (displayMath tex ++ footerDiv tgs cardType)

-- Sectioned back (each section body is raw HTML, not wrapped in <p>)
backL :: [(String, String)] -> [String] -> String -> String
backL sections tgs cardType =
  div' bodyStyle
    (concatMap (\(title, body) -> h3' title ++ body ++ hr') sections
    ++ footerDiv tgs cardType)

-- Footer tag line: e.g. "Quadratic • Easy • Derivative"
footerDiv :: [String] -> String -> String
footerDiv tgs cardType =
  let kindMap = [("quadratic","Quadratic"),("cubic","Cubic"),
                 ("quartic","Quartic"),("rational","Rational")]
      diffMap = [("easy","Easy"),("medium","Medium"),("hard","Hard"),("mixed","Mixed")]
      kind = head ([v | (k,v) <- kindMap, k `elem` tgs] ++ ["Polynomial"])
      diff = head ([v | (k,v) <- diffMap, k `elem` tgs] ++ [""])
      label = kind ++ (if null diff then "" else " \x2022 " ++ diff) ++ " \x2022 " ++ cardType
  in "<div style='margin-top:18px;font-size:11px;color:#999;text-align:right;'>" ++ label ++ "</div>"

-- ── Polynomial card set ───────────────────────────────────────────────────────

polyTex :: Polynomial -> String
polyTex = showPolyLatex

polynomialCards :: Polynomial -> [String] -> [AnkiCard]
polynomialCards p extraTags
  | isZeroPoly p = []
  | otherwise    = catMaybes
      [ fullAnalysisCard  p extraTags
      , derivativeCard    p extraTags
      , zerosPolyCard     p extraTags
      , integralCard      p extraTags
      , signCard          p extraTags
      , definiteIntCard   p extraTags
      , criticalPointCard p extraTags
      ]

fullAnalysisCard :: Polynomial -> [String] -> Maybe AnkiCard
fullAnalysisCard p tgs =
  let sc    = buildSignChart p
      rs    = roots sc
      deriv = differentiate p
      integ = integrate p
      posIv = showIntervalsLatex (intervals sc) Positive
      negIv = showIntervalsLatex (intervals sc) Negative
      zerosBody | null rs   = p' (inlineMath "\\varnothing")
                | otherwise = displayMath (showRootsLatex rs)
      sections =
        [ ("Zeros",
             zerosBody)
        , (inlineMath "f(x) > 0",
             p' (inlineMath posIv))
        , (inlineMath "f(x) < 0",
             p' (inlineMath negIv))
        , ("Derivative",
             displayMath ("f'(x) = " ++ polyTex deriv))
        , ("Integral",
             displayMath ("\\int f(x)\\,dx = " ++ showIntegralLatex integ))
        , ("End behavior",
             displayMath ("\\lim_{x\\to+\\infty}f(x)=" ++ endLimit p)
          ++ displayMath ("\\lim_{x\\to-\\infty}f(x)=" ++ endLimitNeg p))
        ]
  in Just $ AnkiCard
      (frontF ("f(x) = " ++ polyTex p))
      (backL sections tgs ("Full Analysis \x2022 Degree " ++ show (degree p)))
      ("polynomial" : "full-analysis" : tgs)

derivativeCard :: Polynomial -> [String] -> Maybe AnkiCard
derivativeCard p tgs =
  let deriv = differentiate p
  in Just $ AnkiCard
      (frontQ ("Differentiate " ++ inlineMath "f(x)") ("f(x) = " ++ polyTex p))
      (backS ("f'(x) = " ++ polyTex deriv) tgs "Derivative")
      ("polynomial" : "derivative" : tgs)

zerosPolyCard :: Polynomial -> [String] -> Maybe AnkiCard
zerosPolyCard p tgs =
  let sc = buildSignChart p
      rs = roots sc
  in Just $ AnkiCard
      (frontQ ("Find all zeros of " ++ inlineMath "f(x)") ("f(x) = " ++ polyTex p))
      (div' (centerStyle ++ "font-size:20px;")
        ( (if null rs
           then p' (inlineMath "\\varnothing")
           else displayMath (showRootsLatex rs))
        ++ footerDiv tgs "Zeros"))
      ("polynomial" : "zeros" : tgs)

integralCard :: Polynomial -> [String] -> Maybe AnkiCard
integralCard p tgs =
  let integ = integrate p
  in Just $ AnkiCard
      (frontQ ("Find " ++ inlineMath "\\int f(x)\\,dx") ("f(x) = " ++ polyTex p))
      (backS (showIntegralLatex integ) tgs "Integral")
      ("polynomial" : "integral" : tgs)

signCard :: Polynomial -> [String] -> Maybe AnkiCard
signCard p tgs =
  let sc    = buildSignChart p
      posIv = showIntervalsLatex (intervals sc) Positive
      negIv = showIntervalsLatex (intervals sc) Negative
      sections =
        [ (inlineMath "f(x) > 0", p' (inlineMath posIv))
        , (inlineMath "f(x) < 0", p' (inlineMath negIv))
        ]
  in Just $ AnkiCard
      (frontQ ("Where is " ++ inlineMath "f(x) > 0" ++ "?  Where is " ++ inlineMath "f(x) < 0" ++ "?")
              ("f(x) = " ++ polyTex p))
      (backL sections tgs "Sign Chart")
      ("polynomial" : "sign-chart" : tgs)

definiteIntCard :: Polynomial -> [String] -> Maybe AnkiCard
definiteIntCard p tgs =
  let sc = buildSignChart p
  in case map fst (roots sc) of
    (a:b:_) ->
      let result = definiteIntegral p a b
          qTex   = "\\int_{" ++ showRationalLatex a ++ "}^{" ++ showRationalLatex b ++ "} f(x)\\,dx"
      in Just $ AnkiCard
          (frontQ ("Evaluate " ++ inlineMath qTex) ("f(x) = " ++ polyTex p))
          (backS (showRationalLatex result) tgs "Definite Integral")
          ("polynomial" : "definite-integral" : tgs)
    _ -> Nothing

criticalPointCard :: Polynomial -> [String] -> Maybe AnkiCard
criticalPointCard p tgs =
  let deriv   = differentiate p
      sc      = buildSignChart deriv
      critPts = map fst (roots sc)
  in if null critPts then Nothing
     else
       let entries = map (\c ->
             let cls   = classifyCritical deriv c
                 yval  = evaluate p c
                 cpTex = "x = " ++ showRationalLatex c
                      ++ ",\\quad f(" ++ showRationalLatex c ++ ") = " ++ showRationalLatex yval
             in p' (inlineMath cpTex ++ " &nbsp;\x2014;&nbsp; <b>" ++ cls ++ "</b>")
             ) critPts
       in Just $ AnkiCard
           (frontQ "Find and classify all critical points" ("f(x) = " ++ polyTex p))
           (div' bodyStyle (concat entries ++ footerDiv tgs "Critical Points"))
           ("polynomial" : "critical-points" : tgs)

classifyCritical :: Polynomial -> Rational -> String
classifyCritical deriv c =
  let eps = 1 / 1000 :: Rational
      l   = evaluate deriv (c - eps)
      r   = evaluate deriv (c + eps)
  in if l < 0 && r > 0 then "local min"
     else if l > 0 && r < 0 then "local max"
     else "saddle point"

-- ── Rational function helpers ─────────────────────────────────────────────────

horizontalAsymptote :: RationalFunc -> Maybe Rational
horizontalAsymptote (RationalFunc (Polynomial nc) (Polynomial dc))
  | null nc || null dc     = Nothing
  | length nc < length dc  = Just 0
  | length nc == length dc = Just (head nc / head dc)
  | otherwise              = Nothing

limitAtPosInfty :: RationalFunc -> String
limitAtPosInfty (RationalFunc (Polynomial nc) (Polynomial dc))
  | null nc || null dc    = "0"
  | length nc < length dc = "0"
  | length nc == length dc = showRationalLatex (head nc / head dc)
  | otherwise =
      if (head nc > 0) == (head dc > 0) then "+\\infty" else "-\\infty"

holeValue :: RationalFunc -> Rational -> Maybe Rational
holeValue (RationalFunc num den) x =
  let num' = synthDiv num x
      den' = synthDiv den x
      dv   = evaluate den' x
  in if dv /= 0 then Just (evaluate num' x / dv) else Nothing

-- ── Rational function card set ────────────────────────────────────────────────

rationalFuncCards :: RationalFunc -> [String] -> [AnkiCard]
rationalFuncCards rf@(RationalFunc num den) extraTags =
  let numRoots = nub (findRoots num)
      denRoots = nub (findRoots den)
      shared   = filter (\r -> evaluate num r == 0) denRoots
      vas      = sort $ filter (`notElem` shared) denRoots
      zeros    = sort $ filter (`notElem` shared) numRoots
      holeData = [(x, v) | x <- shared, Just v <- [holeValue rf x]]
      ha       = horizontalAsymptote rf
      rfTex    = showRatFuncLatex rf
      tgs      = "rational" : extraTags
  in catMaybes
       [ Just (ratDomainCard rfTex (sort denRoots) vas shared tgs)
       , ratVACard    rfTex vas tgs
       , ratHACard    rfTex ha tgs
       , ratZerosCard rfTex zeros tgs
       , Just (ratLimInfCard rfTex rf tgs)
       ]
     ++ concatMap (\(x, v) ->
          catMaybes [ratHoleCard rfTex x v tgs, ratLimHoleCard rfTex x v tgs]
        ) holeData

ratDomainCard :: String -> [Rational] -> [Rational] -> [Rational] -> [String] -> AnkiCard
ratDomainCard rfTex allDen vas holes tgs =
  let domTex = if null allDen
               then "\\mathbb{R}"
               else "\\mathbb{R} \\setminus \\left\\{" ++
                    intercalate ",\\;" (map showRationalLatex allDen) ++
                    "\\right\\}"
      detail =
        [p' ("Vertical asymptote" ++ (if length vas /= 1 then "s" else "") ++ ": "
            ++ intercalate ", " (map (\x -> inlineMath ("x = " ++ showRationalLatex x)) vas))
        | not (null vas)]
        ++
        [p' ("Hole" ++ (if length holes /= 1 then "s" else "") ++ ": "
            ++ intercalate ", " (map (\x -> inlineMath ("x = " ++ showRationalLatex x)) holes))
        | not (null holes)]
  in AnkiCard
      (frontQ "State the domain of \\(f(x)\\)" ("f(x) = " ++ rfTex))
      (div' bodyStyle (displayMath domTex ++ concat detail ++ footerDiv tgs "Domain"))
      ("rational" : "domain" : tgs)

ratVACard :: String -> [Rational] -> [String] -> Maybe AnkiCard
ratVACard _ [] _ = Nothing
ratVACard rfTex vas tgs =
  let vaTex = intercalate ",\\qquad " (map (\x -> "x = " ++ showRationalLatex x) vas)
  in Just $ AnkiCard
      (frontQ "Find all vertical asymptotes of \\(f(x)\\)" ("f(x) = " ++ rfTex))
      (backS vaTex tgs "Vertical Asymptotes")
      ("rational" : "vertical-asymptote" : tgs)

ratHACard :: String -> Maybe Rational -> [String] -> Maybe AnkiCard
ratHACard rfTex ha tgs =
  let (question, ansBody) = case ha of
        Just k  -> ("Find the horizontal asymptote of \\(f(x)\\)",
                    backS ("y = " ++ showRationalLatex k) tgs "Horizontal Asymptote")
        Nothing -> ("Does \\(f(x)\\) have a horizontal asymptote?",
                    div' bodyStyle (p' "No horizontal asymptote."
                      ++ footerDiv tgs "Horizontal Asymptote"))
  in Just $ AnkiCard
      (frontQ question ("f(x) = " ++ rfTex))
      ansBody
      ("rational" : "horizontal-asymptote" : tgs)

ratZerosCard :: String -> [Rational] -> [String] -> Maybe AnkiCard
ratZerosCard _ [] _ = Nothing
ratZerosCard rfTex zeros tgs =
  let zTex = intercalate ",\\qquad " (map (\x -> "x = " ++ showRationalLatex x) zeros)
  in Just $ AnkiCard
      (frontQ "Find all x-intercepts of \\(f(x)\\)" ("f(x) = " ++ rfTex))
      (backS zTex tgs "Zeros")
      ("rational" : "zeros" : tgs)

ratLimInfCard :: String -> RationalFunc -> [String] -> AnkiCard
ratLimInfCard rfTex rf tgs =
  let limTex = "\\lim_{x \\to +\\infty} f(x)"
      ansTex = limitAtPosInfty rf
  in AnkiCard
      (frontQ (inlineMath limTex ++ " = ?") ("f(x) = " ++ rfTex))
      (backS ansTex tgs "Limit at Infinity")
      ("rational" : "limit-infinity" : tgs)

ratHoleCard :: String -> Rational -> Rational -> [String] -> Maybe AnkiCard
ratHoleCard rfTex x y tgs =
  let coordTex = "\\left(" ++ showRationalLatex x ++ ",\\;" ++ showRationalLatex y ++ "\\right)"
  in Just $ AnkiCard
      (frontQ "Identify any holes in the graph of \\(f(x)\\)" ("f(x) = " ++ rfTex))
      (div' bodyStyle
        ( p' ("Hole at " ++ inlineMath coordTex)
        ++ p' ("(open circle &#8212; \\(f(" ++ showRationalLatex x ++ ")\\) is undefined)")
        ++ footerDiv tgs "Hole"))
      ("rational" : "hole" : tgs)

ratLimHoleCard :: String -> Rational -> Rational -> [String] -> Maybe AnkiCard
ratLimHoleCard rfTex x y tgs =
  let limTex = "\\lim_{x \\to " ++ showRationalLatex x ++ "} f(x)"
  in Just $ AnkiCard
      (frontQ (inlineMath limTex ++ " = ?") ("f(x) = " ++ rfTex))
      (backS (showRationalLatex y) tgs "Limit at Hole")
      ("rational" : "limit-hole" : tgs)

-- ── Utilities ─────────────────────────────────────────────────────────────────

deduplicateCards :: [AnkiCard] -> [AnkiCard]
deduplicateCards = nubBy (\a b -> front a == front b)

writeCSVFile :: FilePath -> [AnkiCard] -> IO ()
writeCSVFile path cards = writeFile path (cardsToCSV cards)
