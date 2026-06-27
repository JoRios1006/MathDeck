# MathDeck

Generate unlimited calculus and algebra Anki flashcards from mathematically consistent functions.

## Overview

MathDeck is a Haskell CLI tool that generates fresh math exercises every time it runs. Instead of random coefficients, it builds functions from known properties (roots, intervals, etc.) so every generated exercise includes its complete solution.

## How to Run

The "Start application" workflow builds and runs MathDeck with default settings (10 mixed cards).

To run manually from the Shell:

```bash
# Build
ghc -isrc src/Main.hs -o mathdeck -w

# Run with options: [difficulty] [count]
./mathdeck easy 5
./mathdeck medium 20
./mathdeck hard 10
./mathdeck mixed 15
./mathdeck          # defaults to mixed, 10 cards
```

Output is written to `mathdeck_output.csv`, ready for import into Anki.

## Project Structure

```
src/
  Main.hs        — Entry point, CLI argument parsing
  Types.hs       — Data type definitions
  Polynomial.hs  — Polynomial arithmetic (add, multiply, evaluate, fromRoots)
  Generator.hs   — Random function generation (LCG-based RNG, no external deps)
  Calculus.hs    — Differentiation and integration
  Limits.hs      — Limit calculations and end behavior
  SignChart.hs   — Root finding and sign interval analysis
  PrettyPrint.hs — Human-readable formatting with Unicode superscripts
  CSV.hs         — Anki CSV export
```

## Difficulty Levels

- **easy** — Quadratic functions
- **medium** — Cubic and quartic functions
- **hard** — Rational functions
- **mixed** — Random selection from all types

## User Preferences

- Language: Haskell (GHC 9.10.3)
- No external package dependencies (uses only GHC base + time)
- Build with: `ghc -isrc src/Main.hs -o mathdeck -w`
