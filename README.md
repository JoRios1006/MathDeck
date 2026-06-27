# MathDeck

> Generate an unlimited number of calculus and algebra Anki cards from mathematically consistent functions.

## Motivation

When studying mathematics, the biggest limitation is often the number of good exercises available.

Textbooks contain a finite set of problems. After solving them once or twice, patterns become familiar and practice loses effectiveness.

MathDeck solves this by generating fresh exercises every time it runs.

Instead of memorizing answers, you repeatedly practice the underlying mathematical procedures.

Every generated exercise includes its complete solution, making the resulting CSV immediately importable into Anki.

---

# Philosophy

The project is built around a simple idea:

> Generate mathematical objects, not questions.

Instead of randomly inventing coefficients and hoping the resulting function is interesting, MathDeck generates functions from known mathematical properties.

Example:

Choose the roots

```
-3
2
5
```

Construct

```
(x+3)(x-2)(x-5)
```

Expand

```
x³-4x²-11x+30
```

Now the program already knows

* every root
* sign intervals
* derivative
* integral
* end behavior

Everything else is derived automatically.

---

# Features

## Polynomial generation

Generate random

* Quadratic functions
* Cubic functions
* Quartic functions

using integer roots.

Example

```
f(x)=x³-4x²-11x+30
```

---

## Rational function generation

Generate rational expressions such as

```
(x+2)(x-5)
-----------
(x-3)
```

including

* domain
* poles
* roots
* sign chart
* limits

---

## Automatic derivatives

Generate and solve

```
d/dx f(x)
```

for every polynomial.

---

## Automatic integrals

Generate

```
∫f(x)dx
```

including the constant of integration.

Optionally generate

```
∫ab f(x)dx
```

for random intervals.

---

## Limits

Supported categories include

* direct substitution
* removable discontinuities
* factorization
* common factors
* difference of squares

Future versions may include

* L'Hôpital's Rule
* infinite limits
* limits at infinity
* exponential and logarithmic limits

---

## Sign analysis

Automatically determine

* roots
* positive intervals
* negative intervals

Example

```
Roots

-2
1
5

Positive

(-2,1)
(5,∞)

Negative

(-∞,-2)
(1,5)
```

---

## End behavior

Determine

```
lim x→∞

lim x→−∞
```

for every polynomial.

---

## Full function analysis

One generated function can produce a complete review exercise.

Example front

```
f(x)=x³-4x²-11x+30
```

Example back

```
Roots

-3,2,5

Positive intervals

(-3,2)
(5,∞)

Negative intervals

(-∞,-3)
(2,5)

Derivative

3x²-8x-11

Integral

¼x⁴-(4/3)x³-(11/2)x²+30x+C

End behavior

x→∞

∞

x→−∞

−∞
```

---

# Anki integration

MathDeck exports directly to CSV.

```
Front,Back,Tags
```

Example

```
"f(x)=x²-7x+12",
"Roots:
3,4

Derivative:
2x-7

Integral:
x³/3-7x²/2+12x+C",
"quadratic derivative integral"
```

Import directly into Anki.

---

# Difficulty levels

## Easy

* Quadratics
* Basic derivatives
* Basic integrals

---

## Medium

* Cubics
* Quartics
* Sign charts
* Definite integrals

---

## Hard

* Rational functions
* Limits
* Mixed review cards

---

## Mixed

Random selection from every supported topic.

---

# Project structure

```
MathDeck/

Main.hs

Types.hs

Polynomial.hs

Generator.hs

Calculus.hs

Limits.hs

SignChart.hs

PrettyPrint.hs

CSV.hs
```

---

# Roadmap

## Version 1

* Quadratic generator
* CSV export

## Version 2

* Generic polynomial type
* Cubics
* Quartics
* Derivatives
* Integrals
* Sign charts

## Version 3

* Rational functions
* Limits
* Definite integrals

## Version 4

* Graph generation
* SVG export
* PNG export
* Automatic graph cards

---

# Future ideas

* Taylor polynomials
* Optimization problems
* Newton's Method
* Differential equations
* Complex roots
* Matrix exercises
* Linear algebra decks
* Probability decks
* Statistics decks

---

# Why Haskell?

The project is intentionally written in Haskell because mathematics maps naturally onto immutable data and pure functions.

Examples

```
differentiate :: Polynomial -> Polynomial

integrate :: Polynomial -> Polynomial

evaluate :: Polynomial -> Rational -> Rational

roots :: Polynomial -> [Rational]

signChart :: Polynomial -> SignChart
```

Most of the project consists of composing these transformations together.

---

# Long-term vision

The long-term goal is for MathDeck to become a reusable mathematics practice engine capable of generating thousands of mathematically correct exercises covering algebra, calculus, linear algebra, probability and beyond.

Instead of asking:

> "Do I have enough exercises?"

the question becomes:

> "What would I like to practice today?"
