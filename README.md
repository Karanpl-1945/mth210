# MTH 210: Statistical Computing

This repository contains R and Quarto solutions for MTH 210 laboratory exercises. The labs cover random-variable generation, Monte Carlo methods, optimization, regression, and maximum-likelihood estimation.

## Repository contents

| Lab | Topics | Files |
| --- | --- | --- |
| 2 | Inverse-transform sampling for Bernoulli, Poisson, Binomial, and a geometric-type random variable; comparison of sample and population moments | [Source](Lab%202/Lab%202%20Solution.qmd) · [PDF](Lab%202/Lab%202%20Solution.pdf) |
| 3 | Acceptance-rejection sampling of Binomial distributions with a Geometric proposal; theoretical and empirical acceptance constants | [Source](Lab%203/Lab%203%20Solution.qmd) · [PDF](Lab%203/Lab-3-Solution.pdf) |
| 4 | Acceptance-rejection generation of a Gamma(4, 5) distribution using an Exponential proposal; uniform simulation of points inside the unit circle | [Source](Lab%204/Lab%204%20Solution.qmd) · [PDF](Lab%204/Lab-4-Solution.pdf) |
| 5 | Ratio-of-uniforms sampling for the standard Normal distribution and Monte Carlo integration | [Source](Lab%205/Lab%205%20Solution%20.qmd) · PDF not included |
| 6 | Importance sampling for the mean of a standard Normal distribution and the variance of a Gamma(2, 4) distribution | [Source](Lab%206/Lab%206%20Solution.qmd) · [PDF](Lab%206/Lab-6-Solution.pdf) |
| 7 | Numerical checks of convexity for a parameterized quadratic function | [Source](Lab%207/Lab%207%20Solution.qmd) · [PDF](Lab%207/Lab-7-Solution.pdf) |
| 8 | Bound-constrained and equality-constrained optimization with `nloptr` | [Source](Lab%208/Lab%208%20Solution.qmd) · [PDF](Lab%208/Lab-8-Solution.pdf) |
| 9 | Nonlinear constrained optimization with inequality and equality constraints, including a four-variable quadratic problem | [Source](Lab%209/Lab-9-Solution%20.qmd) · [PDF](Lab%209/Lab-9-Solution-.pdf) |
| 10 | Least-squares estimates for multiple linear regression using the matrix formula and `lm()` | [Source](Lab%2010/Lab%2010%20Solution.qmd) · [PDF](Lab%2010/Lab-10-Solution.pdf) |
| 11 | Newton-Raphson estimation of Normal mean and variance, compared with closed-form maximum-likelihood estimates | [Source](Lab%2011/Lab%2011%20Solution.qmd) · [PDF](Lab%2011/Lab-11-Solution.pdf) |
| 12 | Gradient-ascent estimation for a Normal model and logistic regression; comparison with analytical estimates and `glm()` | [Source](Lab%2012/Lab%2012%20Solution.qmd) · [PDF](Lab%2012/Lab-12-Solution.pdf) |

Labs 1 and 13 onward are not included. Lab 5 contains Quarto source only; all other listed labs include both source and a rendered PDF.

## Requirements

- R
- Quarto or RStudio with Quarto support
- The R package `nloptr` for Labs 8 and 9

Install the non-base package from R with:

```r
install.packages("nloptr")
```

## Using the labs

Open a PDF to read a rendered solution. To run or modify a solution, open its `.qmd` file in RStudio, or render it from the repository root with Quarto:

```sh
quarto render "Lab 2/Lab 2 Solution.qmd"
```

Change the path to render a different lab. The exercises use randomized simulations, so numerical results may vary between runs unless a random seed is set.
