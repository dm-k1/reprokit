# What can be shared for this analysis?

`sufficient_stats()` is the author's verb. Given the analysis, it says
whether summary statistics can make it reproducible without the raw
data, what to publish, and what readers lose. When they do, it holds the
summaries, stored at full precision. The verdict never errors:
"summaries do not suffice, because ..." is an answer, and the printed
object narrates it (the verdict and why, what to publish, what is lost,
and which group or cell a disclosure flag fired for). Only invalid input
is an error, of class `reprokit_error_input`: a formula without `data`
or `test`, an unknown `test`, a `t.test` or `cor.test` result without
`data`, or an explicit choice that conflicts with the result.

## Usage

``` r
sufficient_stats(x, ..., threshold = 5)
```

## Arguments

- x:

  A fitted model, a test result, or a formula; see above.

- ...:

  Passed on: the second `lm` fit of a mediation, `data` for a formula or
  a `t.test` / `cor.test` result, `test` for a formula, and the choices
  of the analysis for a formula (`alternative`, `conf_level`, `mu`,
  `correct`, `p0`, `ss_type`).

- threshold:

  Smallest group or cell size whose summaries are clearly less
  identifying than the raw data. Default 5. The total sample size must
  also be at least `2 * threshold`.

## Value

An object of class `reprokit_sufficient_stats` with the verdict
(`verdict`, `verdict_reason`), what to publish (`publish`), the
handler's default `lost`, the `disclosure` check, and `stats` and `meta`
(`NULL` and mostly empty when the summaries do not suffice).

## Details

`x` is one of:

- a fitted `lm`, a `glm`, an `aov` (one factor, or two factors with
  their interaction; other designs get a verdict), or the result of
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html). These hold
  what is needed; the choices recorded in a result (continuity
  correction, H0 proportions) are read from it. An `aov` fit does not
  record the type of sums of squares, so
  [`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
  asks for it (`choices = list(ss_type = ...)`).

- the result of [`t.test()`](https://rdrr.io/r/stats/t.test.html) or
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) together with
  `data`. These do not hold the data, but they record Student or Welch,
  paired, one-sample, the tails, the confidence level and the test
  value. reprokit rebuilds the test from `data` and compares it with the
  result (statistic, df and p, scaled difference below 1e-8). A result
  that does not follow from `data` (wrong data, a subset of rows) gets a
  verdict saying so. Spearman and Kendall are not supported tests.

- a formula with `data` and `test`, for the analysis the result objects
  above do not cover, or to be explicit:
  `y ~ group, data = d, test = "welch"`. `test` is one of `"student"`,
  `"welch"`, `"one_sample"`, `"paired"`, `"pearson"`, `"oneway"`,
  `"factorial"`, `"chisq"`, `"gof"` or `"mediation"`.

- two `lm` fits for a simple mediation: the mediator model (`M ~ X`) and
  the outcome model (`Y ~ X + M`), in either order, as
  `sufficient_stats(lm(M ~ X, d), lm(Y ~ X + M, d))`. The second fit
  goes in `...`. reprokit works out which is which; the two must be
  fitted to the same cases. Covariates or more mediators get a verdict,
  not an error.

A result and the formula for the same analysis give the same summaries;
the field `meta$route` records which one was used.

## Examples

``` r
sufficient_stats(extra ~ group, data = sleep, test = "welch")
#> <reprokit sufficient_stats>  analysis: independent-samples t-test  (input: formula)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: per-group n, mean and SD determine the reported results exactly.
#> To publish: per-group n, mean and SD (appendix() writes this out).
#> Disclosure check: passed (group sizes, threshold 5).
#> What readers lose compared with the raw data:
#>   * checking the distribution and outliers within each group
#>   * nonparametric or robust alternatives
#> Statistics stored: n, mean, sd

# From the result you already have:
sufficient_stats(t.test(extra ~ group, data = sleep), data = sleep)
#> <reprokit sufficient_stats>  analysis: independent-samples t-test  (input: t.test)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: per-group n, mean and SD determine the reported results exactly.
#> To publish: per-group n, mean and SD (appendix() writes this out).
#> Disclosure check: passed (group sizes, threshold 5).
#> Choices read from the t.test result: test = Welch.
#> What readers lose compared with the raw data:
#>   * checking the distribution and outliers within each group
#>   * nonparametric or robust alternatives
#> Statistics stored: n, mean, sd
sufficient_stats(chisq.test(table(mtcars$am, mtcars$vs)))
#> <reprokit sufficient_stats>  analysis: chi-squared test of independence  (input: chisq.test)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: the contingency table of counts determine the reported results exactly.
#> To publish: the contingency table of counts (appendix() writes this out).
#> Disclosure check: passed (cell counts (a count above 0 and below 5 is flagged; zeros are not), threshold 5).
#> Choices read from the chisq.test result: test = Yates.
#> What readers lose compared with the raw data:
#>   * checking that the expected counts are large enough for the chi-squared approximation (expected counts below 5; reprokit does not test this)
#>   * exact, Fisher or Monte Carlo alternatives for sparse tables
#>   * any variable other than the ones in the table
#> Statistics stored: n

# Mediation from two models, in either order:
sufficient_stats(lm(hp ~ wt, data = mtcars), lm(mpg ~ wt + hp, data = mtcars))
#> <reprokit sufficient_stats>  analysis: simple mediation  (input: lm)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: n, means and covariance matrix of X, M and Y determine the reported results exactly.
#> To publish: n, means and covariance matrix of X, M and Y (appendix() writes this out).
#> Disclosure check: passed (sample size only (no groups or cells; no individual values stored), threshold 5).
#> What readers lose compared with the raw data:
#>   * the bootstrap (or Monte Carlo) confidence interval of the indirect effect a*b
#>   * residual diagnostics (normality, outliers, influence)
#>   * refitting with covariates, other mediators or transformations
#> Statistics stored: n, mean, cov

# A verdict, not an error, when summaries cannot do it:
sufficient_stats(glm(am ~ wt, data = mtcars, family = binomial))
#> <reprokit sufficient_stats>  analysis: not recognised
#> Verdict: needs_raw_data
#> Because: reprokit does not cover glm/lm objects, so it cannot show that summary statistics are enough.
#> Nothing is stored: summary statistics do not make this analysis reproducible.
#> What readers lose without the raw data:
#>   * exact reproduction of this analysis
#> Instead you could:
#>   * share the raw data (possibly under a data-use agreement)
#>   * share synthetic data and report its utility
#>   * offer federated analysis (e.g. DataSHIELD)
```
