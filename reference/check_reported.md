# Check reported statistics against each other

Takes the numbers a paper reports for an analysis and tests whether they
can all be right at once, given how precisely they were reported ("2.85"
means anything from 2.845 to 2.855). Where one input is missing but the
output is reported, it gives the range the missing input must lie in.

## Usage

``` r
check_reported(
  ...,
  test,
  variants = NULL,
  mu = 0,
  dims = NULL,
  p0 = NULL,
  effect = NULL,
  k = NULL
)
```

## Arguments

- ...:

  Reported numbers, named by quantity (see
  [`quantity_names()`](https://dm-k1.github.io/reprokit/reference/quantity_names.md)).
  Pass strings, so the decimals count: `t = "2.85"`, `p = "< .001"`,
  `df = "29"`. A quantity with one value per group or condition is a
  character vector (`m = c("5.1", "4.2")`). Integer-valued numbers are
  taken as exact.

  For `"mediation"` (a simple, regression-based mediation, as in PROCESS
  model 4), a quantity with one value per path is a character vector
  named by path, `a` (X to M), `b` (M to Y), `c` (X to Y) and `c_prime`
  (X to Y with M held constant; `c'` is accepted): `b`, `se`, `t`, `p`,
  `df`, `ci_lower`, `ci_upper` and `beta` (for example
  `ci_lower = c(a = "26.50", b = "-0.05")`). The coefficient of a path
  is `b`, so `b = c(a = "46.16", b = "-0.03")` is the path a and the
  path b. `n` is a single string. The indirect effect is `indirect`; its
  bootstrap interval (`indirect_ci_lower`, `indirect_ci_upper`) is
  accepted but cannot be checked. When a, b and `indirect` are all
  reported, `indirect` is checked for coherence with a times b: it must
  lie in the product of the rounding intervals of a and b. This is
  arithmetic on the printed numbers and says nothing about significance;
  a bootstrap mean reported instead of the sample a times b is one
  possible reason for a mismatch. Two further arguments are read from
  `...` for a mediation: `indirect_type` (`"unstandardized"`,
  `"standardized"` or `"partially_standardized"`; a standardized
  indirect effect is only checked with `beta` for a and b, a partially
  standardized one is not evaluable) and `estimator` (`"sem"`, `"ml"` or
  `"lavaan"` for numbers from an ML/SEM fit, which are not evaluable as
  a regression).

- test:

  The test the numbers come from, in the vocabulary of
  [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md):
  `"student"`, `"welch"` (two independent groups; the name fixes the
  variant), `"one_sample"`, `"paired"`, `"pearson"`, `"oneway"`,
  `"factorial"`, `"chisq"`, `"gof"`, `"mediation"` and `"lm"` (a
  multiple regression; `"lm_simple"` and `"lm_moderation"` for a simple
  regression and a moderation analysis). It can be given by name or as
  the first unnamed argument. Any other value is refused with the list
  of valid ones.

- variants:

  Character vector restricting which variants to try (such as
  `"two.sided"`, `"0.95"` or `"welch"`), or `NULL` for all. Each value
  restricts only its own kind of variant (tail, confidence level, test,
  convention): `c("welch", "0.95")` fixes the test and the level. A kind
  that no value names keeps all its variants. The accepted values are
  the codes below; the printed output shows a readable label instead. A
  value the handler does not have is an error that lists the ones it
  has.

  - Tail: `"two.sided"`, `"less"`, `"greater"` (shown as two-sided,
    one-sided (less), one-sided (greater)).

  - Level: `"0.90"`, `"0.95"`, `"0.99"` (shown as 90%, 95%, 99%).

  - Test: `"student"`, `"welch"` (shown as Student's t, Welch's t).

  - SS type (factorial ANOVA, unbalanced designs): `"III"`, `"II"`,
    `"I"` (shown as type III, type II, type I).

  - Effect-size convention: `"pooled"` (Cohen's d, pooled SD),
    `"hedges_g"`, `"average"` (Cohen's d, root mean square of the SDs),
    `"glass_sd1"`, `"glass_sd2"` (Glass's delta), `"cohen"`, `"d_z"`,
    `"d_rm"`, `"d_av"`, `"g_av"`, `"eta2"` (eta squared), `"omega2"`
    (omega squared), `"partial_eta2"`, `"partial_omega2"` (factorial
    ANOVA). Which of them a handler has depends on the test.

- mu:

  The test value the mean (or mean difference) is compared with, for
  tests whose t depends on it (`"one_sample"`, `"paired"`). A plain
  number, not a string (`mu = 20`, not `mu = "20"`), unlike the reported
  numbers. Default 0. A test without a test value errors on `mu` other
  than 0.

- dims:

  For `"chisq"`, `"gof"` and `"factorial"`, the table shape:
  `c(rows, columns)` for a test of independence, the number of
  categories for goodness of fit, the levels of each factor for a
  factorial design. Not needed when `n` is a matrix (its
  [`dim()`](https://rdrr.io/r/base/dim.html) is read before the counts
  are parsed). Without a shape `df` cannot be checked, because a flat
  vector of counts does not say whether 12 cells are 2 x 6 or 3 x 4.

- p0:

  For `"gof"`, the H0 proportions, one per category, summing to 1
  (within 1e-8). Without them the categories are taken as equally
  likely.

- effect:

  For `"factorial"`, the effect to check: `"A"` (the factor in the rows
  of the cell matrices), `"B"` (the columns) or `"AB"` (the
  interaction). A paper with three F tests gets three calls with the
  same cell descriptives.

- k:

  For the regression tests (`"lm"` and the like), the number of
  predictor columns (the model's df1), when F is not reported to say it.
  With `k`, `n` and `df2` are checked against each other (df2 = n -
  k - 1) and either gives the other; without it they are not, because k
  is never guessed from the number of coefficients reported (a paper may
  leave some out). A plain number.

## Value

A `reprokit_check` object: `$numbers` (status of each reported number),
`$level` (coverage, 0 to 3), `$missing` (what would raise the level, and
the range implied for it), `$variants` (a list with one data frame per
kind of variant the handler has, each of the variants tried and whether
they fit: `tail`, `level`, `test` (Student or Welch) and `convention`
(how an effect size was computed)), `$relations` (one row per relation
and variant; `$variant` and `$kind` join the values and kinds of a
relation that varies over several with "+", and there is one column per
kind, `tail`, `level`, `test`, `convention`, holding the row's value or
`NA`), `$implied`, `$implied_inputs` (numbers derived from reported
ones, such as `n` from `df`) and `$checked` (whether any relation that
can falsify was compared with a reported number; without one there is no
verdict).

The reported numbers come from one analysis, so they share one test and
one confidence level: a variant fits only if one choice per kind
explains every relation that can falsify. A CI whose bounds fit
different levels, or a t that fits only Student next to a CI that fits
only Welch, is inconsistent even though each relation holds on its own.

Some relations cannot falsify: how `d` was computed (pooled SD, average
SD, Glass) is rarely declared, so a `d` that no convention reproduces
gets the status `"not reproducible"`, never `"inconsistent"`, and
`$consistent` is not affected.

A reported number is only ever compared within what it can be: `r` lies
in -1 to 1, `sd` and `df` are not negative, `p` is in 0 to 1, `n` is a
whole number. A relation whose result is not finite for the reported
values (`"1.00"` for `r` gives an infinite t) is skipped, as if an input
were missing; it never counts as a failure.

When `n` is not reported but `df` is, `n` is derived from `df` for the
handlers where that is exact (a correlation, a paired or one-sample t):
it is used by the other relations, shown as `$implied_inputs`, and
counts for the coverage level, but `df` is never checked against it.

## Examples

``` r
check_reported("paired", n = "20", t = "2.85", df = "19", p = ".010")
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Variants that fit: two-sided
#> 
#>  quantity reported   status     checked_against
#>         n       20 coherent           df_from_n
#>         t     2.85 coherent            p_from_t
#>        df       19 verified df_from_n; p_from_t
#>         p     .010 coherent            p_from_t
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.

# A 2 x 2 table: Yates' correction is a variant, V is a convention
check_reported("chisq", n = matrix(c("30", "10", "20", "40"), 2), chisq = "15.04",
               df = "1", p = ".0001", v = ".41")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Tests that fit: Yates' continuity correction
#> Effect-size conventions that fit: none
#> 
#>  quantity       reported   status                              checked_against
#>         n 30, 10, 20, 40 coherent chisq_from_table; p_from_table; v_from_table
#>     chisq          15.04 verified chisq_from_table; df_from_dims; p_from_chisq
#>        df              1 coherent                   df_from_dims; p_from_chisq
#>         p          .0001 verified                   p_from_table; p_from_chisq
#>         v            .41 verified                                 v_from_table
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.

# A regression reports a number per coefficient: name them.
# A simple mediation (mpg ~ wt through hp): the paths, and the indirect effect
# checked for coherence with a times b
check_reported("mediation",
               b = c(a = "46.16", b = "-0.03", c = "-5.34", c_prime = "-3.88"),
               se = c(a = "9.63", b = "0.01", c = "0.56", c_prime = "0.63"),
               t = c(a = "4.80", b = "-3.52", c = "-9.56", c_prime = "-6.13"),
               n = "32", indirect = "-1.46")
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> 
#>    quantity reported        status                 checked_against
#>         b:a    46.16      coherent t_from_b_se:a; indirect_from_ab
#>         b:b    -0.03      coherent t_from_b_se:b; indirect_from_ab
#>         b:c    -5.34      coherent                   t_from_b_se:c
#>   b:c_prime    -3.88      coherent             t_from_b_se:c_prime
#>        se:a     9.63      coherent                   t_from_b_se:a
#>        se:b     0.01      coherent                   t_from_b_se:b
#>        se:c     0.56      coherent                   t_from_b_se:c
#>  se:c_prime     0.63      coherent             t_from_b_se:c_prime
#>         t:a     4.80      coherent                   t_from_b_se:a
#>         t:b    -3.52      coherent                   t_from_b_se:b
#>         t:c    -9.56      coherent                   t_from_b_se:c
#>   t:c_prime    -6.13      coherent             t_from_b_se:c_prime
#>           n       32 not checkable                               -
#>    indirect    -1.46      coherent                indirect_from_ab
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.

# A regression reports a number per coefficient: name them.
check_reported("lm", b = c(wt = "-3.88", hp = "-0.03"), se = c(wt = "0.63", hp = "0.01"),
               t = c(wt = "-6.13", hp = "-3.52"), p = c(wt = "< .001", hp = ".001"),
               r2 = ".83", f = "69.21", df1 = "2", df2 = "29", n = "32")
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Effect-size conventions that fit: R²
#> 
#>  quantity reported   status                                 checked_against
#>      b:wt    -3.88 coherent                                  t_from_b_se:wt
#>      b:hp    -0.03 coherent                                  t_from_b_se:hp
#>     se:wt     0.63 coherent                                  t_from_b_se:wt
#>     se:hp     0.01 coherent                                  t_from_b_se:hp
#>      t:wt    -6.13 coherent                     t_from_b_se:wt; p_from_t:wt
#>      t:hp    -3.52 coherent                     t_from_b_se:hp; p_from_t:hp
#>      p:wt   < .001 coherent                                     p_from_t:wt
#>      p:hp     .001 coherent                                     p_from_t:hp
#>        r2      .83 coherent                                       f_from_r2
#>         f    69.21 coherent                                       f_from_r2
#>       df1        2 coherent                                       f_from_r2
#>       df2       29 verified p_from_t:wt; p_from_t:hp; f_from_r2; df2_from_n
#>         n       32 coherent                                      df2_from_n
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```
