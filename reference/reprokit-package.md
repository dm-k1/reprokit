# reprokit: reproduce and check results when raw data cannot be shared

Sharing raw data remains the standard. When sharing is genuinely
impossible, reprokit says what the minimum is that authors owe their
readers, and what those readers lose. It also lets readers check whether
the numbers a paper reports are consistent with each other.

## Details

Two directions use the same statistics.

**Share** (you cannot share the raw data):

- [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
  says whether summary statistics suffice and what to publish, and
  stores them. It never errors on the verdict, and its print narrates
  the verdict, what to publish and what readers lose. It takes a fitted
  model or result, a formula, or, for a mediation, two `lm` fits.

- [`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
  writes the appendix for the paper from the same input. reprokit states
  what readers lose, from the test; you do not fill that in. A
  `sharing_reason`, in your own words, is optional: without one
  [`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
  gives a warning and the appendix says that no reason was stated.

**Check** (you are reading a paper):

- [`check_reported()`](https://dm-k1.github.io/reprokit/reference/check_reported.md)
  tests whether reported numbers are consistent with each other,
  allowing for rounding, for the `test` you name.

An inconsistency means the numbers cannot all be correct as reported. It
does not say which number is wrong or why.

Reference:
[`coverage_table()`](https://dm-k1.github.io/reprokit/reference/coverage_table.md)
lists which tests are covered and what each needs, and
[`conventions()`](https://dm-k1.github.io/reprokit/reference/conventions.md)
the effect-size conventions and sums-of-squares types you can declare.
See also
[`handlers()`](https://dm-k1.github.io/reprokit/reference/handlers.md),
[`quantity_names()`](https://dm-k1.github.io/reprokit/reference/quantity_names.md)
and
[`verdicts()`](https://dm-k1.github.io/reprokit/reference/verdicts.md).

## Vignettes

- [`vignette("reprokit")`](https://dm-k1.github.io/reprokit/articles/reprokit.md):
  getting started, one example for each direction.

- [`vignette("concepts")`](https://dm-k1.github.io/reprokit/articles/concepts.md):
  what the terms mean.

- [`vignette("validation")`](https://dm-k1.github.io/reprokit/articles/validation.md):
  the evidence behind the claims.

## See also

Useful links:

- <https://dm-k1.github.io/reprokit/>

- <https://github.com/dm-k1/reprokit>

- Report bugs at <https://github.com/dm-k1/reprokit/issues>

## Author

**Maintainer**: D.M. Kloppenburg
<192165061+dm-k1@users.noreply.github.com>

Authors:

- D.M. Kloppenburg <192165061+dm-k1@users.noreply.github.com>
