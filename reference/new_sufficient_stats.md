# Store the sufficient statistics of one analysis

The central object of the share direction. Users get one from
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md);
`new_sufficient_stats()` is the low-level constructor. The check
functions do not use it: they work from reported numbers.

## Usage

``` r
new_sufficient_stats(
  handler,
  verdict,
  verdict_reason,
  stats = list(),
  meta = list(),
  disclosure = list(),
  lost
)

is_sufficient_stats(x)
```

## Arguments

- handler:

  Name of the registered handler that made the object.

- verdict:

  One of
  [`verdicts()`](https://dm-k1.github.io/reprokit/reference/verdicts.md).

- verdict_reason:

  Why this verdict, in one sentence.

- stats:

  Named list of sufficient statistics, stored at full precision (means,
  covariance matrix, counts, per-cluster summaries, ...).

- meta:

  Named list of metadata: complete-case `n`, `contrasts`,
  `effect_size_conventions`, `rounding`, `ss_type`.

- disclosure:

  Named list of disclosure flags (small cells, small clusters, extreme
  values). A risk reduction, never a guarantee.

- lost:

  Required: character vector of what readers lose compared with the raw
  data.

- x:

  An object.

## Value

An object of class `reprokit_sufficient_stats`.

## Details

Authors never call this constructor:
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
builds the object and fills `lost` from the test.

This constructor cannot make an object without a non-empty list of what
readers lose. The author's reason for not sharing is not stored here: it
belongs to
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
alone, is optional, and is never filled in by reprokit.

## Examples

``` r
s <- new_sufficient_stats(
  handler = "example",
  verdict = "summaries_suffice",
  verdict_reason = "Group means and SDs determine the t statistic.",
  stats = list(n = c(20, 22), mean = c(5.1, 4.3), sd = c(1.2, 1.4)),
  lost = c("outlier inspection", "alternative analyses")
)
s
#> <reprokit sufficient_stats>  analysis: not recognised
#> Verdict: summaries_suffice
#> Because: Group means and SDs determine the t statistic.
#> To publish: NA (appendix() writes this out).
#> What readers lose compared with the raw data:
#>   * outlier inspection
#>   * alternative analyses
#> Statistics stored: n, mean, sd
```
