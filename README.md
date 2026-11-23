# reprokit

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dm-k1/reprokit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dm-k1/reprokit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Sharing raw data remains the standard. When sharing is genuinely impossible,
> reprokit says what the minimum is that authors owe their readers, and what
> those readers lose.

*Working name. See the workspace README for how to rename.*

## Two directions, one set of statistics

| Direction | For | Functions |
|---|---|---|
| Share | authors who cannot share raw data | `triage()`, `sufficient_stats()`, `reproduce()`, `appendix()` |
| Check | readers and reviewers | `check_*()` consistency checks |

So far `triage()`, `sufficient_stats()`, `reproduce()`, `appendix()`, `check_reported()`,
`check_t()`, `check_r()`, `handlers()`, `coverage_table()` and `verdicts()` exist, for one-sample,
independent and paired t and Pearson r; the rest is planned (see `docs/project-plan.md` in the
workspace).

## What it covers

`coverage_table()` generates the coverage table from the handler registry, so it cannot
disagree with the code: per test, what APA reports, what suffices, what is missing from
APA's list, which reported numbers can be checked, and which choices to declare. Run
`coverage_table()`, or see the Validation vignette (`vignette("validation")`) for the
rendered table.

## Installation

```r
# install.packages("pak")
pak::pak("dm-k1/reprokit")
```

## Prior art

statcheck, GRIM/GRIMMER/SPRITE (rsprite2), synthpop, DataSHIELD, and the
summary-statistics input of lavaan and metafor. reprokit builds on these rather
than replacing them.
