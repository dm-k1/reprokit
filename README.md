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
| Share | authors who cannot share raw data | `triage()`, `kit()`, `reproduce()`, `appendix()` |
| Check | readers and reviewers | `check_*()` consistency checks |

Only `triage()`, `new_kit()`, `handlers()`, `coverage_table()` and `verdicts()`
exist so far; the rest is planned (see `docs/project-plan.md` in the workspace).

## What it covers

`coverage_table()` generates this table from the handler registry, so it cannot
disagree with the code. No handlers are registered yet.

## Installation

```r
# install.packages("pak")
pak::pak("dm-k1/reprokit")
```

## Prior art

statcheck, GRIM/GRIMMER/SPRITE (rsprite2), synthpop, DataSHIELD, and the
summary-statistics input of lavaan and metafor. reprokit builds on these rather
than replacing them.
