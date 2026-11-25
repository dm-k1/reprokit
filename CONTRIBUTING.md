# Contributing

## Adding a model = adding a handler

1.  Open a “New handler” issue: analysis, verdict, sufficient
    statistics, catch, reference implementation, what is lost.
2.  Add `R/handler-<name>.R` that calls `register_handler()` from
    `.onLoad()`.
3.  A `summaries_suffice` handler must supply `summarise`, `reproduce`,
    `reported` and an `example` model; the round-trip test in
    `test-infra-registry.R` then covers it automatically.
4.  Add refusal tests for what the handler cannot do
    (`class = "reprokit_impossible"`; a wrong call is
    `reprokit_error_input`).
5.  Document the evidence in `vignettes/validation.Rmd`.

## Rules

- **No new Imports.** The app runs in the browser via webR, which keeps
  the dependencies small. Heavy packages go in Suggests and are used
  with [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).
- **Never round stored statistics.** Rounding is a display concern.
- **The verdict path never errors.**
  [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
  returns a verdict for an unsupported model; only invalid input (and
  [`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
  without summaries) is an `abort_impossible()` error.
- **Complete cases only**; never impute.
- **Wording**: “consistent with” and “values that cannot co-occur”,
  never “fraud” or “error”. Disclosure checks reduce risk; they never
  “guarantee” it.

## Before a PR

``` r

devtools::document()
devtools::test()
lintr::lint_package()
devtools::check()
```

Add a line to `NEWS.md`.

Before a release, run the scripts in `inst/validation/`.
`check-false-positives.R` is the release gate;
[`vignette("validation")`](https://dm-k1.github.io/reprokit/articles/validation.md)
explains them.

By taking part you agree to follow the [code of
conduct](https://dm-k1.github.io/reprokit/CODE_OF_CONDUCT.md).

Before the first CRAN/rOpenSci submission, run the rOpenSci
statistical-software tools: `srr` (standards), `pkgcheck`, `autotest`.
