# Contributing

## Adding a model = adding a handler

1. Open a "New handler" issue: analysis, verdict, sufficient statistics, catch,
   reference implementation, what is lost.
2. Add `R/handler-<name>.R` that calls `register_handler()` from `.onLoad()`.
3. A `summaries_suffice` handler must supply `kit`, `reproduce` and an `example`
   model; the round-trip test in `test-registry.R` then covers it automatically.
4. Add refusal tests for what the handler cannot do (`class = "reprokit_impossible"`).
5. Document the evidence in `vignettes/validation.Rmd`.

## Rules

- **No new Imports.** The app runs in the browser via webR (ADR 0002). Heavy
  packages go in Suggests and are used with `requireNamespace()`.
- **Never round stored statistics.** Rounding is a display concern.
- **`triage()` never errors.** An unsupported model is a verdict.
- **Complete cases only**; never impute.
- **Wording**: "consistent with" and "values that cannot co-occur", never
  "fraud" or "error". Disclosure checks reduce risk; they never "guarantee" it.

## Before a PR

```r
devtools::document()
devtools::test()
lintr::lint_package()
devtools::check()
```

Add a line to `NEWS.md`. Before the first CRAN/rOpenSci submission, run the
rOpenSci statistical-software tools: `srr` (standards), `pkgcheck`, `autotest`.
