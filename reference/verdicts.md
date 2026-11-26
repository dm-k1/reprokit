# The three sufficient_stats verdicts

The verdicts are drawn around what an APA-style report already contains:

## Usage

``` r
verdicts()
```

## Value

A character vector of verdict codes.

## Details

- `apa_suffices` – the numbers APA style asks a paper to report make the
  analysis exactly reproducible (any further number the summaries need
  follows from them by an exact identity at printed precision)

- `apa_plus_suffices` – exact reproduction needs a number beyond the APA
  set, such as the correlation between the two conditions of a paired
  design

- `needs_raw_data` – exact reproduction, diagnostics, reanalysis or
  fraud detection need the raw data

Sharing the raw data remains the standard (APA Publication Manual
section 1.14, Ethics Code 8.14). For an analysis that has no finite
summaries (a logistic or multilevel model), the advice mentions
synthetic data as an option outside reprokit.

## Examples

``` r
verdicts()
#> [1] "apa_suffices"      "apa_plus_suffices" "needs_raw_data"   
```
