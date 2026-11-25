# The three sufficient_stats verdicts

- `summaries_suffice` – exact reproduction from summary statistics

- `synthetic_possible` – approximate reproduction via synthetic data

- `needs_raw_data` – exact reproduction, diagnostics, reanalysis or
  fraud detection need the raw data

## Usage

``` r
verdicts()
```

## Value

A character vector of verdict codes.

## Examples

``` r
verdicts()
#> [1] "summaries_suffice"  "synthetic_possible" "needs_raw_data"    
```
