# List the reporting conventions each handler accepts

Some results depend on a choice the statistics cannot show: which effect
size was computed (`choices$convention` in
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md))
and, for an unbalanced factorial ANOVA, the type of sums of squares
(`choices$ss_type`, also `ss_type` in
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)).
`conventions()` lists the codes reprokit accepts for them. It reads the
handler registry, the same place
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
validates against, so the list cannot drift from the package.

## Usage

``` r
conventions(handler = NULL)
```

## Arguments

- handler:

  A handler name from
  [`handlers()`](https://dm-k1.github.io/reprokit/reference/handlers.md),
  or `NULL` for all handlers that accept a convention choice.

## Value

A data frame with one row per accepted code and columns `handler`,
`choice` (`"convention"` or `"ss_type"`, the name used in `choices`),
`code` (the exact string reprokit accepts), `default` (logical) and
`label` (the short label used in the appendix).

## Details

`default` is `TRUE` for the code reprokit uses when the argument is left
out. The sums of squares type has one on the share side: type III,
because a fitted `aov` does not record it, shown in the appendix as
reprokit's default, not the author's answer. Checking a paper never
assumes a type. An effect-size convention with several codes has no
default: leaving it out means "not declared". A handler with a single
code (V, w) uses it.

## Examples

``` r
conventions("t_paired")
#>    handler     choice   code default                                 label
#> 1 t_paired convention    d_z   FALSE           d_z (SD of the differences)
#> 2 t_paired convention   d_rm   FALSE              d_rm (repeated measures)
#> 3 t_paired convention   d_av   FALSE                     d_av (average SD)
#> 4 t_paired convention   g_av   FALSE g_av (average SD, Hedges' correction)
#> 5 t_paired convention   eta2   FALSE                           eta squared
#> 6 t_paired convention omega2   FALSE                         omega squared
subset(conventions(), choice == "ss_type")
#>           handler  choice code default    label
#> 3 anova_factorial ss_type  III    TRUE type III
#> 4 anova_factorial ss_type   II   FALSE  type II
#> 5 anova_factorial ss_type    I   FALSE   type I
```
