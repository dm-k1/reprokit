# reprokit

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dm-k1/reprokit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dm-k1/reprokit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Sharing raw data remains the standard. When sharing is genuinely impossible,
> reprokit says what the minimum is that authors owe their readers, and what
> those readers lose.

reprokit works in two directions, using the same statistics.

* **Share.** If you cannot share your raw data, reprokit tells you whether
  summary statistics are enough for readers to reproduce your result exactly,
  stores those statistics, and writes an appendix that lists them.
* **Check.** If you are reading a paper that reports its statistics, reprokit
  tests whether the reported numbers are consistent with each other, taking
  rounding into account.

Raw data stays the standard. `sufficient_stats()` and `appendix()` require you
to state why the data are not shared, and they list what readers lose by getting
summaries instead. reprokit does not make withholding data easier to justify; it
makes the minimum you owe your readers explicit.

reprokit needs R >= 4.1 and depends only on base R (`stats`, `utils`).

## Installation

reprokit is not on CRAN. Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("dm-k1/reprokit")

# or
# install.packages("remotes")
remotes::install_github("dm-k1/reprokit")
```

The vignettes are not built by default. To get them, install with:

```r
pak::pak("dm-k1/reprokit", dependencies = TRUE, build_vignettes = TRUE)
# or
remotes::install_github("dm-k1/reprokit", build_vignettes = TRUE)
```

Then open them with `vignette("reprokit")`. Without installing, you can read
the sources on GitHub:
[reprokit.Rmd](https://github.com/dm-k1/reprokit/blob/main/vignettes/reprokit.Rmd),
[concepts.Rmd](https://github.com/dm-k1/reprokit/blob/main/vignettes/concepts.Rmd),
[validation.Rmd](https://github.com/dm-k1/reprokit/blob/main/vignettes/validation.Rmd).

## Share: you cannot share the raw data

`triage()` says whether summaries suffice. It never errors; it always returns a
verdict, one of:

* `summaries_suffice`: summary statistics determine the reported results exactly.
* `synthetic_possible`: summaries do not suffice, but synthetic data could.
* `needs_raw_data`: neither works, or the groups are so small that summaries are
  almost the data.

`sufficient_stats()` then stores the statistics. It requires a reason for not
sharing:

```r
library(reprokit)

s <- sufficient_stats(extra ~ group, data = sleep, test = "welch",
                      sharing_reason = "Consent did not cover public sharing.")
s
#> <reprokit sufficient_stats>  handler: t_independent 
#> Verdict:        summaries_suffice -- Summary statistics suffice: per-group n, mean and SD determine the reported results exactly.
#> Not shared because: Consent did not cover public sharing. 
#> Statistics stored: n, mean, sd
#> What readers lose:
#>   * checking the distribution and outliers within each group
#>   * nonparametric or robust alternatives
```

`reproduce(s)` recomputes the test from the stored statistics alone, and
`appendix(s)` writes the appendix for your paper (Markdown, ready to paste):

```r
reproduce(s)
#>           t          df           p   mean_diff    ci_lower    ci_upper 
#> -1.86081347 17.77647352  0.07939414 -1.58000000 -3.36548323  0.20548323

appendix(s, choices = list(convention = "pooled"))
```

The appendix lists the numbers APA style asks for, any further numbers a reader
needs, the analysis choices you made (tails, confidence level, which test,
which effect-size convention), the numbers a reader cannot check from these
statistics, why the raw data are not shared, what readers lose, and a
disclosure check on small groups.

## Check: you are reading a paper

Pass the reported numbers as **strings**, so the decimals count: `"2.85"`
means anything from 2.845 to 2.855.

```r
check_t(handler = "t_independent",
        n = c("10", "10"), m = c("0.75", "2.33"), sd = c("1.79", "2.00"),
        t = "-1.86", df = "17.8", p = ".079")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Variants that fit: two-sided
#> Tests that fit: Welch's t
#> 
#>  quantity   reported   status                checked_against
#>         n     10, 10 coherent df_from_n; t_from_descriptives
#>         m 0.75, 2.33 coherent            t_from_descriptives
#>        sd 1.79, 2.00 coherent df_from_n; t_from_descriptives
#>         t      -1.86 verified  t_from_descriptives; p_from_t
#>        df       17.8 verified            df_from_n; p_from_t
#>         p       .079 coherent                       p_from_t
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

Reading the check output:

| Part | Meaning |
| --- | --- |
| First line | The numbers are consistent or inconsistent with each other. |
| Level 0 to 3 | How much could be checked: 0 nothing; 1 statistic, df and p against each other; 2 the statistic recomputed from the descriptives; 3 the whole sufficient set is reported. |
| `verified` | An output (t, df) was recomputed from more basic numbers and matches. |
| `coherent` | The number fits the others, but only as an input to a check, or through numbers derived from other numbers. |
| `inconsistent` | The number is in a relation that fails under every variant, or in relations no single analysis choice satisfies together. |
| `not checkable` | No relation of this test reaches the number. |
| `unconfirmed` | The number's relation could not be evaluated because another input is missing. |
| Missing numbers | If a number was not reported but the others constrain it, its implied range is shown. |

A set of numbers that cannot all be right is reported as inconsistent:

```r
check_t(n = "20", t = "2.85", df = "19", p = ".050")
#> The analysis is inconsistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Variants that fit: none
#> # ... (rest of the output elided)
```

"Inconsistent" means the numbers cannot all be correct as reported. It does not
say which number is wrong, or why: a typo, a different test, a different
rounding or a real mistake all look the same. `check_r()` does the same for a
Pearson correlation, and `check_reported()` is the general function behind both.

## What it covers

Independent-samples t (Student and Welch), paired-samples t, one-sample t and
Pearson correlation, and a one-way between-subjects ANOVA (check direction),
chi-squared tests of independence and goodness of fit, and a two-factor
between-subjects factorial ANOVA. For each test, `coverage_table()` lists the numbers APA style reports, the sufficient
set, what is missing from the APA set, what can be checked and what cannot (with
the reason), and the choices you have to declare. The table is generated from
what the package does, so it is the reference:

```r
coverage_table()
```

A one-way ANOVA is checked with `check_F()` (note the lowercase `f` for the
statistic): per-group n, mean and SD, plus F, both df, eta squared and p.

```r
check_F(n = c("10", "10", "10"), m = c("5.03", "4.66", "5.53"),
        sd = c("0.58", "0.79", "0.44"),
        f = "4.85", df1 = "2", df2 = "27", eta2 = "0.26", p = ".016")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (...)
#> Effect-size conventions that fit: eta squared
#> # ... (table elided)
```

A two-factor between-subjects ANOVA is checked with `check_F()` too, one effect
at a time: pass the cell n, M and SD as a x b matrices (rows are the levels of A,
columns the levels of B) and name the `effect` (`"A"`, `"B"` or `"AB"`). In an
unbalanced design the sums of squares type (III, II or I) is a choice the report
must settle; the output lists the types that fit. Partial eta squared and partial
omega squared are conventions.

```r
cells <- matrix(rep("10", 4), 2)
check_F(n = cells, m = matrix(c("4.10", "5.20", "4.80", "7.40"), 2),
        sd = matrix(rep("1.50", 4), 2), effect = "A",
        f = "15.21", df1 = "1", df2 = "36", p = "< .001", eta2 = ".30")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (...)
#> # ... (table elided)
```

A chi-squared test is checked with `check_chisq()`: pass the table as a matrix
(a vector for goodness of fit) and the reported numbers. Yates' correction is a
variant, Cramer's V a convention.

```r
check_chisq(n = matrix(c("30", "10", "20", "40"), 2), chisq = "15.04", df = "1",
            p = ".0001", v = ".41")
```

## Documentation

* `vignette("reprokit")`: getting started, one worked example for authors and
  one for readers.
* `vignette("concepts")`: what the terms mean (sufficient set, APA set,
  coverage level, verdicts, rounding intervals, and so on).
* `vignette("validation")`: the evidence behind the claims, and the scripts that
  produce it.

## Status and limits

reprokit is pre-release (version 0.0.0.9000); function names and output may
still change. In the current version:

* It checks whether reported numbers agree with each other. It does not check
  whether the analysis was appropriate, whether the data are plausible, or
  which number is wrong.
* It cannot replace the raw data. For analyses where summaries do not suffice,
  `triage()` says so (`synthetic_possible` or `needs_raw_data`) and
  `sufficient_stats()` refuses with the alternatives.
* The disclosure check on small groups lowers the risk of identifying a
  participant; it does not remove it.

## Reporting bugs

Please open an issue at <https://github.com/dm-k1/reprokit/issues>. A minimal
example with the reported numbers as strings helps most.

## Citation

Run `citation("reprokit")` for the citation.

## Licence

MIT, see the file `LICENSE`.

## Prior art

statcheck, GRIM/GRIMMER/SPRITE (rsprite2), synthpop and DataSHIELD. reprokit
builds on these rather than replacing them.
