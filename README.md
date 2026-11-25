
<!-- README.md is generated from README.Rmd. Edit README.Rmd, then run devtools::build_readme(). -->

# reprokit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dm-k1/reprokit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dm-k1/reprokit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Sharing raw data remains the standard. When sharing is genuinely
> impossible, reprokit says what the minimum is that authors owe their
> readers, and what those readers lose.

reprokit works in two directions, using the same statistics.

- **Share.** If you cannot share your raw data, reprokit tells you
  whether summary statistics are enough for readers to reproduce your
  result exactly, stores those statistics, and writes an appendix that
  lists them.
- **Check.** If you are reading a paper that reports its statistics,
  reprokit tests whether the reported numbers are consistent with each
  other, taking rounding into account.

Raw data stays the standard. `sufficient_stats()` and `appendix()`
require you to state why the data are not shared, and they list what
readers lose by getting summaries instead. reprokit does not make
withholding data easier to justify; it makes the minimum you owe your
readers explicit.

reprokit needs R \>= 4.1 and depends only on base R (`stats`, `utils`).

## Installation

reprokit is not on CRAN. Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("dm-k1/reprokit")

# or
# install.packages("remotes")
remotes::install_github("dm-k1/reprokit")
```

The vignettes are not built by default. To get them, install with:

``` r
pak::pak("dm-k1/reprokit", dependencies = TRUE, build_vignettes = TRUE)
# or
remotes::install_github("dm-k1/reprokit", build_vignettes = TRUE)
```

Then open them with `vignette("reprokit")`. Without installing, you can
read the sources on GitHub:
[reprokit.Rmd](https://github.com/dm-k1/reprokit/blob/main/vignettes/reprokit.Rmd),
[concepts.Rmd](https://github.com/dm-k1/reprokit/blob/main/vignettes/concepts.Rmd),
[validation.Rmd](https://github.com/dm-k1/reprokit/blob/main/vignettes/validation.Rmd).

## Share: you cannot share the raw data

`triage()` says whether summaries suffice. It never errors; it always
returns a verdict, one of:

- `summaries_suffice`: summary statistics determine the reported results
  exactly.
- `synthetic_possible`: summaries do not suffice, but synthetic data
  could.
- `needs_raw_data`: neither works, or the groups are so small that
  summaries are almost the data.

`sufficient_stats()` then stores the statistics. It requires a reason
for not sharing:

``` r
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
`appendix(s)` writes the appendix for your paper (Markdown, ready to
paste):

``` r
reproduce(s)
#>           t          df           p   mean_diff    ci_lower    ci_upper 
#> -1.86081347 17.77647352  0.07939414 -1.58000000 -3.36548323  0.20548323

appendix(s, choices = list(convention = "pooled"))
#> ## Appendix: sufficient statistics for the independent-samples t-test
#> 
#> The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (§3.7) and for data that "allow readers to confirm the basic reported analyses" (§6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (§3.7). This is that appendix.
#> 
#> ### A. The numbers APA style asks for
#> 
#> |  | group 1 | group 2 |
#> | --- | --- | --- |
#> | n | 10 | 10 |
#> | M | 0.75 | 2.33 |
#> | SD | 1.79 | 2.00 |
#> 
#> | Result | Value |
#> | --- | --- |
#> | t(17.78) | −1.86 |
#> | p | .079 |
#> | d, Cohen's d (pooled SD) | −0.83 |
#> | 95% CI of the mean difference (group 1 − group 2) | [−3.37, 0.21] |
#> 
#> ### B. Numbers APA does not require that the result needs
#> 
#> None: the APA numbers in block A determine the result.
#> 
#> ### C. Analysis choices
#> 
#> | Choice | Answer |
#> | --- | --- |
#> | Tails | two-sided |
#> | Confidence level | 95% |
#> | Test | Welch's t |
#> | Effect-size convention | Cohen's d (pooled SD) (declaring it is a reprokit rule; APA does not require it) |
#> 
#> ### D. APA numbers a reader cannot check from these statistics
#> 
#> None: every number in block A follows from blocks A and B.
#> 
#> ### Raw data
#> 
#> **Why the raw data are not shared.** Consent did not cover public sharing.
#> 
#> **What readers lose compared with the raw data:**
#> 
#> - checking the distribution and outliers within each group
#> - nonparametric or robust alternatives
#> 
#> **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: group sizes. This lowers the risk of identifying a participant; it does not remove it.
```

The appendix lists the numbers APA style asks for, any further numbers a
reader needs, the analysis choices you made (tails, confidence level,
which test, which effect-size convention), the numbers a reader cannot
check from these statistics, why the raw data are not shared, what
readers lose, and a disclosure check on small groups.

## Check: you are reading a paper

Pass the reported numbers as **strings**, so the decimals count:
`"2.85"` means anything from 2.845 to 2.855.

``` r
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
|----|----|
| First line | The numbers are consistent or inconsistent with each other. |
| Level 0 to 3 | How much could be checked: 0 nothing; 1 statistic, df and p against each other; 2 the statistic recomputed from the descriptives; 3 the whole sufficient set is reported. |
| `verified` | An output (t, df) was recomputed from more basic numbers and matches. |
| `coherent` | The number fits the others, but only as an input to a check, or through numbers derived from other numbers. |
| `inconsistent` | The number is in a relation that fails under every variant, or in relations no single analysis choice satisfies together. |
| `not checkable` | No relation of this test reaches the number. |
| `unconfirmed` | The number’s relation could not be evaluated because another input is missing. |
| Missing numbers | If a number was not reported but the others constrain it, its implied range is shown. |

A set of numbers that cannot all be right is reported as inconsistent:

``` r
check_t(n = "20", t = "2.85", df = "19", p = ".050")
#> The analysis is inconsistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Variants that fit: none
#> 
#>  quantity reported       status     checked_against
#>         n       20     coherent           df_from_n
#>         t     2.85 inconsistent            p_from_t
#>        df       19 inconsistent df_from_n; p_from_t
#>         p     .050 inconsistent            p_from_t
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

“Inconsistent” means the numbers cannot all be correct as reported. It
does not say which number is wrong, or why: a typo, a different test, a
different rounding or a real mistake all look the same. `check_r()` does
the same for a Pearson correlation, and `check_reported()` is the
general function behind both.

## What it covers

Version 0.1.0 covers, for share, check and appendix alike:

- chi-squared tests of independence and goodness of fit;
- one-sample, independent-samples (Student and Welch) and paired-samples
  t;
- Pearson correlation;
- one-way and factorial (two-factor) between-subjects ANOVA;
- simple, multiple and moderation regression, and simple mediation.

Everything else gets triage only: a verdict and advice, but no stored
statistics and no check (repeated-measures ANOVA, logistic and Poisson
regression, SEM, rank tests and Spearman, standalone effect sizes,
multilevel and mixed models, GEE).

`coverage_table()` lists, for each test, the numbers APA style reports,
the sufficient set, what is missing from the APA set, what can be
checked and what cannot (with the reason), and the choices you have to
declare. The table below is generated from the handler registry when
this README is built, so it is the reference, not a copy:

| test | apa | sufficient | missing | checkable | not_checkable | choices |
|:---|:---|:---|:---|:---|:---|:---|
| Pearson correlation test | n, M, SD per variable, r, df, p, CI | n, r | — | df, p, CI | M, SD per variable (r does not depend on them) | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%) |
| Paired-samples t-test | n, M, SD per condition, t, df, p, d, CI | n, M, SD per condition, r | r (the SD of the differences depends on the correlation between conditions; per-condition M and SD do not determine it) | t, df, p, d, CI | — | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); effect-size convention (d_z (SD of the differences), d_rm (repeated measures), d_av (average SD), g_av (average SD, Hedges’ correction)); direction of the difference: first minus second in Pair(first, second) |
| Moderation | n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p | n, M, SD per variable, the correlations, the X·Z moments | the correlations (coefficients depend on how the predictors overlap); the X·Z moments (cannot be derived from X and Z summaries) | b, SE, t, p, CI, R², F, df1, df2, model p | — | confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²); centering; coding |
| Multiple regression | n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p | n, M, SD per variable, the correlations | the correlations (coefficients depend on how the predictors overlap) | b, SE, t, p, CI, R², F, df1, df2, model p | — | confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²); factor coding |
| Simple mediation | n, M, SD per variable, b, SE, t, p, CI, a·b and its CI | n, M, SD and correlations of X, M, Y | the correlations of X, M, Y | a, b, c, c′ (b, SE, t, p, CI each) | a·b and its CI (inferred by bootstrap, which resamples cases; summaries cannot) | confidence level (90%, 95%, 99%) |
| Simple regression | n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p | n, M, SD per variable, r | — | b, SE, t, p, CI, R², F, df1, df2, model p | — | confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²) |
| Chi-squared goodness of fit | the observed counts, chisq, df, p, N | the observed counts, H0 proportions | H0 proportions (the expected counts depend on them; not needed when they are equal) | chisq, df, p, N | — | effect-size convention (Cohen’s w) |
| Chi-squared test of independence | the contingency table, chisq, df, p, N, V | the contingency table | — | chisq, df, p, N, V | — | test (Pearson chi-squared, Yates’ continuity correction); effect-size convention (Cramér’s V) |
| Factorial ANOVA, two factors (between-subjects) | n, M, SD per condition, F, df1, df2, p, partial eta2 | n, M, SD per condition | — | F, df1, df2, p, partial eta2 | — | SS type for unbalanced designs (type III, type II, type I); effect-size convention (partial eta squared, partial omega squared) |
| Independent-samples t-test (Student, Welch) | n, M, SD per condition, t, df, p, d, CI | n, M, SD per condition | — | t, df, p, d, CI | — | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); test (Student’s t, Welch’s t); effect-size convention (Cohen’s d (pooled SD), Hedges’ g, Cohen’s d (root mean square of the SDs), Glass’s Δ (SD of the first group), Glass’s Δ (SD of the second group)) |
| One-sample t-test | n, M, SD, t, df, p, d, CI | n, M, SD | — | t, df, p, d, CI | — | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); effect-size convention (Cohen’s d, Hedges’ g); test value mu (default 0) |
| One-way ANOVA (between-subjects) | n, M, SD per condition, F, df1, df2, p, eta2 | n, M, SD per condition | — | F, df1, df2, p, eta2 | — | effect-size convention (eta squared, omega squared); variance assumption: equal variances (classical F test, not Welch’s) |

A one-way ANOVA is checked with `check_F()` (note the lowercase `f` for
the statistic): per-group n, mean and SD, plus F, both df, eta squared
and p.

``` r
check_F(n = c("10", "10", "10"), m = c("5.03", "4.66", "5.53"),
        sd = c("0.58", "0.79", "0.44"),
        f = "4.85", df1 = "2", df2 = "27", eta2 = "0.26", p = ".016")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Effect-size conventions that fit: eta squared
#> 
#>  quantity         reported   status                             checked_against
#>         n       10, 10, 10 coherent df1_from_n; df2_from_n; f_from_descriptives
#>         m 5.03, 4.66, 5.53 coherent                         f_from_descriptives
#>        sd 0.58, 0.79, 0.44 coherent                         f_from_descriptives
#>         f             4.85 verified  f_from_descriptives; p_from_f; eta2_from_f
#>       df1                2 verified           df1_from_n; p_from_f; eta2_from_f
#>       df2               27 verified           df2_from_n; p_from_f; eta2_from_f
#>      eta2             0.26 coherent                                 eta2_from_f
#>         p             .016 coherent                                    p_from_f
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

A two-factor between-subjects ANOVA is checked with `check_F()` too, one
effect at a time: pass the cell n, M and SD as a x b matrices (rows are
the levels of A, columns the levels of B) and name the `effect` (`"A"`,
`"B"` or `"AB"`). In an unbalanced design the sums of squares type (III,
II or I) is a choice the report must settle; the output lists the types
that fit. Partial eta squared and partial omega squared are conventions.

``` r
cells <- matrix(rep("10", 4), 2)
check_F(n = cells, m = matrix(c("4.10", "5.20", "4.80", "7.40"), 2),
        sd = matrix(rep("1.50", 4), 2), effect = "A",
        f = "15.21", df1 = "1", df2 = "36", p = "< .001", eta2 = ".30")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Effect-size conventions that fit: partial eta squared
#> 
#>  quantity               reported   status
#>         n         10, 10, 10, 10 coherent
#>         m 4.10, 5.20, 4.80, 7.40 coherent
#>        sd 1.50, 1.50, 1.50, 1.50 coherent
#>         f                  15.21 verified
#>       df1                      1 coherent
#>       df2                     36 verified
#>         p                 < .001 verified
#>      eta2                    .30 coherent
#>                                            checked_against
#>       df2_from_n; f_from_descriptives; p_from_descriptives
#>                   f_from_descriptives; p_from_descriptives
#>                   f_from_descriptives; p_from_descriptives
#>  df1_from_dims; f_from_descriptives; p_from_f; eta2_from_f
#>                       df1_from_dims; p_from_f; eta2_from_f
#>                          df2_from_n; p_from_f; eta2_from_f
#>                              p_from_descriptives; p_from_f
#>                                                eta2_from_f
#> 
#> Note: Effect checked: A (the factor in the rows of the cell matrices).
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

A chi-squared test is checked with `check_chisq()`: pass the table as a
matrix (a vector for goodness of fit) and the reported numbers. Yates’
correction is a variant, Cramer’s V a convention.

``` r
check_chisq(n = matrix(c("30", "10", "20", "40"), 2), chisq = "15.04", df = "1",
            p = ".0001", v = ".41")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Tests that fit: Yates' continuity correction
#> Effect-size conventions that fit: none
#> 
#>  quantity       reported   status                              checked_against
#>         n 30, 10, 20, 40 coherent chisq_from_table; p_from_table; v_from_table
#>     chisq          15.04 verified chisq_from_table; df_from_dims; p_from_chisq
#>        df              1 coherent                   df_from_dims; p_from_chisq
#>         p          .0001 verified                   p_from_table; p_from_chisq
#>         v            .41 verified                                 v_from_table
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

A regression is checked with `check_lm()`, with one named value per
term; pass `handler = "lm_simple"` or `"lm_moderation"` for those rows.
Simple mediation is checked with `check_mediation()`, which checks the
paths a, b, c and c’ and says nothing about the indirect effect.

``` r
check_lm(b = c(wt = "-3.19", hp = "-0.04"), se = c(wt = "0.64", hp = "0.01"),
         t = c(wt = "-5.00", hp = "-3.67"), df2 = "29", k = 2)
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> 
#>  quantity reported        status checked_against
#>      b:wt    -3.19      coherent  t_from_b_se:wt
#>      b:hp    -0.04      coherent  t_from_b_se:hp
#>     se:wt     0.64      coherent  t_from_b_se:wt
#>     se:hp     0.01      coherent  t_from_b_se:hp
#>      t:wt    -5.00      coherent  t_from_b_se:wt
#>      t:hp    -3.67      coherent  t_from_b_se:hp
#>       df2       29 not checkable               -
#> 
#> Derived from the reported numbers: n = 32 (from df2).
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

## Documentation

- `vignette("reprokit")`: getting started, one worked example for
  authors and one for readers.
- `vignette("concepts")`: what the terms mean (sufficient set, APA set,
  coverage level, verdicts, rounding intervals, and so on).
- `vignette("validation")`: the evidence behind the claims, and the
  scripts that produce it.

## Status and limits

reprokit is version 0.1.0, the first release, with lifecycle
“experimental”: function names and output may still change. In this
version:

- It checks whether reported numbers agree with each other. It does not
  check whether the analysis was appropriate, whether the data are
  plausible, or which number is wrong.
- It cannot replace the raw data. For analyses where summaries do not
  suffice, or that are outside the list above, `triage()` says so
  (`synthetic_possible` or `needs_raw_data`) and `sufficient_stats()`
  refuses with the alternatives.
- The disclosure check flags groups, cells, categories and factor levels
  with fewer than 5 cases (the default `threshold`). It lowers the risk
  of identifying a participant; it never removes it, and passing it is
  not a guarantee of anonymity.
- The evidence for the claims is in `inst/validation/`; each script
  writes its result to `inst/validation/results/`, and
  `vignette("validation")` shows them.

## Reporting bugs

Please open an issue at <https://github.com/dm-k1/reprokit/issues>. A
minimal example with the reported numbers as strings helps most.

## Citation

Run `citation("reprokit")` for the citation.

## Licence

MIT, see the file `LICENSE`.

## Prior art

reprokit builds on these rather than replacing them:

- [statcheck](https://github.com/MicheleNuijten/statcheck) (Nuijten and
  Epskamp): recomputes p values from reported test statistics.
- JATSdecoder `get.stats()` (Böschen): extracts statistical results from
  articles.
- scrutiny (Jung): error detection for reported summary statistics,
  including GRIM.
- rsprite2 (Wallrich): SPRITE, reconstructing possible samples from a
  mean and SD.
- unsum and CLOSURE (Jung): the same reconstruction, by exhaustive
  search.
- Nick Brown’s `f_range()`: the range of F values consistent with
  rounded means and SDs.
- RIVETS (Brown and Heathers): a checklist for detecting problems in a
  study report.
- [synthpop](https://www.synthpop.org.uk/) (Nowok, Raab and Dibben):
  synthetic data.
- faux (DeBruine): simulating data from a specified structure.
- DataSHIELD: analysis without moving the data.
- metacheck (DeBruine, Lakens and others): automated checks of reported
  results.
