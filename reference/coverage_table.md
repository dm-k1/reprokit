# Coverage table generated from the handler registry

One row per registered test that has an APA design, generated from the
registry so it always matches what the package does. Missing, checkable
and the choices from relations are computed; only the reasons and extra
choices are declared by each handler.

## Usage

``` r
coverage_table()
```

## Value

A data frame of character columns, one row per test:

- `test`: the analysis;

- `verdict`: the tier the analysis reaches, one of
  [`verdicts()`](https://dm-k1.github.io/reprokit/reference/verdicts.md):
  `apa_suffices` when the APA numbers determine every summary the
  analysis needs, `apa_plus_suffices` when a number beyond them is
  needed (the tier is computed from the additions each handler declares,
  not set per test). For the goodness-of-fit test the null proportions
  are an addition only when they are not equal, so the tier depends on
  the data; the table shows the stricter case, `apa_plus_suffices`;

- `apa`: what APA style asks a paper to report;

- `sufficient`: the quantities that determine every reported result;

- `missing`: sufficient but not in APA, and not covered by an identity,
  each with its reason (these are the additions that make the verdict
  `apa_plus_suffices`);

- `covered`: sufficient but not in APA, yet determined by the APA
  numbers through an exact identity at printed precision, each with that
  identity;

- `checkable`: APA numbers the relations derive from `sufficient`;

- `not_checkable`: APA numbers they do not reach, each with its reason;

- `choices`: what the author must declare;

- `extras`: the further quantities `appendix(extra = )` can add, which
  the stored summaries determine.

An empty cell shows an em dash. Handlers without a design are left out.

## Examples

``` r
coverage_table()
#>                                               test           verdict
#> 1                         Pearson correlation test      apa_suffices
#> 2                            Paired-samples t-test apa_plus_suffices
#> 3                                       Moderation apa_plus_suffices
#> 4                              Multiple regression apa_plus_suffices
#> 5                                 Simple mediation apa_plus_suffices
#> 6                                Simple regression      apa_suffices
#> 7                      Chi-squared goodness of fit apa_plus_suffices
#> 8                 Chi-squared test of independence      apa_suffices
#> 9  Factorial ANOVA, two factors (between-subjects)      apa_suffices
#> 10     Independent-samples t-test (Student, Welch)      apa_suffices
#> 11                               One-sample t-test      apa_suffices
#> 12                One-way ANOVA (between-subjects)      apa_suffices
#>                                                                 apa
#> 1                               n, M, SD per variable, r, df, p, CI
#> 2                           n, M, SD per condition, t, df, p, d, CI
#> 3  n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p
#> 4  n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p
#> 5            n, M, SD per variable, b, SE, t, p, CI, a·b and its CI
#> 6  n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p
#> 7                              the observed counts, chisq, df, p, N
#> 8                         the contingency table, chisq, df, p, N, V
#> 9              n, M, SD per condition, F, df1, df2, p, partial eta2
#> 10                          n, M, SD per condition, t, df, p, d, CI
#> 11                                        n, M, SD, t, df, p, d, CI
#> 12                     n, M, SD per condition, F, df1, df2, p, eta2
#>                                                  sufficient
#> 1                                                      n, r
#> 2                                 n, M, SD per condition, r
#> 3  n, M, SD per variable, the correlations, the X·Z moments
#> 4                   n, M, SD per variable, the correlations
#> 5                      n, M, SD and correlations of X, M, Y
#> 6                                  n, M, SD per variable, r
#> 7                       the observed counts, H0 proportions
#> 8                                     the contingency table
#> 9                                    n, M, SD per condition
#> 10                                   n, M, SD per condition
#> 11                                                 n, M, SD
#> 12                                   n, M, SD per condition
#>                                                                                                                             missing
#> 1                                                                                                                                 —
#> 2           r (the SD of the differences depends on the correlation between conditions; per-condition M and SD do not determine it)
#> 3  the correlations (coefficients depend on how the predictors overlap); the X·Z moments (cannot be derived from X and Z summaries)
#> 4                                                              the correlations (coefficients depend on how the predictors overlap)
#> 5                                                                                                       the correlations of X, M, Y
#> 6                                                                                                                                 —
#> 7                                               H0 proportions (the expected counts depend on them; not needed when they are equal)
#> 8                                                                                                                                 —
#> 9                                                                                                                                 —
#> 10                                                                                                                                —
#> 11                                                                                                                                —
#> 12                                                                                                                                —
#>                                           covered
#> 1                                               —
#> 2                                               —
#> 3                                               —
#> 4                                               —
#> 5                                               —
#> 6  r (standardized β, sign(b)·√R², or t/√(t²+df))
#> 7                                               —
#> 8                                               —
#> 9                                               —
#> 10                                              —
#> 11                                              —
#> 12                                              —
#>                                    checkable
#> 1                                  df, p, CI
#> 2                            t, df, p, d, CI
#> 3  b, SE, t, p, CI, R², F, df1, df2, model p
#> 4  b, SE, t, p, CI, R², F, df1, df2, model p
#> 5         a, b, c, c′ (b, SE, t, p, CI each)
#> 6  b, SE, t, p, CI, R², F, df1, df2, model p
#> 7                            chisq, df, p, N
#> 8                         chisq, df, p, N, V
#> 9               F, df1, df2, p, partial eta2
#> 10                           t, df, p, d, CI
#> 11                           t, df, p, d, CI
#> 12                      F, df1, df2, p, eta2
#>                                                                     not_checkable
#> 1                                  M, SD per variable (r does not depend on them)
#> 2                                                                               —
#> 3                                                                               —
#> 4                                                                               —
#> 5  the CI of a·b (inferred by bootstrap, which resamples cases; summaries cannot)
#> 6                                                                               —
#> 7                                                                               —
#> 8                                                                               —
#> 9                                                                               —
#> 10                                                                              —
#> 11                                                                              —
#> 12                                                                              —
#>                                                                                                                                                                                                                                                                                                                                     choices
#> 1                                                                                                                                                                                                                                                tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%)
#> 2  tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); effect-size convention (d_z (SD of the differences), d_rm (repeated measures), d_av (average SD), g_av (average SD, Hedges' correction), eta squared, omega squared); direction of the difference: first minus second in Pair(first, second)
#> 3                                                                                                                                                                                                                                             confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²); centering; coding
#> 4                                                                                                                                                                                                                                                 confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²); factor coding
#> 5                                                                                                                                                                                                                                                                                                          confidence level (90%, 95%, 99%)
#> 6                                                                                                                                                                                                                                                                confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²)
#> 7                                                                                                                                                                                                                                                                                                        effect-size convention (Cohen's w)
#> 8                                                                                                                                                                                                                                             test (Pearson chi-squared, Yates' continuity correction); effect-size convention (Cramér's V)
#> 9                                                                                                                                                                                                           SS type for unbalanced designs (type III, type II, type I); effect-size convention (partial eta squared, partial omega squared)
#> 10         tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); test (Student's t, Welch's t); effect-size convention (Cohen's d (pooled SD), Hedges' g, Cohen's d (root mean square of the SDs), Glass's Δ (SD of the first group), Glass's Δ (SD of the second group), eta squared, omega squared)
#> 11                                                                                                                                         tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); effect-size convention (Cohen's d, Hedges' g, eta squared, omega squared); test value mu (default 0)
#> 12                                                                                                                                                                                                                effect-size convention (eta squared, omega squared); variance assumption: equal variances (classical F test, not Welch's)
#>                          extras
#> 1               R², adjusted R²
#> 2              eta2, R², omega2
#> 3             standardized beta
#> 4             standardized beta
#> 5                         R², F
#> 6             standardized beta
#> 7                             —
#> 8                             —
#> 9  partial eta2, partial omega2
#> 10             eta2, R², omega2
#> 11             eta2, R², omega2
#> 12                   R², omega2
```
