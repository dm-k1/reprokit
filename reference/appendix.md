# Write the reporting appendix for a sufficient-statistics object

The APA Publication Manual (seventh edition, section 3.7) says that when
the sufficient set of statistics is extensive, "the information could be
supplied in a supplementary data set or an appendix". `appendix()`
writes that appendix as markdown, in four blocks generated from
reprokit's test definitions:

## Usage

``` r
appendix(
  x,
  ...,
  sharing_reason,
  choices = list(),
  digits = 2L,
  digits_added = 4L,
  threshold = 5,
  extra = character()
)
```

## Arguments

- x:

  A fitted model or test result (what
  [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
  accepts: `lm`, `aov`, `chisq.test`, `t.test` / `cor.test` with `data`;
  two `lm` fits for a mediation, the second one in `...`), a formula
  with `data` and `test`, or an object made by
  [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md).

- ...:

  Passed to
  [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
  when `x` is not already its object (`data`, `test`, the second model
  of a mediation, the choices of a formula analysis).

- sharing_reason:

  Optional: why the raw data are not shared, in the author's words.
  reprokit does not suggest reasons. A missing or blank one gives a
  warning and the appendix line "not stated by the author". What readers
  lose is not an argument: reprokit states it, from the test (see
  `sufficient_stats(...)$lost`).

- choices:

  Named list of answers the object cannot know. `convention`: the
  effect-size convention, as a variant code (e.g. `"d_z"`, `"d_av"`).
  Declaring it is a reprokit rule, not an APA requirement. `ss_type`:
  for a factorial ANOVA, the type of sums of squares (`"III"`, `"II"` or
  `"I"`); only an unbalanced design needs it. Left out, the appendix
  uses type III and says it is reprokit's default, not the author's
  answer; checking a paper never assumes a type. `level`: for a
  regression or mediation, the confidence level of the paper's intervals
  (`"0.95"`, `"0.90"` or `"0.99"`); for a regression, `convention` is
  `"r2"` or `"adj_r2"` (the R-squared the paper reports). Choices
  recorded in a fitted result (tails, level, test variant, test value,
  continuity correction, H0 proportions) are read from it and marked
  "read from the result"; an explicit choice that conflicts with one is
  refused.

- digits:

  Decimals for the APA numbers in block A (p always gets 3).

- digits_added:

  Decimals for the added numbers in block B. They feed back into the
  result, so they get more decimals than APA's two; the object itself
  keeps full precision.

- threshold:

  Passed to
  [`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
  when it is called here.

- extra:

  Names of further numbers to add after block D, or `"all"`. They are
  quantities of the test that APA style does not ask for and that the
  stored summaries determine: the R-squared family (`"r2"`, `"adj_r2"`,
  `"eta2"`, `"omega2"`, `"partial_eta2"`, `"partial_omega2"`), the F of
  each mediation model, or `"beta"` for the standardized coefficients of
  a regression.
  [`coverage_table()`](https://dm-k1.github.io/reprokit/reference/coverage_table.md)
  lists them per test (column `extras`). A name the test does not offer
  is an error that lists the ones it does. The default adds nothing, and
  the rest of the appendix does not change.

## Value

A character vector of markdown lines, class `reprokit_appendix`. Refuses
with an error of class `reprokit_impossible` when summaries do not
suffice.

## Details

- **A.** the numbers APA style asks for, with their values;

- **B.** the numbers APA does not require but the result needs, with
  their values and the reason;

- **C.** the analysis choices, with the author's answers;

- **D.** the APA numbers a reader cannot check from these statistics.

It ends with the reason for not sharing the raw data, what readers lose,
and the disclosure check.

## Examples

``` r
appendix(chisq.test(table(mtcars$am, mtcars$vs)),
         sharing_reason = "Consent did not cover public sharing.")
#> ## Appendix: sufficient statistics for the chi-squared test of independence
#> 
#> The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (§3.7) and for data that "allow readers to confirm the basic reported analyses" (§6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (§3.7). This is that appendix.
#> 
#> ### A. The numbers APA style asks for
#> 
#> | row \ column | 0 | 1 |
#> | --- | --- | --- |
#> | 0 | 12 | 7 |
#> | 1 | 6 | 7 |
#> 
#> | Result | Value |
#> | --- | --- |
#> | χ²(1) | 0.35 |
#> | p | .556 |
#> | N | 32 |
#> | Cramér's V | .17 |
#> 
#> ### B. Numbers APA does not require that the result needs
#> 
#> None: the APA numbers in block A determine the result.
#> 
#> ### C. Analysis choices
#> 
#> | Choice | Answer |
#> | --- | --- |
#> | Test | Yates' continuity correction (read from the chisq.test result) |
#> | Effect-size convention | Cramér's V (declaring it is a reprokit rule; APA does not require it) |
#> 
#> ### D. APA numbers a reader cannot check from these statistics
#> 
#> None: every number in block A follows from blocks A and B.
#> 
#> ### Raw data
#> 
#> Reason for not sharing the raw data: Consent did not cover public sharing.
#> 
#> **What readers lose compared with the raw data:**
#> 
#> - checking that the expected counts are large enough for the chi-squared approximation (expected counts below 5; reprokit does not test this)
#> - exact, Fisher or Monte Carlo alternatives for sparse tables
#> - any variable other than the ones in the table
#> 
#> **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: cell counts (a count above 0 and below 5 is flagged; zeros are not). This lowers the risk of identifying a participant; it does not remove it.

# Add the standardized coefficients (and, for other tests, the R-squared family)
appendix(lm(mpg ~ wt + hp, data = mtcars), extra = "beta",
         sharing_reason = "Consent did not cover public sharing.")
#> ## Appendix: sufficient statistics for the multiple regression
#> 
#> The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (§3.7) and for data that "allow readers to confirm the basic reported analyses" (§6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (§3.7). This is that appendix.
#> 
#> ### A. The numbers APA style asks for
#> 
#> n = 32
#> 
#> | Variable | M | SD |
#> | --- | --- | --- |
#> | wt | 3.22 | 0.98 |
#> | hp | 146.69 | 68.56 |
#> | mpg (outcome) | 20.09 | 6.03 |
#> 
#> Correlations of the model columns and the outcome:
#> 
#> |  | wt | hp | mpg (outcome) |
#> | --- | --- | --- | --- |
#> | wt | 1.00 | .66 | −.87 |
#> | hp | .66 | 1.00 | −.78 |
#> | mpg (outcome) | −.87 | −.78 | 1.00 |
#> 
#> | Coefficient | b | SE | t | p |
#> | --- | --- | --- | --- | --- |
#> | (Intercept) | 37.23 | 1.60 | 23.28 | < .001 |
#> | wt | −3.88 | 0.63 | −6.13 | < .001 |
#> | hp | −0.03 | 0.01 | −3.52 | .001 |
#> 
#> df = 29 for every coefficient. Each coefficient's confidence interval is b ± t(df) × SE at the level the paper states.
#> 
#> | Model | Value |
#> | --- | --- |
#> | R² | .83 |
#> | Adjusted R² | .81 |
#> | F(2, 29) | 69.21 |
#> | p (model) | < .001 |
#> 
#> ### B. Numbers APA does not require that the result needs
#> 
#> | Number | Value | Why it is needed |
#> | --- | --- | --- |
#> | the correlations | shown in block A | APA does not require this; coefficients depend on how the predictors overlap |
#> 
#> ### C. Analysis choices
#> 
#> | Choice | Answer |
#> | --- | --- |
#> | Confidence level | not declared |
#> | Effect-size convention | not declared (declaring it is a reprokit rule; APA does not require it) |
#> | Factor coding | no factor predictors |
#> 
#> ### D. APA numbers a reader cannot check from these statistics
#> 
#> None: every number in block A follows from blocks A and B.
#> 
#> ### Additional numbers
#> 
#> Computed from the stored summaries at the author's request.
#> 
#> | Number | Value |
#> | --- | --- |
#> | β (standardized), wt | −0.63 |
#> | β (standardized), hp | −0.36 |
#> 
#> ### Raw data
#> 
#> Reason for not sharing the raw data: Consent did not cover public sharing.
#> 
#> **What readers lose compared with the raw data:**
#> 
#> - residual diagnostics (normality, outliers, influence)
#> - refitting with other predictors or transformations
#> - heteroscedasticity-robust standard errors
#> 
#> **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: factor levels (unweighted rows in the fit), counts of 0/1 columns and their pairwise tables. This lowers the risk of identifying a participant; it does not remove it.

sleep_wide <- data.frame(drug1 = sleep$extra[sleep$group == 1],
                         drug2 = sleep$extra[sleep$group == 2])
appendix(Pair(drug1, drug2) ~ 1, data = sleep_wide, test = "paired",
         sharing_reason = "Consent did not cover public sharing.",
         choices = list(convention = "d_z"))
#> ## Appendix: sufficient statistics for the paired-samples t-test
#> 
#> The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (§3.7) and for data that "allow readers to confirm the basic reported analyses" (§6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (§3.7). This is that appendix.
#> 
#> ### A. The numbers APA style asks for
#> 
#> |  | drug1 | drug2 |
#> | --- | --- | --- |
#> | M | 0.75 | 2.33 |
#> | SD | 1.79 | 2.00 |
#> 
#> n = 10
#> 
#> | Result | Value |
#> | --- | --- |
#> | t(9) | −4.06 |
#> | p | .003 |
#> | d, d_z (SD of the differences) | −1.28 |
#> | 95% CI of the mean difference (drug1 − drug2) | [−2.46, −0.70] |
#> 
#> ### B. Numbers APA does not require that the result needs
#> 
#> | Number | Value | Why it is needed |
#> | --- | --- | --- |
#> | r between drug1 and drug2 | .7952 | APA does not require this; the SD of the differences depends on the correlation between conditions; per-condition M and SD do not determine it |
#> 
#> ### C. Analysis choices
#> 
#> | Choice | Answer |
#> | --- | --- |
#> | Tails | two-sided |
#> | Confidence level | 95% |
#> | Effect-size convention | d_z (SD of the differences) (declaring it is a reprokit rule; APA does not require it) |
#> | Direction of the difference | drug1 minus drug2 |
#> 
#> ### D. APA numbers a reader cannot check from these statistics
#> 
#> None: every number in block A follows from blocks A and B.
#> 
#> ### Raw data
#> 
#> Reason for not sharing the raw data: Consent did not cover public sharing.
#> 
#> **What readers lose compared with the raw data:**
#> 
#> - checking the distribution and outliers of the differences
#> - nonparametric or robust alternatives
#> 
#> **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: sample size only (no groups or cells; no individual values stored). This lowers the risk of identifying a participant; it does not remove it.
```
