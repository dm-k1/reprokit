# Getting started with reprokit

reprokit has two halves. The first is for authors who cannot share their
raw data. The second is for readers who want to know whether the numbers
in a paper are consistent with each other. Both rest on the same fact:
for many common tests, a small set of summary statistics determines the
result exactly.

Sharing raw data remains the standard; reprokit never replaces it.

This vignette walks through one example for each half. The terms it uses
(sufficient set, coverage level and so on) are defined where they first
appear and collected in
[`vignette("concepts")`](https://dm-k1.github.io/reprokit/articles/concepts.md).

``` r

library(reprokit)
```

## The path in short

An author shares summaries instead of data (share), writes the appendix,
and a reader later checks the reported numbers (check):

``` r

model <- t.test(extra ~ group, data = sleep)
s0 <- sufficient_stats(model, data = sleep)
s0$verdict
#> [1] "summaries_suffice"
a0 <- appendix(model, data = sleep, choices = list(convention = "pooled"),
               sharing_reason = "Consent did not cover public sharing.")
cat(head(a0, 6), sep = "\n")
#> ## Appendix: sufficient statistics for the independent-samples t-test
#> 
#> The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (§3.7) and for data that "allow readers to confirm the basic reported analyses" (§6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (§3.7). This is that appendix.
#> 
#> ### A. The numbers APA style asks for
check_reported("welch",
               n = c("10", "10"), m = c("0.75", "2.33"), sd = c("1.79", "2.00"),
               t = "-1.86", df = "17.8", p = ".079")$consistent
#> [1] TRUE
```

The rest of this vignette shows each step in full.

## For authors: you cannot share the raw data

The built-in `sleep` data (extra hours of sleep under two drugs, 10
people per drug) stand in for data you cannot share.

### Step 1. What can be shared: do summaries suffice?

You can start from the result you already have, here a
[`t.test()`](https://rdrr.io/r/stats/t.test.html) result and the data it
came from:

``` r

model <- t.test(extra ~ group, data = sleep)
sufficient_stats(model, data = sleep)
#> <reprokit sufficient_stats>  analysis: independent-samples t-test  (input: t.test)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: per-group n, mean and SD determine the reported results exactly.
#> To publish: per-group n, mean and SD (appendix() writes this out).
#> Disclosure check: passed (group sizes, threshold 5).
#> Choices read from the t.test result: test = Welch.
#> What readers lose compared with the raw data:
#>   * checking the distribution and outliers within each group
#>   * nonparametric or robust alternatives
#> Statistics stored: n, mean, sd
```

[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
never stops with an error on the verdict. “Summaries do not suffice,
because …” is an answer, and the printed object narrates it: the verdict
and why, what to publish, what readers lose, and which group or cell a
disclosure flag fired for. There are three verdicts:

- `summaries_suffice`: the summary statistics determine the reported
  results exactly, and are not so close to the raw data that sharing
  them gains nothing.
- `synthetic_possible`: summaries do not suffice, but synthetic data
  could.
- `needs_raw_data`: neither works. The raw data are needed.

`needs_raw_data` is also the verdict when the groups are so small that
the summaries are almost the data. By default, no group may rest on
fewer than 5 cases and the total must be at least 10 (change this with
`threshold`).

``` r

verdicts()
#> [1] "summaries_suffice"  "synthetic_possible" "needs_raw_data"
```

### Step 2. The sufficient statistics

A **sufficient set** is the smallest set of numbers from which the whole
result can be recomputed. For an independent-samples t-test it is n,
mean and SD per group.
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
stores them at full precision. The object also states what readers lose
compared with the raw data; reprokit derives that from the test, so you
do not write it.

The choices the result records (here Welch, two-sided, 95%) are read
from it and marked as such. A result that does not match its data is
refused, and so is a `data.name` reprokit cannot map to the data (use
the formula route then). The same analysis can be given as a formula:

``` r

sufficient_stats(extra ~ group, data = sleep, test = "welch")
#> <reprokit sufficient_stats>  analysis: independent-samples t-test  (input: formula)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: per-group n, mean and SD determine the reported results exactly.
#> To publish: per-group n, mean and SD (appendix() writes this out).
#> Disclosure check: passed (group sizes, threshold 5).
#> What readers lose compared with the raw data:
#>   * checking the distribution and outliers within each group
#>   * nonparametric or robust alternatives
#> Statistics stored: n, mean, sd
```

Fitted `lm`, `aov` and
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) results work
without `data`. For a mediation give the two regressions,
`sufficient_stats(lm(M ~ X), lm(Y ~ X + M))`.

If the groups are too small, the verdict is `needs_raw_data`, the
disclosure flag is named, and the alternatives are listed: share the raw
data, share synthetic data, or offer federated analysis.

``` r

tiny <- sleep[c(1:3, 11:13), ]
sufficient_stats(extra ~ group, data = tiny, test = "welch")
#> <reprokit sufficient_stats>  analysis: independent-samples t-test  (input: formula)
#> Verdict: needs_raw_data
#> Because: The summaries would be close to the raw data, so they give no real privacy gain, only less information: total n = 6 is below the minimum of 10 (2 x threshold).
#> Disclosure flag fired:
#>   * total n = 6 is below the minimum of 10 (2 x threshold)
#> Nothing is stored: summary statistics do not make this analysis reproducible.
#> What readers lose without the raw data:
#>   * checking the distribution and outliers within each group
#>   * nonparametric or robust alternatives
#> Instead you could:
#>   * share the raw data (possibly under a data-use agreement)
#>   * share synthetic data and report its utility
#>   * offer federated analysis (e.g. DataSHIELD)
```

### Step 3. Write the appendix

[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
reproduces the result from the stored statistics and refuses if that
does not match your original analysis (scaled difference below 1e-8,
roughly 8 significant digits). So a reader who has the statistics gets
the same result. The script `inst/validation/roundtrip-simulation.R`
checks this over many random designs for every supported test (see
[`vignette("validation")`](https://dm-k1.github.io/reprokit/articles/validation.md)).

[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
produces Markdown to paste into your paper’s supplement. It has labelled
blocks:

- **A. The numbers APA style asks for.** The **APA set**: what a paper
  conventionally reports for this test.
- **B. Numbers APA does not require that the result needs.** The
  **missing** numbers: the sufficient set minus the APA set. For the
  independent t-test there are none; for the paired t-test it is the
  correlation between the two conditions.
- **C. Analysis choices.** The things the numbers alone do not tell a
  reader: one- or two-sided, confidence level, Student or Welch, and
  which effect-size convention you used.
- **D. APA numbers a reader cannot check** from these statistics.
- **Raw data.** Why they are not shared, what readers lose, and a
  disclosure check.

The reason for not sharing is optional (`sharing_reason`, given to
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
only). reprokit never suggests or fills one in. Without it,
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
warns and the appendix says “Reason for not sharing the raw data: not
stated by the author.”

The convention for the effect size is yours to declare. A **d
convention** is the formula behind an effect size such as Cohen’s d: for
example `"pooled"` divides the mean difference by the pooled SD,
`"d_av"` by the average of the two SDs, and `"d_z"` (paired design) by
the SD of the differences. The allowed values depend on the test. If you
give a wrong one, the error lists the allowed values.

``` r

appendix(model, data = sleep, choices = list(convention = "d_av"),
         sharing_reason = "Consent did not cover public sharing.")
#> Error in `appendix_answers()`:
#> ! `choices$convention` must be one of: "pooled", "hedges_g", "average", "glass_sd1", "glass_sd2".
```

``` r

appendix(model, data = sleep, choices = list(convention = "pooled"),
         sharing_reason = "Consent did not cover public sharing.")
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
#> | Tails | two-sided (read from the t.test result) |
#> | Confidence level | 95% (read from the t.test result) |
#> | Test | Welch's t (read from the t.test result) |
#> | Effect-size convention | Cohen's d (pooled SD) (declaring it is a reprokit rule; APA does not require it) |
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
#> - checking the distribution and outliers within each group
#> - nonparametric or robust alternatives
#> 
#> **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: group sizes. This lowers the risk of identifying a participant; it does not remove it.
```

The disclosure check at the end lowers the risk that a small group
identifies a participant. It does not remove it, and passing it is not a
guarantee. It flags a group with fewer than 5 cases (the `threshold`);
for a chi-squared test it flags a cell or category with a count above 0
and below the threshold (empty cells are not flagged), and for a
regression with factor predictors a factor level with fewer rows than
the threshold, counted unweighted. A flag makes
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
return `needs_raw_data`. Tests with no groups, such as the one-sample
t-test, state that only the sample size was checked.

### A test with a missing number: the paired t-test

For a paired design the per-condition means and SDs are not enough: the
SD of the differences depends on the correlation between the two
conditions. That correlation is part of the sufficient set (block B of
the appendix).

``` r

wide <- data.frame(drug1 = sleep$extra[sleep$group == 1],
                   drug2 = sleep$extra[sleep$group == 2])
sp <- sufficient_stats(Pair(drug1, drug2) ~ 1, data = wide, test = "paired")
sp
#> <reprokit sufficient_stats>  analysis: paired-samples t-test  (input: formula)
#> Verdict: summaries_suffice
#> Because: Summary statistics suffice: n, M and SD per condition, and r between the conditions determine the reported results exactly.
#> To publish: n, M and SD per condition, and r between the conditions (appendix() writes this out).
#> Disclosure check: passed (sample size only (no groups or cells; no individual values stored), threshold 5).
#> What readers lose compared with the raw data:
#>   * checking the distribution and outliers of the differences
#>   * nonparametric or robust alternatives
#> Statistics stored: n, mean, sd, r
```

``` r

appendix(sp, choices = list(convention = "d_z"),
         sharing_reason = "Consent did not cover public sharing.")
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

To see which tests are supported, what each needs, and what each leaves
uncheckable, use the table generated from the package’s **registry**,
the internal list that records, for each supported test, how to store,
reproduce and check it:

``` r

knitr::kable(coverage_table()[, c("test", "sufficient", "missing")])
```

| test | sufficient | missing |
|:---|:---|:---|
| Pearson correlation test | n, r | — |
| Paired-samples t-test | n, M, SD per condition, r | r (the SD of the differences depends on the correlation between conditions; per-condition M and SD do not determine it) |
| Moderation | n, M, SD per variable, the correlations, the X·Z moments | the correlations (coefficients depend on how the predictors overlap); the X·Z moments (cannot be derived from X and Z summaries) |
| Multiple regression | n, M, SD per variable, the correlations | the correlations (coefficients depend on how the predictors overlap) |
| Simple mediation | n, M, SD and correlations of X, M, Y | the correlations of X, M, Y |
| Simple regression | n, M, SD per variable, r | — |
| Chi-squared goodness of fit | the observed counts, H0 proportions | H0 proportions (the expected counts depend on them; not needed when they are equal) |
| Chi-squared test of independence | the contingency table | — |
| Factorial ANOVA, two factors (between-subjects) | n, M, SD per condition | — |
| Independent-samples t-test (Student, Welch) | n, M, SD per condition | — |
| One-sample t-test | n, M, SD | — |
| One-way ANOVA (between-subjects) | n, M, SD per condition | — |

## For readers: do the reported numbers agree?

Suppose a paper reports an independent-samples t-test: two groups of 10,
means 0.75 and 2.33, SDs 1.79 and 2.00, Welch’s *t*(17.8) = -1.86, *p* =
.079.

Pass the numbers as **strings**, so the decimals count. A reported
“2.85” means anything from 2.845 to 2.855, and reprokit checks against
that whole **rounding interval**, not against the single number 2.85.

``` r

check_reported("welch",
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

The first argument names the test (see
[`?check_reported`](https://dm-k1.github.io/reprokit/reference/check_reported.md);
the valid ones are `"student"`, `"welch"`, `"one_sample"`, `"paired"`,
`"pearson"`, `"oneway"`, `"factorial"`, `"chisq"`, `"gof"`,
`"mediation"`, `"lm"`, `"lm_simple"` and `"lm_moderation"`). A
**handler** is the part of reprokit that knows one test (for example the
one behind `"welch"`), and a **relation** is one formula that links
reported numbers, such as p from t and df. How to read the output:

- The first line is the answer: the numbers are consistent or
  inconsistent with each other.
- The **level** (0 to 3) says how much could be checked;
  [`vignette("concepts")`](https://dm-k1.github.io/reprokit/articles/concepts.md)
  defines each level.
- “Variants that fit” lists the analysis choices that explain the
  numbers (tails, confidence level, which test, effect-size convention).
  A paper rarely declares all of them, so reprokit tries each and keeps
  those that work. All numbers in one result must share one choice.
- In the table, each reported number gets a status:
  - `verified`: it is an output (such as t or df) that was recomputed
    from more basic numbers (n, means, SDs) and matches.
  - `coherent`: it fits the others, but only as an input to a check, or
    through numbers derived from other numbers. For example r, df and p
    give t, which is compared with p, so r is coherent even though t was
    not reported.
  - `inconsistent`: it is involved in a relation that fails under every
    variant, or in a set of relations that no single choice of test,
    level and tails satisfies at once.
  - `unconfirmed`: it belongs to a relation that could not be evaluated
    because another input is missing; `checked_against` names what is
    needed.
  - `not reproducible`: an effect size that no convention reproduces.
    This is a remark, not a conflict, because the convention is rarely
    stated.
  - `not checkable`: no relation of this test reaches the number.

### An inconsistent set

``` r

check_reported("paired", n = "20", t = "2.85", df = "19", p = ".050")
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

Here the test is a paired t-test. With t(19) = 2.85, the two-sided p is
about .010, not .050, so no way of rounding makes these numbers agree.

This does not say which number is wrong, or that the paper is. It could
be a typo, a one-sided test reported as two-sided, a different df, or a
different analysis. The check cannot tell which; only the authors or the
raw data can. If the numbers merely cannot be tested, you get a lower
level or a status such as `unconfirmed`, never an accusation.

### Missing numbers and implied ranges

If a number is left out, but the others constrain it, reprokit gives the
range it must have been in.

``` r

check_reported("paired", n = "10", m = c("0.75", "2.33"), sd = c("1.79", "2.00"),
               t = "-4.06", df = "9", p = ".003")
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Variants that fit: two-sided
#> 
#>  quantity   reported      status               checked_against
#>         n         10    coherent                     df_from_n
#>         m 0.75, 2.33 unconfirmed t_from_descriptives (needs r)
#>        sd 1.79, 2.00 unconfirmed t_from_descriptives (needs r)
#>         t      -4.06    coherent                      p_from_t
#>        df          9    verified           df_from_n; p_from_t
#>         p       .003    coherent                      p_from_t
#> 
#> Implied ranges for numbers that were not reported:
#>   r: 0.7912 to 0.7982
#> 
#> Reporting these would raise the level: r (to level 3).
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

Here the paired t-test needs the correlation r between conditions. It
was not reported, but t and the descriptives imply that it lies between
0.7912 and 0.7982. The message “Reporting these would raise the level”
says what to ask the authors for.

### Derived quantities, and correlations

When n is not reported but df is, n is derived from df (for correlations
and for paired and one-sample t-tests) and shown as such.

``` r

check_reported("pearson", r = ".41", df = "28", p = ".024")
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Variants that fit: two-sided
#> 
#>  quantity reported   status         checked_against
#>         r      .41 coherent p_from_t (via t_from_r)
#>        df       28 coherent p_from_t (via t_from_r)
#>         p     .024 verified                p_from_t
#> 
#> Derived from the reported numbers: n = 30 (from df).
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

For a one-sample test, give the test value with `mu` if it is not 0.
`mu` is a plain number, not a string:

``` r

check_reported("one_sample", n = "32", m = "20.09", sd = "6.03",
               t = "0.08", df = "31", p = ".94", mu = 20)
#> The analysis is consistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Variants that fit: two-sided
#> 
#>  quantity reported   status                checked_against
#>         n       32 coherent df_from_n; t_from_descriptives
#>         m    20.09 coherent            t_from_descriptives
#>        sd     6.03 coherent            t_from_descriptives
#>         t     0.08 verified  t_from_descriptives; p_from_t
#>        df       31 verified            df_from_n; p_from_t
#>         p      .94 coherent                       p_from_t
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

### Restricting the variants

If you know the analysis was one-sided, or Welch, say so with
`variants`, and the check only considers those. Here the one-sample
numbers above fit a two-sided test only, so restricting to a one-sided
test makes them inconsistent:

``` r

check_reported("one_sample", n = "32", m = "20.09", sd = "6.03",
               t = "0.08", df = "31", p = ".94", mu = 20, variants = "greater")
#> The analysis is inconsistent with the reported statistics.
#> Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
#> Variants that fit: none
#> 
#>  quantity reported       status                checked_against
#>         n       32     coherent df_from_n; t_from_descriptives
#>         m    20.09     coherent            t_from_descriptives
#>        sd     6.03     coherent            t_from_descriptives
#>         t     0.08 inconsistent  t_from_descriptives; p_from_t
#>        df       31 inconsistent            df_from_n; p_from_t
#>         p      .94 inconsistent                       p_from_t
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

The values `variants` accepts are codes such as `"welch"`,
`"two.sided"`, `"greater"` or `"0.99"`; the output shows readable labels
instead (“Welch’s t”, “one-sided (greater)”, “99%”). A value the handler
does not have is an error that lists the ones it has. See
[`?check_reported`](https://dm-k1.github.io/reprokit/reference/check_reported.md).

### A one-way ANOVA

`check_reported("oneway", ...)` takes per-group n, mean and SD and the
reported F (lowercase `f`), both degrees of freedom, eta squared and
p. These are the PlantGrowth data:

``` r

check_reported("oneway", n = c("10", "10", "10"), m = c("5.03", "4.66", "5.53"),
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

### A chi-squared test

`check_reported("chisq", ...)` takes the table as a matrix (a vector for
goodness of fit, with `p0` if the H0 proportions are not equal) and the
reported chi-squared, df, p and V (use `"gof"` for goodness of fit). A 2
x 2 table has Yates’ correction as a variant; V is a convention.

``` r

check_reported("chisq", n = matrix(c("30", "10", "20", "40"), 2), chisq = "15.04",
               df = "1", p = ".0001", v = ".41")
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

### A two-factor ANOVA

For a factorial design, pass the cell n, M and SD as matrices (rows are
the levels of A, columns the levels of B) and name the `effect`: `"A"`,
`"B"` or `"AB"`. In an unbalanced design the sums of squares type (III,
II or I) is a choice the report must settle, and the output lists the
types that fit.

``` r

cells <- matrix(rep("10", 4), 2)
check_reported("factorial", n = cells, m = matrix(c("4.10", "5.20", "4.80", "7.40"), 2),
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

### A regression

`check_reported("lm", ...)` takes one named value per coefficient. Here
are the numbers of `mpg ~ wt + hp` in `mtcars` (n = 32); the check tests
t = b / SE, p from t and the residual df, F from R squared and the df,
and so on.

``` r

check_reported("lm", b = c(wt = "-3.88", hp = "-0.03"), se = c(wt = "0.63", hp = "0.01"),
               t = c(wt = "-6.13", hp = "-3.52"), p = c(wt = "< .001", hp = ".001"),
               r2 = ".83", f = "69.21", df1 = "2", df2 = "29", n = "32")
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Effect-size conventions that fit: R²
#> 
#>  quantity reported   status                                 checked_against
#>      b:wt    -3.88 coherent                                  t_from_b_se:wt
#>      b:hp    -0.03 coherent                                  t_from_b_se:hp
#>     se:wt     0.63 coherent                                  t_from_b_se:wt
#>     se:hp     0.01 coherent                                  t_from_b_se:hp
#>      t:wt    -6.13 coherent                     t_from_b_se:wt; p_from_t:wt
#>      t:hp    -3.52 coherent                     t_from_b_se:hp; p_from_t:hp
#>      p:wt   < .001 coherent                                     p_from_t:wt
#>      p:hp     .001 coherent                                     p_from_t:hp
#>        r2      .83 coherent                                       f_from_r2
#>         f    69.21 coherent                                       f_from_r2
#>       df1        2 coherent                                       f_from_r2
#>       df2       29 verified p_from_t:wt; p_from_t:hp; f_from_r2; df2_from_n
#>         n       32 coherent                                      df2_from_n
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

Use `"lm_simple"` or `"lm_moderation"` as the test for those designs.

### A simple mediation

`check_reported("mediation", ...)` checks the four paths (a, b, c and
c’, each a regression coefficient). The reported indirect effect is
checked against the product of the rounding intervals of a and b. If it
falls outside, the result says the indirect effect is “not coherent”
with a and b (a bootstrap mean can differ a little from the product, and
the output says so). The indirect effect’s confidence interval is not
checkable: a bootstrap resamples cases, and summaries cannot.

``` r

check_reported(
  "mediation",
  b = c(a = "46.16", b = "-0.03", c = "-5.34", c_prime = "-3.88"),
  se = c(a = "9.63", b = "0.01", c = "0.56", c_prime = "0.63"),
  t = c(a = "4.80", b = "-3.52", c = "-9.56", c_prime = "-6.13"),
  df = c(a = "30", b = "29", c = "30", c_prime = "29"),
  ci_lower = c(a = "26.50", b = "-0.05", c = "-6.49", c_prime = "-5.17"),
  ci_upper = c(a = "65.82", b = "-0.01", c = "-4.20", c_prime = "-2.58"),
  n = "32", indirect = "-1.46",
  indirect_ci_lower = "-2.28", indirect_ci_upper = "-0.80"
)
#> The analysis is consistent with the reported statistics.
#> Level 1: internal coherence only (statistic, df, p and CI are checked against each other, not against the descriptives).
#> Confidence levels that fit: 95%
#> 
#>           quantity reported        status
#>                b:a    46.16      coherent
#>                b:b    -0.03      coherent
#>                b:c    -5.34      coherent
#>          b:c_prime    -3.88      coherent
#>               se:a     9.63      coherent
#>               se:b     0.01      coherent
#>               se:c     0.56      coherent
#>         se:c_prime     0.63      coherent
#>                t:a     4.80      coherent
#>                t:b    -3.52      coherent
#>                t:c    -9.56      coherent
#>          t:c_prime    -6.13      coherent
#>               df:a       30      verified
#>               df:b       29      verified
#>               df:c       30      verified
#>         df:c_prime       29      verified
#>         ci_lower:a    26.50      coherent
#>         ci_lower:b    -0.05      coherent
#>         ci_lower:c    -6.49      coherent
#>   ci_lower:c_prime    -5.17      coherent
#>         ci_upper:a    65.82      coherent
#>         ci_upper:b    -0.01      coherent
#>         ci_upper:c    -4.20      coherent
#>   ci_upper:c_prime    -2.58      coherent
#>                  n       32      coherent
#>           indirect    -1.46      coherent
#>  indirect_ci_lower    -2.28 not checkable
#>  indirect_ci_upper    -0.80 not checkable
#>                                                              checked_against
#>  t_from_b_se:a; ci_lower_from_b_se:a; ci_upper_from_b_se:a; indirect_from_ab
#>  t_from_b_se:b; ci_lower_from_b_se:b; ci_upper_from_b_se:b; indirect_from_ab
#>                    t_from_b_se:c; ci_lower_from_b_se:c; ci_upper_from_b_se:c
#>  t_from_b_se:c_prime; ci_lower_from_b_se:c_prime; ci_upper_from_b_se:c_prime
#>                    t_from_b_se:a; ci_lower_from_b_se:a; ci_upper_from_b_se:a
#>                    t_from_b_se:b; ci_lower_from_b_se:b; ci_upper_from_b_se:b
#>                    t_from_b_se:c; ci_lower_from_b_se:c; ci_upper_from_b_se:c
#>  t_from_b_se:c_prime; ci_lower_from_b_se:c_prime; ci_upper_from_b_se:c_prime
#>                                                                t_from_b_se:a
#>                                                                t_from_b_se:b
#>                                                                t_from_b_se:c
#>                                                          t_from_b_se:c_prime
#>                      df_from_n:a; ci_lower_from_b_se:a; ci_upper_from_b_se:a
#>                      df_from_n:b; ci_lower_from_b_se:b; ci_upper_from_b_se:b
#>                      df_from_n:c; ci_lower_from_b_se:c; ci_upper_from_b_se:c
#>    df_from_n:c_prime; ci_lower_from_b_se:c_prime; ci_upper_from_b_se:c_prime
#>                                                         ci_lower_from_b_se:a
#>                                                         ci_lower_from_b_se:b
#>                                                         ci_lower_from_b_se:c
#>                                                   ci_lower_from_b_se:c_prime
#>                                                         ci_upper_from_b_se:a
#>                                                         ci_upper_from_b_se:b
#>                                                         ci_upper_from_b_se:c
#>                                                   ci_upper_from_b_se:c_prime
#>                     df_from_n:a; df_from_n:b; df_from_n:c; df_from_n:c_prime
#>                                                             indirect_from_ab
#>                                                                            -
#>                                                                            -
#> 
#> Note: The confidence interval of the indirect effect is not checkable: it is inferred by bootstrap, which resamples cases; summaries cannot. No Sobel, Aroian or Goodman test is computed.
#> 
#> An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
```

[`quantity_names()`](https://dm-k1.github.io/reprokit/reference/quantity_names.md)
lists the names you can pass.

## Limits

- The check tests agreement between reported numbers, not whether the
  analysis was appropriate or the data plausible. Agreeing numbers can
  still be wrong.
- Only the tests in
  [`coverage_table()`](https://dm-k1.github.io/reprokit/reference/coverage_table.md)
  are covered: chi-squared, t (one-sample, independent, paired),
  Pearson, one-way and factorial ANOVA, and the regression family
  (simple, multiple, moderation, simple mediation). Everything else gets
  a verdict only.
- The check is conservative: ambiguity resolves towards silence, because
  flagging a correct paper would be a false accusation.
- The disclosure check lowers the risk of identifying a participant; it
  never removes it and is never a guarantee.
- The share half states what readers lose and records the reason you
  give, if any; it cannot judge whether the reason is good enough.
