# reprokit 0.1.0

First release (lifecycle: experimental). Not on CRAN; names and output may change.

## Share: for authors who cannot share raw data

* `sufficient_stats(x, ..., threshold = 5)` says whether summary statistics make an
  analysis reproducible without the raw data, what to publish and what readers lose,
  and stores the summaries at full precision. It never errors on the verdict
  (`summaries_suffice`, `synthetic_possible` or `needs_raw_data`); the printed object
  narrates the verdict, what to publish, what readers lose and which group or cell a
  disclosure flag fired for. Only invalid input is an error of class
  `reprokit_impossible`.
* What readers lose is stated by reprokit, from the test (`sufficient_stats(...)$lost`);
  it is not an argument.
* `appendix(x, ..., sharing_reason, choices = list())` writes a Markdown appendix: the
  numbers APA style asks for, the numbers it does not but the result needs, the
  analysis choices, the numbers a reader cannot check, why the data are not shared,
  what readers lose, and a disclosure check. Underneath, it reproduces the result from
  the stored statistics and refuses if it does not match the original analysis.
  `sharing_reason` is optional and reprokit never suggests one: without it
  `appendix()` warns and the appendix says "Reason for not sharing the raw data: not
  stated by the author."
* Model input. `sufficient_stats()` and `appendix()` accept an `lm`, `aov` or
  `chisq.test()` result, and a `t.test()` or `cor.test()` result together with
  `data`. Choices recorded in a result (tails, confidence level, Student or Welch,
  test value, continuity correction, H0 proportions) are read from it and marked
  "read from the <fn> result". A result that does not match its data is refused, and
  so is a `data.name` that cannot be mapped to the data (with a hint to the formula
  route). A formula with `data` and `test` still works for any covered test.
* Mediation: `sufficient_stats(lm(M ~ X), lm(Y ~ X + M))` stores n, the means and the
  covariance of X, M and Y.
* Handlers (`handlers()`): `t_independent`, `t_one_sample`, `t_paired`,
  `cor_pearson`, `anova_oneway`, `anova_factorial`, `chisq_independence`,
  `chisq_gof`, `lm_simple`, `lm_multiple`, `lm_moderation`, `mediation_simple`, and a
  general `lm` handler that shares and reproduces but has no coverage row and no
  check route.
  - `anova_factorial` (two-factor between-subjects) stores n, mean and SD per cell.
    The pooled error term (equal variances) is listed as lost. The sums of squares
    type is a declared choice in unbalanced designs.
  - The chi-squared handlers store the counts; low expected counts are listed as
    lost, not checked.
  - `mediation_simple` lists the bootstrap CI of the indirect effect as lost, and
    gives the missing correlations of X, M and Y their own reason (the paths b and c'
    depend on how X and M overlap).
* Disclosure check per test. The default threshold is 5 and applies to
  groups, cells, counts and factor levels; the general minimum n stays 2 x threshold.
  - `chisq_independence` flags a cell and `chisq_gof` a category with
    0 < count < threshold. A count of 0 is not flagged: a zero can reveal an attribute of a whole group,
    but flagging it would also flag every sparse table.
  - `lm_multiple`, `lm_moderation` and the general `lm` flag every factor level with
    fewer than `threshold` rows in the fit, counted unweighted (rows with weight 0 are
    not in the fit). The check of 0/1 columns and their pairwise tables stays.
  - `t_one_sample`, `t_paired`, `cor_pearson` and `mediation_simple`, which have no
    groups or cells, say so, so the appendix always names what was checked ("sample
    size only (no groups or cells; no individual values stored)").
  - A flag drops the verdict to `needs_raw_data`. The check lowers the risk of
    identifying a participant; it is not a guarantee of anonymity.

## Check: for readers of a paper

* `check_reported(test, ...)` tests whether reported numbers are consistent with each
  other. `test` is one of `"student"`, `"welch"`, `"one_sample"`, `"paired"`,
  `"pearson"`, `"oneway"`, `"factorial"`, `"chisq"`, `"gof"`, `"mediation"`, `"lm"`,
  `"lm_simple"` or `"lm_moderation"`, given first or as `test =`. Numbers are given as
  strings, so their decimals define rounding intervals. The output gives a verdict, a
  coverage level (0 to 3), a status for each number, the analysis variants that fit,
  and the ranges implied for numbers that were not reported. An inconsistency never
  says which number is wrong.
* Pearson: r is not reported as "not checkable" when it reaches p only through an
  unreported t (for example `check_reported("pearson", r = ".41", df = "28", p = ".024")`).
* Chi-squared: the table of counts (a matrix, or a vector for goodness of fit) and the
  reported chi-squared, df, p, N and Cramer's V (Cohen's w for goodness of fit). The
  table shape (`dims`) and the H0 proportions (`p0`) are test arguments: df is checked
  only when the shape is known. Yates' correction is a variant for a 2 x 2 table,
  shared by chi-squared and p; V and w are conventions that never make a report
  inconsistent, and a reported phi is compared by absolute value. An expected count of
  0 is not evaluable.
* Test arguments: `mu`, `dims`, `p0`, `k`, `mediator`, and `effect` (`"A"`, `"B"` or
  `"AB"`) for factorial designs; a handler rejects one it does not declare.
* Factorial ANOVA: checked from the cell matrices. The sums of squares type (III, II,
  I) is a variant shared by F, p and partial eta squared; a balanced design has none.
  Partial eta squared and partial omega squared are conventions. Exact F ranges are
  used for one-df effects and balanced main effects; other effects are checked for
  coherence only.
* One-way ANOVA: a negative omega squared (F < 1) reported in the `eta2` slot is
  accepted; the `eta2` domain is [-1, 1], as for the factorial ANOVA.
* Regression (`"lm"`, `"lm_simple"`, `"lm_moderation"`): t = b / SE, p from t and the
  residual df, the CI at 90, 95 or 99 per cent, F from R squared with df1 and df2, p
  from F, and the n implied by df2 = n - k - 1. Per-term numbers are vectors named by
  term. k is a test argument or is read from df1; it is never guessed from the number
  of terms given, so without it n is not checkable. A simple regression adds
  R squared = r squared, t squared = F and r = b SD_x / SD_y. Adjusted R squared and
  beta are conventions that never make a report inconsistent.
* Mediation: the paths a, b, c and c' are checked as regression coefficients (t from b
  and SE, p from t and df, with df = n - 2 or n - 3). A reported CI of a path
  (`ci_lower`, `ci_upper`, named by path) is checked from b, SE, the path's df and the
  level (90, 95 or 99 per cent); it is optional. The reported indirect effect is
  checked against the product of the rounding intervals of a and b; if it falls
  outside, the result says "not coherent" (a bootstrap mean can differ a little from
  the product). The indirect effect's CI is not checkable, and no Sobel-type test is
  computed.
* `appendix()` for the regression rows asks for the confidence level of the intervals
  and, for the three lm rows, the R squared convention (R squared or adjusted R
  squared): `choices = list(level = "0.95", convention = "r2")`. Mediation asks for
  the level only. Both appear in block C, "not declared" when not given.

## Coverage

* Independent-samples t-test (Student and Welch), paired-samples t-test, one-sample
  t-test, Pearson correlation, one-way and two-factor between-subjects ANOVA,
  chi-squared tests of independence and goodness of fit, simple, multiple and
  moderation regression, and simple mediation.
* `coverage_table()` lists, per test, what APA style asks to report, what the result
  needs, what is missing and why, what can be checked, and which choices must be
  declared. It is generated from the registry. The coverage row for simple mediation
  reads: checkable "a, b, c, c' (b, SE, t, p, CI each)", sufficient "n, M, SD and
  correlations of X, M, Y", missing "the correlations of X, M, Y".
* `conventions()` lists the effect-size conventions (`choices$convention`) and
  sums-of-squares types (`choices$ss_type`) each handler accepts, with the default and
  the appendix label. It reads the registry that `appendix()` validates against.
* `appendix()` uses type III when a factorial ANOVA's SS type is not declared and says
  so: "type III (reprokit default, not stated by the author)", because a fitted `aov` does not record it. Checking a
  paper still tries III, II and I. An undeclared effect-size convention of a one-way
  ANOVA is shown as not declared.

## Documentation

* New vignettes: `vignette("reprokit")` (getting started),
  `vignette("concepts")` (terms) and `vignette("validation")` (evidence). The validation
  vignette reads the committed results of the scripts in `inst/validation/results/`, each
  stamped with its seed, the package version and the date.
* The README is generated from `README.Rmd`, with the coverage table rendered from
  `coverage_table()`.
* The package help page (`?reprokit`) gives an overview of both directions.
