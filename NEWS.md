# reprokit (development version)

First development version. Not yet on CRAN; names and output may change.

## Share: for authors who cannot share raw data

* `triage()` returns one of three verdicts (`summaries_suffice`,
  `synthetic_possible`, `needs_raw_data`) and never errors.
* `sufficient_stats()` stores the sufficient statistics at full precision. It
  requires a reason for not sharing and a non-empty list of what readers lose.
  Requests that summaries cannot meet stop with an error of class
  `reprokit_impossible` that lists the alternatives.
* `reproduce()` recomputes the result from the stored statistics alone.
* New handler `anova_oneway` (`y ~ group`, `test = "oneway"`) stores n, mean
  and SD per group and reproduces F, df, p and eta squared.
* New handlers `chisq_independence` (`~ a + b`, `test = "chisq"`) and
  `chisq_gof` (`~ a`, `test = "gof"`, optional `p0`) store the counts and
  reproduce chi-squared, df, p, N and V (w). Low expected counts are listed as
  lost, not checked.
* New handler `anova_factorial` (`y ~ a * b`, `test = "factorial"`) for
  two-factor between-subjects designs: stores n, mean and SD per cell and
  reproduces F, df, p and partial eta squared for A, B and AB. The pooled error
  term (equal variances) is listed as lost. The sums of squares type is a
  declared choice in unbalanced designs; `appendix()` asks for it.
* `appendix()` writes a Markdown appendix: the numbers APA style asks for, the
  numbers it does not but the result needs, the analysis choices, the numbers a
  reader cannot check, why the data are not shared, what readers lose, and a
  disclosure check on small groups.

## Check: for readers of a paper

* `check_reported()`, with the functions `check_t()`, `check_r()` and
  `check_F()`, tests whether reported numbers are consistent with each other.
  Numbers are given as strings, so their decimals define rounding intervals.
  The output gives a verdict, a coverage level (0 to 3), a status for each
  number, the analysis variants that fit, and the ranges implied for numbers
  that were not reported. An inconsistency never says which number is wrong.
* `check_r()` does not report r as "not checkable" when it reaches p only
  through an unreported t (for example `check_r(r = ".41", df = "28", p = ".024")`).

* `check_chisq()` checks a chi-squared test of independence or goodness of fit
  from the table of counts (a matrix, or a vector for goodness of fit) and the
  reported chi-squared, df, p, N and Cramer's V (Cohen's w for goodness of fit).
  The table shape (`dims`) and the H0 proportions (`p0`) are test arguments:
  df is checked only when the shape is known. Yates' correction is a variant
  for a 2 x 2 table, shared by chi-squared and p; V and w are conventions that
  never make a report inconsistent, and a reported phi is compared by absolute
  value. An expected count of 0 is not evaluable.
* `check_reported()` takes the test arguments `dims` and `p0` next to `mu`; a
  handler rejects one it does not declare. It also takes `effect` (`"A"`, `"B"`
  or `"AB"`) for factorial designs.
* `check_F()` checks a factorial effect from the cell matrices. The sums of
  squares type (III, II, I) is a variant shared by F, p and partial eta squared;
  a balanced design has none. Partial eta squared and partial omega squared are
  conventions. Exact F ranges are used for one-df effects and balanced main
  effects; other effects are checked for coherence only.

## Coverage

* Independent-samples t-test (Student and Welch), paired-samples t-test,
  one-sample t-test, Pearson correlation, one-way and two-factor
  between-subjects ANOVA (`check_F()`), and chi-squared tests of independence and
  goodness of fit (`check_chisq()`). The regression family is planned.
* `coverage_table()` lists, per test, what APA style asks to report, what the
  result needs, what is missing and why, what can be checked, and which choices
  must be declared. It is generated from the registry.

## Documentation

* New vignettes: `vignette("reprokit")` (getting started),
  `vignette("concepts")` (terms) and `vignette("validation")` (evidence).
* The package help page (`?reprokit`) gives an overview of both directions.
