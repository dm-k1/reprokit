# check_reported(): do the numbers a paper reports agree with each other?
# In one line: parse the reported numbers into rounding intervals, evaluate each
# of the handler's relations whose inputs are all reported, compare the derived
# range with the reported output, and summarise per number, per variant and as a
# coverage level (ADR 0010). It never says which number is wrong, only whether
# the set can be correct as reported.

#' Check reported statistics against each other
#'
#' Takes the numbers a paper reports for an analysis and tests whether they can
#' all be right at once, given how precisely they were reported ("2.85" means
#' anything from 2.845 to 2.855). Where one input is missing but the output is
#' reported, it gives the range the missing input must lie in.
#'
#' @param test The test the numbers come from, in the vocabulary of [sufficient_stats()]:
#'   `"student"`, `"welch"` (two independent groups; the name fixes the variant),
#'   `"one_sample"`, `"paired"`, `"pearson"`, `"oneway"`, `"factorial"`, `"chisq"`,
#'   `"gof"`, `"mediation"` and `"lm"` (a multiple regression; `"lm_simple"` and
#'   `"lm_moderation"` for a simple regression and a moderation analysis). It can be given by name
#'   or as the first unnamed argument. Any other
#'   value is refused with the list of valid ones.
#' @param ... Reported numbers, named by quantity (see [quantity_names()]).
#'   Pass strings, so the decimals count: `t = "2.85"`, `p = "< .001"`,
#'   `df = "29"`. A quantity with one value per group or condition is a
#'   character vector (`m = c("5.1", "4.2")`). Integer-valued numbers are taken
#'   as exact.
#'
#'   Each test accepts the quantities that apply to it, and no others. A name
#'   reprokit knows that does not apply to the test (`d` for a correlation, `r2` for a
#'   chi-squared test) is an input error that lists the names the test takes, and an
#'   unknown name is an error too. Besides the numbers of the APA set, the tests take
#'   the R squared family where it exists: `r2` and `adj_r2` for a correlation
#'   (`r2` is `r` squared), `eta2`, `r2` and `omega2` for the t tests, `eta2`, `r2`
#'   (identical to `eta2`) and `omega2` for a one-way ANOVA, `partial_eta2` and
#'   `partial_omega2` per effect for a factorial ANOVA, `r2`, `adj_r2` and the model
#'   `F` for a regression, and `r2` and `F` per model for a mediation. Where the
#'   convention behind a number is not stated in a paper (omega squared, a Cohen's d),
#'   a number that does not match is "not reproducible", never "inconsistent".
#'
#'   For `"mediation"` (a simple, regression-based mediation, as in PROCESS model 4),
#'   a quantity with one value per path is a character vector named by path, `a`
#'   (X to M), `b` (M to Y), `c` (X to Y) and `c_prime` (X to Y with M held constant;
#'   `c'` is accepted): `b`, `se`, `t`, `p`, `df`, `ci_lower`, `ci_upper` and `beta`
#'   (for example `ci_lower = c(a = "26.50", b = "-0.05")`). The coefficient of
#'   a path is `b`, so `b = c(a = "46.16", b = "-0.03")` is the path a and the path b.
#'   `n` is a single string. The indirect effect is `indirect`; its bootstrap interval
#'   (`indirect_ci_lower`, `indirect_ci_upper`) is accepted but cannot be checked.
#'   When a, b and `indirect` are all reported, `indirect` is checked for coherence
#'   with a times b: it must lie in the product of the rounding intervals of a and b.
#'   This is arithmetic on the printed numbers and says nothing about significance; a
#'   bootstrap mean reported instead of the sample a times b is one possible reason
#'   for a mismatch. Two further arguments are read from `...` for a mediation:
#'   `indirect_type` (`"unstandardized"`, `"standardized"` or `"partially_standardized"`;
#'   a standardized indirect effect is only checked with `beta` for a and b, a
#'   partially standardized one is not evaluable) and `estimator` (`"sem"`, `"ml"` or
#'   `"lavaan"` for numbers from an ML/SEM fit, which are not evaluable as a regression).
#' @param variants Character vector restricting which variants to try (such as
#'   `"two.sided"`, `"0.95"` or `"welch"`), or `NULL` for all. Each value
#'   restricts only its own kind of variant (tail, confidence level, test,
#'   convention): `c("welch", "0.95")` fixes the test and the level. A kind
#'   that no value names keeps all its variants. The accepted values are the
#'   codes below; the printed output shows a readable label instead. A value
#'   the handler does not have is an error that lists the ones it has.
#'
#'   * Tail: `"two.sided"`, `"less"`, `"greater"` (shown as two-sided,
#'     one-sided (less), one-sided (greater)).
#'   * Level: `"0.90"`, `"0.95"`, `"0.99"` (shown as 90%, 95%, 99%).
#'   * Test: `"student"`, `"welch"` (shown as Student's t, Welch's t).
#'   * SS type (factorial ANOVA, unbalanced designs): `"III"`, `"II"`, `"I"` (shown as
#'     type III, type II, type I).
#'   * Effect-size convention: `"pooled"` (Cohen's d, pooled SD), `"hedges_g"`,
#'     `"average"` (Cohen's d, root mean square of the SDs), `"glass_sd1"`,
#'     `"glass_sd2"` (Glass's delta), `"cohen"`, `"d_z"`, `"d_rm"`, `"d_av"`,
#'     `"g_av"`, `"eta2"` (eta squared), `"omega2"` (omega squared),
#'     `"partial_eta2"`, `"partial_omega2"` (factorial ANOVA). Which of
#'     them a handler has depends on the test. For the t tests, `"eta2"` and
#'     `"omega2"` are computed from t and df.
#' @param mu The test value the mean (or mean difference) is compared with, for
#'   tests whose t depends on it (`"one_sample"`, `"paired"`). A plain
#'   number, not a string (`mu = 20`, not `mu = "20"`), unlike the reported
#'   numbers. Default 0. A test without a test value errors on `mu` other
#'   than 0.
#' @param k For the regression tests (`"lm"` and the like), the number of predictor
#'   columns (the model's df1), when F is not reported to say it. With `k`, `n`
#'   and `df2` are checked against each other (df2 = n - k - 1) and either gives
#'   the other; without it they are not, because k is never guessed from the
#'   number of coefficients reported (a paper may leave some out). A plain number.
#' @param dims For `"chisq"`, `"gof"` and `"factorial"`, the table shape: `c(rows, columns)`
#'   for a test of independence, the number of categories for goodness of fit, the
#'   levels of each factor for a factorial design. Not needed when `n` is a matrix (its
#'   `dim()` is read before the counts are parsed). Without a shape `df` cannot be
#'   checked, because a flat vector of counts does not say whether 12 cells are 2 x 6
#'   or 3 x 4.
#' @param p0 For `"gof"`, the H0 proportions, one per category, summing to 1 (within
#'   1e-8). Without them the categories are taken as equally likely.
#' @param effect For `"factorial"`, the effect to check: `"A"` (the factor in the rows
#'   of the cell matrices), `"B"` (the columns) or `"AB"` (the interaction). A paper
#'   with three F tests gets three calls with the same cell descriptives.
#' @return A `reprokit_check` object: `$numbers` (status of each reported
#'   number), `$level` (coverage, 0 to 3), `$missing` (what would raise the
#'   level, and the range implied for it: one row per number that raises the level
#'   on its own, and, when none does, one row with `together = TRUE` whose
#'   `quantity` lists the smallest set that raises it jointly, such as `"m, sd, r"`),
#'   `$variants` (a list with one data
#'   frame per kind of variant the handler has, each of the variants tried and
#'   whether they fit: `tail`, `level`, `test` (Student or Welch) and
#'   `convention` (how an effect size was computed)), `$relations` (one row per
#'   relation and variant; `$variant` and `$kind` join the values and kinds of a
#'   relation that varies over several with "+", and there is one column per
#'   kind, `tail`, `level`, `test`, `convention`, holding the row's value or
#'   `NA`), `$implied`,
#'   `$implied_inputs` (numbers derived from reported ones, such as `n` from `df`)
#'   and `$checked` (whether any relation that can falsify was compared with a
#'   reported number; without one there is no verdict).
#'
#'   The reported numbers come from one analysis, so they share one test and one
#'   confidence level: a variant fits only if one choice per kind explains every
#'   relation that can falsify. A CI whose bounds fit different levels, or a t that
#'   fits only Student next to a CI that fits only Welch, is inconsistent even
#'   though each relation holds on its own.
#'
#'   Some relations cannot falsify: how `d` was computed (pooled SD, average SD,
#'   Glass) is rarely declared, so a `d` that no convention reproduces gets the
#'   status `"not reproducible"`, never `"inconsistent"`, and `$consistent` is
#'   not affected.
#'
#'   A reported number is only ever compared within what it can be: `r` lies
#'   in -1 to 1, `sd` and `df` are not negative, `p` is in 0 to 1, `n` is a
#'   whole number. A relation whose result is not finite for the reported values
#'   (`"1.00"` for `r` gives an infinite t) is skipped, as if an input were
#'   missing; it never counts as a failure.
#'
#'   When `n` is not reported but `df` is, `n` is derived from `df` for the
#'   handlers where that is exact (a correlation, a paired or one-sample t): it
#'   is used by the other relations, shown as `$implied_inputs`, and counts for
#'   the coverage level, but `df` is never checked against it.
#' @export
#' @examples
#' check_reported("paired", n = "20", t = "2.85", df = "19", p = ".010")
#'
#' # A 2 x 2 table: Yates' correction is a variant, V is a convention
#' check_reported("chisq", n = matrix(c("30", "10", "20", "40"), 2), chisq = "15.04",
#'                df = "1", p = ".0001", v = ".41")
#'
#' # A regression reports a number per coefficient: name them.
#' # A simple mediation (mpg ~ wt through hp): the paths, and the indirect effect
#' # checked for coherence with a times b
#' check_reported("mediation",
#'                b = c(a = "46.16", b = "-0.03", c = "-5.34", c_prime = "-3.88"),
#'                se = c(a = "9.63", b = "0.01", c = "0.56", c_prime = "0.63"),
#'                t = c(a = "4.80", b = "-3.52", c = "-9.56", c_prime = "-6.13"),
#'                n = "32", indirect = "-1.46")
#'
#' # A regression reports a number per coefficient: name them.
#' check_reported("lm", b = c(wt = "-3.88", hp = "-0.03"), se = c(wt = "0.63", hp = "0.01"),
#'                t = c(wt = "-6.13", hp = "-3.52"), p = c(wt = "< .001", hp = ".001"),
#'                r2 = ".83", f = "69.21", df1 = "2", df2 = "29", n = "32")
check_reported <- function(..., test, variants = NULL, mu = 0, dims = NULL, p0 = NULL,
                           effect = NULL, k = NULL) {
  tests <- check_tests()
  dots <- list(...)
  if (missing(test)) {
    # The first unnamed argument is the test. `test` sits after `...`, so a short quantity
    # name (t, r, p, n) can never partially match it.
    nm <- names(dots)
    unnamed <- if (is.null(nm)) seq_along(dots) else which(!nzchar(nm))
    if (length(unnamed) > 0L && is.character(dots[[unnamed[1L]]]) &&
          length(dots[[unnamed[1L]]]) == 1L && !is.na(dots[[unnamed[1L]]])) {
      test <- dots[[unnamed[1L]]]
      dots <- dots[-unnamed[1L]]
    } else {
      abort_input(
        "Name the test first: check_reported(\"paired\", t = ...). Valid tests: ",
        paste0("\"", names(tests), "\"", collapse = ", "), "."
      )
    }
  }
  if (!is.character(test) || length(test) != 1L || is.na(test) || !test %in% names(tests)) {
    abort_input(
      "`test` is ", if (is.character(test) && length(test) == 1L) paste0("\"", test, "\"") else
        "not a single test name", ", which is not a test reprokit knows. Valid values of `test`: ",
      paste0("\"", names(tests), "\"", collapse = ", "), ". Student and Welch are ",
      "`\"student\"` and `\"welch\"`."
    )
  }
  given <- c(mu = !(is.numeric(mu) && length(mu) == 1L && isTRUE(mu == 0)), dims = !is.null(dims),
             p0 = !is.null(p0), effect = !is.null(effect), k = !is.null(k))
  allowed <- switch(test,
                    one_sample = , paired = "mu", chisq = "dims", gof = c("dims", "p0"),
                    oneway = c("effect", "dims"), factorial = c("effect", "dims"),
                    lm = , lm_simple = , lm_moderation = "k", character())
  unused <- setdiff(names(given)[given], allowed)
  if (length(unused) > 0L) {
    abort_input("`", unused[1L], "` does not apply to test \"", test, "\".")
  }
  handler <- tests[[test]]
  run <- function(f, ...) do.call(f, c(dots, list(...)))
  switch(test,
    student = , welch = {
      other <- setdiff(c("student", "welch"), test)
      if (other %in% variants) {
        abort_input("`variants` names \"", other, "\" but `test` is \"", test, "\".")
      }
      run(check_handler, handler = handler, variants = c(variants, test))
    },
    one_sample = , paired = run(check_t, handler = handler, variants = variants, mu = mu),
    pearson = run(check_r, handler = handler, variants = variants),
    oneway = , factorial = run(check_F, handler = handler, variants = variants,
                               effect = effect, dims = dims),
    chisq = , gof = run(check_chisq, handler = handler, variants = variants, dims = dims,
                        p0 = p0),
    lm = , lm_simple = , lm_moderation = run(check_lm, handler = handler, variants = variants,
                                             k = k),
    mediation = run(check_mediation, variants = variants)
  )
}

format_implied_input <- function(found) {
  if (found$lower == found$upper) format(found$lower) else paste(found$lower, "to", found$upper)
}

level_words <- function(level) {
  words <- c(
    "Level 0: nothing could be checked with the reported numbers.",
    paste("Level 1: internal coherence only (statistic, df, p and CI are checked",
          "against each other, not against the descriptives)."),
    "Level 2: the test statistic was recomputed from the descriptives.",
    paste("Level 3: the sufficient set is reported (the numbers contain",
          "everything needed to reproduce the analysis; that is not itself a check).")
  )
  words[level + 1L]
}

#' @export
print.reprokit_check <- function(x, ...) {
  if (!x$checked) {
    cat("No check was possible with the reported statistics.\n")
  } else if (x$consistent) {
    cat("The analysis is consistent with the reported statistics.\n")
  } else {
    cat("The analysis is inconsistent with the reported statistics.\n")
  }
  cat(level_words(x$level), "\n", sep = "")
  for (kind in names(x$variants)) {
    tried <- x$variants[[kind]]
    if (nrow(tried) == 0L) next
    fitting <- tried$variant[tried$fits]
    fitting_text <- if (length(fitting) > 0L) {
      paste(variant_label(fitting), collapse = ", ")
    } else {
      "none"
    }
    kind_label <- variant_kind_labels()[[kind]]
    if (kind == "test" && all(tried$variant %in% ss_type_codes())) kind_label <- "SS types that fit"
    cat(kind_label, ": ", fitting_text, "\n", sep = "")
  }
  if (nrow(x$numbers) > 0L) {
    cat("\n")
    print(x$numbers, row.names = FALSE, na.print = "-")
  }
  for (note in x$notes) cat("\nNote: ", note, "\n", sep = "")
  if (length(x$implied) > 0L) {
    cat("\nImplied ranges for numbers that were not reported:\n")
    for (quantity in names(x$implied)) {
      cat("  ", quantity, ": ", format_implied(x$implied[[quantity]]), "\n", sep = "")
    }
  }
  if (length(x$implied_inputs) > 0L) {
    values <- vapply(x$implied_inputs, format_implied_input, character(1))
    sources <- vapply(x$implied_inputs, `[[`, character(1), "from")
    cat("\nDerived from the reported numbers: ",
        paste0(names(x$implied_inputs), " = ", values, " (from ", sources, ")",
               collapse = ", "), ".\n", sep = "")
  }
  if (any(x$numbers$status == "inconsistent")) {
    cat("\nAn inconsistency shows the numbers cannot all be correct as reported, ",
        "not which one is wrong or why.\n", sep = "")
  }
  # One closing line: what to report to raise the level.
  if (nrow(x$missing) > 0L) {
    single <- !x$missing$together
    parts <- character(0)
    if (any(single)) {
      parts <- paste0(x$missing$quantity[single], " (to level ",
                      x$missing$would_reach_level[single], ")", collapse = ", ")
    }
    for (i in which(x$missing$together)) {
      set <- strsplit(x$missing$quantity[i], ", ", fixed = TRUE)[[1L]]
      listed <- set
      if (length(set) > 1L) {
        listed <- paste0(paste(set[-length(set)], collapse = ", "), " and ", set[length(set)])
      }
      parts <- c(parts, paste0(listed, " (together, to level ",
                               x$missing$would_reach_level[i], ")"))
    }
    cat("Also report ", paste(parts, collapse = ", "), " to make this fully checkable.\n", sep = "")
  }
  invisible(x)
}
