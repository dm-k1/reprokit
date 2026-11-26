#' What can be shared for this analysis?
#'
#' `sufficient_stats()` is the author's verb. Given the analysis, it says whether
#' summary statistics can make it reproducible without the raw data, what to publish,
#' and what readers lose. When they do, it holds the summaries, stored at full
#' precision. The verdict never errors: "summaries do not suffice, because ..." is an
#' answer, and the printed object narrates it (the verdict and why, what to publish,
#' what is lost, and which group or cell a disclosure flag fired for). Only invalid
#' input is an error, of class `reprokit_error_input`: a formula without `data` or
#' `test`, an unknown `test`, a `t.test` or `cor.test` result without `data`, or an
#' explicit choice that conflicts with the result.
#'
#' `x` is one of:
#'
#' * a fitted `lm`, a `glm`, an `aov` (one factor, or two factors with their
#'   interaction; other designs get a verdict), or the result of `chisq.test()`. These
#'   hold what is needed; the choices recorded in a result (continuity correction,
#'   H0 proportions) are read from it. An `aov` fit does not record the type of sums
#'   of squares, so [appendix()] asks for it (`choices = list(ss_type = ...)`).
#' * the result of `t.test()` or `cor.test()` together with `data`. These do not hold
#'   the data, but they record Student or Welch, paired, one-sample, the tails, the
#'   confidence level and the test value. reprokit rebuilds the test from `data`
#'   and compares it with the result (statistic, df and p, scaled difference below
#'   1e-8). A result that does not follow from `data` (wrong data, a subset of rows) gets
#'   a verdict saying so. Spearman and Kendall are not supported tests.
#' * a formula with `data` and `test`, for the analysis the result objects above do not
#'   cover, or to be explicit: `y ~ group, data = d, test = "welch"`. `test` is one of
#'   `"student"`, `"welch"`, `"one_sample"`, `"paired"`, `"pearson"`, `"oneway"`,
#'   `"factorial"`, `"chisq"`, `"gof"` or `"mediation"`.
#' * two `lm` fits for a simple mediation: the mediator model (`M ~ X`) and the outcome
#'   model (`Y ~ X + M`), in either order, as `sufficient_stats(lm(M ~ X, d), lm(Y ~ X + M, d))`.
#'   The second fit goes in `...`. reprokit works out which is which; the two must be
#'   fitted to the same cases. Covariates or more mediators get a verdict, not an error.
#'
#' A result and the formula for the same analysis give the same summaries; the field
#' `meta$route` records which one was used.
#'
#' The verdict is one of [verdicts()], drawn around what an APA-style report contains.
#' `apa_suffices` means the numbers APA style asks for determine the analysis exactly.
#' `apa_plus_suffices` means exact reproduction needs a number beyond them: the object
#' names each addition, why it is needed and, when the APA numbers imply a range for it,
#' that range ("r is implied between .60 and .64 by the APA numbers; reporting it makes
#' the result exact"). An addition that follows from the APA numbers by an exact identity
#' at printed precision (the correlation in a simple regression, which is the standardized
#' coefficient) does not lower the verdict. Every verdict ends with the reminder that
#' sharing the raw data remains the standard (APA Publication Manual section 1.14,
#' Ethics Code 8.14).
#'
#' **Typed numbers.** Without a model or data, the verdict can be had from the numbers a paper
#' reports: give the test as the first argument or as `test =`, and the numbers as strings,
#' as in [check_reported()] (same test names, parsing and checks for which quantities a test
#' takes). So
#' `sufficient_stats(test = "paired", n = "30", m = c("5.1", "4.6"), sd = c("1.2", "1.3"))`
#' works, and a formula, a model or a test result is never mistaken for it, because those
#' are not a single test name. The result has `meta$route = "typed"` and holds no summaries:
#' [appendix()] refuses it, because an appendix needs a model or data. Each addition is
#' marked as reported, implied (with its range) or missing. An unknown or inapplicable
#' quantity is an input error; the verdict itself never errors.
#'
#' @param x A fitted model, a test result, a formula, or the name of a test for typed
#'   numbers (see above).
#' @param ... Passed on: for typed numbers, the reported numbers and the test arguments of
#'   [check_reported()] (`mu`, `dims`, `p0`, `effect`, `k`, `variants`); otherwise the second
#'   `lm` fit of a mediation, `data` for a formula or a
#'   `t.test` / `cor.test` result, `test` for a formula, and the choices of the analysis for a
#'   formula (`alternative`,
#'   `conf_level`, `mu`, `correct`, `p0`, `ss_type`).
#' @param threshold Smallest group or cell size whose summaries are clearly less
#'   identifying than the raw data. Default 5. The total sample size must also be at
#'   least `2 * threshold`.
#' @return An object of class `reprokit_sufficient_stats` with the verdict
#'   (`verdict`, `verdict_reason`), what to publish (`publish`), the handler's default
#'   `lost`, the `disclosure` check, `additions` (one entry per number beyond the APA
#'   set: `quantity`, `why`, `covered`, `status` and `range`), and `stats` and `meta` (`NULL`
#'   and mostly empty when the summaries do not suffice, and for typed numbers).
#' @export
#' @examples
#' sufficient_stats(extra ~ group, data = sleep, test = "welch")
#'
#' # From the result you already have:
#' sufficient_stats(t.test(extra ~ group, data = sleep), data = sleep)
#' sufficient_stats(chisq.test(table(mtcars$am, mtcars$vs)))
#'
#' # Mediation from two models, in either order:
#' sufficient_stats(lm(hp ~ wt, data = mtcars), lm(mpg ~ wt + hp, data = mtcars))
#'
#' # From numbers typed from a paper, no data needed:
#' sufficient_stats(test = "paired", n = "30", m = c("5.1", "4.6"), sd = c("1.2", "1.3"),
#'                  t = "2.1", df = "29")
#'
#' # A verdict, not an error, when summaries cannot do it:
#' sufficient_stats(glm(am ~ wt, data = mtcars, family = binomial))
sufficient_stats <- function(x, ..., threshold = 5) {
  dots <- list(...)
  if ("lost" %in% names(dots)) {
    abort_input("`lost` is not an argument: reprokit states what readers lose, from the test.")
  }
  # Typed numbers: no model or data, only a test name (first argument or `test =`) and what a
  # paper reports. A formula, a model and a test result are never a single string.
  # Any other string is not a test name, so it is an unsupported object with a verdict.
  if (missing(x) || (is.character(x) && length(x) == 1L && isTRUE(x %in% names(check_tests())))) {
    test <- if (missing(x)) dots$test else x
    if (missing(x)) dots$test <- NULL
    if (is.null(test)) {
      abort_input("Name the test: sufficient_stats(test = \"paired\", n = \"30\", ...). Valid ",
                  "tests: ", paste0("\"", names(check_tests()), "\"", collapse = ", "), ".")
    }
    return(do.call(typed_sufficient_stats, c(list(test = test), dots)))
  }
  a <- do.call(assess, c(list(x), dots, list(threshold = threshold)))
  build_sufficient_stats(
    handler = a$handler,
    verdict = a$verdict,
    verdict_reason = a$reason,
    stats = a$summary$stats,
    meta = if (is.null(a$summary)) list(route = a$prep$route) else a$summary$meta,
    disclosure = a$disclosure,
    lost = a$lost,
    publish = a$publish,
    choices_read = a$prep$read,
    additions = if (is.null(a$additions)) list() else a$additions
  )
}

# The typed route: the verdict of a test from the numbers a paper reports, with the same
# vocabulary, parsing and applicability checks as check_reported() (its argument handling is
# reused, so an unknown or inapplicable name is the same input error). The verdict follows
# from the handler's declared additions; the object holds no summaries.
typed_sufficient_stats <- function(test, ...) {
  dots <- list(...)
  checked <- do.call(check_reported, c(list(test = test), dots))
  h <- get_handler(checked$handler)
  # `s` carries only what an addition's `applies` reads: the H0 proportions of a gof test.
  p0 <- dots$p0
  k <- if (!is.null(p0)) length(p0) else NULL
  shape <- list(meta = list(p0 = if (is.null(p0)) NULL else as.numeric(p0), dims = k))
  applicable <- applicable_additions(h$additions, shape)
  reported <- unique(quantity_base(checked$numbers$quantity))
  additions <- lapply(stats::setNames(nm = names(applicable)), function(q) {
    a <- applicable[[q]]
    found <- checked$implied[[q]]
    status <- if (!is.null(a$covered)) "covered" else if (q %in% reported) "present" else
      if (!is.null(found) && isTRUE(found$feasible)) "implied" else "missing"
    range <- if (identical(status, "implied")) {
      c(max(-1, floor(found$lower * 100) / 100), min(1, ceiling(found$upper * 100) / 100))
    }
    list(quantity = q, why = a$why, covered = if (is.null(a$covered)) NA_character_ else a$covered,
         status = status, range = range)
  })
  verdict <- tier_from_additions(h$additions, shape)
  build_sufficient_stats(
    handler = h$name, verdict = verdict,
    verdict_reason = paste0("From the typed numbers: ", analysis_name(h), ", ",
                            if (identical(verdict, "apa_suffices")) {
                              "the APA numbers determine the result exactly."
                            } else {
                              "the APA numbers need a further number to determine the result."
                            }),
    stats = NULL, meta = list(route = "typed", test = test),
    disclosure = list(), lost = h$lost, publish = h$needs, additions = additions
  )
}
