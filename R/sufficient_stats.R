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
#' @param x A fitted model, a test result, or a formula; see above.
#' @param ... Passed on: the second `lm` fit of a mediation, `data` for a formula or a
#'   `t.test` / `cor.test` result, `test` for a formula, and the choices of the analysis for a formula (`alternative`,
#'   `conf_level`, `mu`, `correct`, `p0`, `ss_type`).
#' @param threshold Smallest group or cell size whose summaries are clearly less
#'   identifying than the raw data. Default 5. The total sample size must also be at
#'   least `2 * threshold`.
#' @return An object of class `reprokit_sufficient_stats` with the verdict
#'   (`verdict`, `verdict_reason`), what to publish (`publish`), the handler's default
#'   `lost`, the `disclosure` check, and `stats` and `meta` (`NULL` and mostly empty
#'   when the summaries do not suffice).
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
#' # A verdict, not an error, when summaries cannot do it:
#' sufficient_stats(glm(am ~ wt, data = mtcars, family = binomial))
sufficient_stats <- function(x, ..., threshold = 5) {
  dots <- list(...)
  if ("lost" %in% names(dots)) {
    abort_input("`lost` is not an argument: reprokit states what readers lose, from the test.")
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
    choices_read = a$prep$read
  )
}
