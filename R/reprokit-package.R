#' reprokit: reproduce and check results when raw data cannot be shared
#'
#' Sharing raw data remains the standard. When sharing is genuinely
#' impossible, reprokit says what the minimum is that authors owe their
#' readers, and what those readers lose. It also lets readers check whether the
#' numbers a paper reports are consistent with each other.
#'
#' Two directions use the same statistics.
#'
#' **Share** (you cannot share the raw data):
#' * [sufficient_stats()] says whether summary statistics suffice and what to publish, and
#'   stores them. It never errors on the verdict, and its print narrates the verdict,
#'   what to publish and what readers lose. It takes a fitted model or result, a formula,
#'   or, for a mediation, two `lm` fits.
#' * [appendix()] writes the appendix for the paper from the same input. reprokit states
#'   what readers lose, from the test; you do not fill that in. A `sharing_reason`, in
#'   your own words, is optional: without one `appendix()` gives a warning and the
#'   appendix says that no reason was stated.
#'
#' **Check** (you are reading a paper):
#' * [check_reported()] tests whether reported numbers are consistent with each other,
#'   allowing for rounding, for the `test` you name.
#'
#' An inconsistency means the numbers cannot all be correct as reported. It does
#' not say which number is wrong or why.
#'
#' Reference: [coverage_table()] lists which tests are covered and what each needs, and
#' [conventions()] the effect-size conventions and sums-of-squares types you can declare.
#' See also [handlers()], [quantity_names()] and [verdicts()].
#'
#' @section Vignettes:
#' * `vignette("reprokit")`: getting started, one example for each direction.
#' * `vignette("concepts")`: what the terms mean.
#' * `vignette("validation")`: the evidence behind the claims.
#'
#' @keywords internal
"_PACKAGE"
