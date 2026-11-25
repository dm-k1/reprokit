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
#'   what to publish and what readers lose. It takes a fitted model or result, or a formula.
#' * [appendix()] writes the appendix for the paper. It requires a reason for not sharing
#'   and a non-empty list of what readers lose.
#'
#' **Check** (you are reading a paper):
#' * [check_reported()] tests whether reported numbers are consistent with each other,
#'   allowing for rounding, for the `test` you name.
#'
#' An inconsistency means the numbers cannot all be correct as reported. It does
#' not say which number is wrong or why.
#'
#' Which tests are covered, and what each needs, is in [coverage_table()]. See
#' also [handlers()], [quantity_names()] and [verdicts()].
#'
#' @section Vignettes:
#' * `vignette("reprokit")`: getting started, one example for each direction.
#' * `vignette("concepts")`: what the terms mean.
#' * `vignette("validation")`: the evidence behind the claims.
#'
#' @keywords internal
"_PACKAGE"
