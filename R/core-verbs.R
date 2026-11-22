#' Store the sufficient statistics of one analysis
#'
#' `sufficient_stats()` finds the handler for `x`, checks that summaries suffice
#' ([triage()] must say `summaries_suffice`, which includes the disclosure
#' check), and stores the statistics that determine the reported results at
#' full precision. If summaries do not suffice, it refuses with an error of
#' class `reprokit_impossible` that says why.
#'
#' @param x A fitted model or a formula; see [triage()].
#' @param ... Passed to the handler, e.g. `data` and `test` for a formula.
#' @param sharing_reason Required: why the raw data are not shared.
#' @param lost Extra things readers lose. They are added to the handler's own
#'   list, which cannot be removed.
#' @param threshold Smallest group or cell size for the disclosure check.
#' @return An object of class `reprokit_sufficient_stats`.
#' @export
#' @examples
#' s <- sufficient_stats(extra ~ group, data = sleep, test = "welch",
#'                       sharing_reason = "Consent did not cover public sharing.")
#' reproduce(s)
sufficient_stats <- function(x, ..., sharing_reason, lost = NULL, threshold = 5) {
  handler <- find_handler(x, ...)
  tr <- triage(x, ..., threshold = threshold)
  if (is.null(handler) || tr$verdict != "summaries_suffice") {
    abort_impossible("store sufficient statistics that reproduce this result exactly", tr$reason)
  }
  summary <- handler$summarise(x, ...)
  disclosure <- if (is.null(handler$disclosure)) list() else handler$disclosure(summary, threshold)
  new_sufficient_stats(
    handler = handler$name,
    verdict = tr$verdict,
    verdict_reason = tr$reason,
    stats = summary$stats,
    meta = summary$meta,
    disclosure = c(disclosure, list(threshold = threshold)),
    sharing_reason = sharing_reason,
    lost = unique(c(handler$lost, lost))
  )
}

#' Recompute the reported results from the sufficient statistics
#'
#' @param s An object made by [sufficient_stats()].
#' @return A named numeric vector of what a paper reports (test statistic,
#'   df, p, estimates, CI, ...), computed from `s` alone.
#' @export
#' @examples
#' s <- sufficient_stats(extra ~ group, data = sleep, test = "welch",
#'                       sharing_reason = "Consent did not cover public sharing.")
#' reproduce(s)
reproduce <- function(s) {
  if (!is_sufficient_stats(s)) stop("`s` must be made by sufficient_stats().", call. = FALSE)
  if (s$verdict != "summaries_suffice") {
    abort_impossible("reproduce these statistics", s$verdict_reason)
  }
  get_handler(s$handler)$reproduce(s)
}
