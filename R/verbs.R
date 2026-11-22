#' Make a kit: the sufficient statistics of one analysis
#'
#' `kit()` finds the handler for `x`, checks that summaries suffice
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
#' @return An object of class `reprokit_kit`.
#' @export
#' @examples
#' k <- kit(extra ~ group, data = sleep, test = "welch",
#'          sharing_reason = "Consent did not cover public sharing.")
#' refit(k)
kit <- function(x, ..., sharing_reason, lost = NULL, threshold = 5) {
  handler <- find_handler(x, ...)
  tr <- triage(x, ..., threshold = threshold)
  if (is.null(handler) || tr$verdict != "summaries_suffice") {
    abort_impossible("make a kit that reproduces this result exactly", tr$reason)
  }
  s <- handler$kit(x, ...)
  disclosure <- if (is.null(handler$disclosure)) list() else handler$disclosure(s, threshold)
  new_kit(
    handler = handler$name,
    verdict = tr$verdict,
    verdict_reason = tr$reason,
    stats = s$stats,
    meta = s$meta,
    disclosure = c(disclosure, list(threshold = threshold)),
    sharing_reason = sharing_reason,
    lost = unique(c(handler$lost, lost))
  )
}

#' Recompute the reported results from a kit
#'
#' @param kit A kit from [kit()].
#' @return A named numeric vector of what a paper reports (test statistic,
#'   df, p, estimates, CI, ...), computed from the kit alone.
#' @export
#' @examples
#' k <- kit(extra ~ group, data = sleep, test = "welch",
#'          sharing_reason = "Consent did not cover public sharing.")
#' refit(k)
refit <- function(kit) {
  if (!is_kit(kit)) stop("`kit` must be a reprokit kit, made by kit().", call. = FALSE)
  if (kit$verdict != "summaries_suffice") {
    abort_impossible("refit this kit", kit$verdict_reason)
  }
  get_handler(kit$handler)$refit(kit)
}
