#' The three sufficient_stats verdicts
#'
#' * `summaries_suffice`  -- exact reproduction from summary statistics
#' * `synthetic_possible` -- approximate reproduction via synthetic data
#' * `needs_raw_data`     -- exact reproduction, diagnostics, reanalysis or
#'   fraud detection need the raw data
#'
#' @return A character vector of verdict codes.
#' @export
#' @examples
#' verdicts()
verdicts <- function() {
  c("summaries_suffice", "synthetic_possible", "needs_raw_data")
}

check_verdict <- function(verdict) {
  if (!is.character(verdict) || length(verdict) != 1L || !verdict %in% verdicts()) {
    stop(
      "`verdict` must be one of: ", paste(verdicts(), collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(verdict)
}
