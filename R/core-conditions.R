# Classed conditions. Tests check the class, not the message text, so messages
# can be improved without breaking tests.

default_alternatives <- function() {
  c(
    "share the raw data (possibly under a data-use agreement)",
    "share synthetic data and report its utility",
    "offer federated analysis (e.g. DataSHIELD)"
  )
}

#' Stop because a request cannot be met from summary statistics
#'
#' Used by functions asked to do the impossible, such as refitting a GLMM
#' from summaries. The error explains why and lists alternatives.
#'
#' @param what What was asked, e.g. `"refit a logistic multilevel model"`.
#' @param why Why summaries are not enough.
#' @param alternatives Character vector of alternatives to suggest.
#' @return Does not return; signals an error of class `reprokit_impossible`.
#' @keywords internal
#' @noRd
abort_impossible <- function(what, why, alternatives = default_alternatives()) {
  msg <- paste0(
    "Cannot ", what, " from summary statistics: ", why, "\n",
    paste0("* ", alternatives, collapse = "\n")
  )
  cnd <- structure(
    class = c("reprokit_impossible", "reprokit_error", "error", "condition"),
    list(message = msg, call = sys.call(-1), alternatives = alternatives)
  )
  stop(cnd)
}
