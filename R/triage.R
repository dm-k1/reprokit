#' Triage: can this result be reproduced without the raw data?
#'
#' `triage()` always returns a verdict, including "needs the raw data,
#' because ...". It never errors on unsupported input: an unknown model is
#' itself an answer.
#'
#' @param x A fitted model object (e.g. from `lm()`), or another object a
#'   handler recognises.
#' @param ... Passed to the handler's triage function.
#' @return An object of class `reprokit_triage`: a list with `verdict`,
#'   `reason`, `handler` (or `NA`) and `lost`.
#' @export
#' @examples
#' triage(list())   # unsupported input still gets a verdict
triage <- function(x, ...) {
  handler <- find_handler(x)
  if (is.null(handler)) {
    return(new_triage(
      verdict = "needs_raw_data",
      reason = paste0(
        "No handler for objects of class '", paste(class(x), collapse = "/"),
        "', so reprokit cannot show that summary statistics are enough."
      ),
      handler = NA_character_,
      lost = "exact reproduction of this analysis"
    ))
  }
  if (is.null(handler$triage)) {
    return(new_triage(handler$verdict, paste0("Per handler '", handler$name, "'."),
                      handler$name, handler$lost))
  }
  # The behaviour rule (plan 5.4) is that triage never crashes, so a failing
  # handler is reported as a verdict instead of an error.
  res <- tryCatch(
    handler$triage(x, ...),
    error = function(e) {
      list(
        verdict = "needs_raw_data",
        reason = paste0("Handler '", handler$name, "' could not triage this: ",
                        conditionMessage(e))
      )
    }
  )
  new_triage(res$verdict, res$reason, handler$name, handler$lost)
}

find_handler <- function(x) {
  for (name in handlers()) {
    h <- get_handler(name)
    if (isTRUE(tryCatch(h$matches(x), error = function(e) FALSE))) return(h)
  }
  NULL
}

new_triage <- function(verdict, reason, handler, lost) {
  check_verdict(verdict)
  structure(
    list(verdict = verdict, reason = reason, handler = handler, lost = lost),
    class = "reprokit_triage"
  )
}

#' @export
print.reprokit_triage <- function(x, ...) {
  cat("<reprokit triage>\n")
  cat("Verdict: ", x$verdict, "\n", sep = "")
  cat("Because: ", x$reason, "\n", sep = "")
  cat("What readers lose:\n", paste0("  * ", x$lost, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
