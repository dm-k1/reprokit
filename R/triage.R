#' Triage: can this result be reproduced without the raw data?
#'
#' `triage()` always returns a verdict, including "needs the raw data,
#' because ...". It never errors on unsupported input: an unknown model is
#' itself an answer.
#'
#' @param x A fitted model object (e.g. from `lm()`), a formula with `data`
#'   and `test` (e.g. `y ~ group, data = d, test = "welch"`), or another
#'   object a handler recognises.
#' @param ... Passed to the handler (e.g. `data`, `test`).
#' @param threshold Smallest group or cell size whose summaries are clearly
#'   less identifying than the raw data (ADR 0004 condition 3). Default 5.
#' @return An object of class `reprokit_triage`: a list with `verdict`,
#'   `reason`, `handler` (or `NA`) and `lost`.
#' @export
#' @examples
#' triage(list())   # unsupported input still gets a verdict
#' triage(extra ~ group, data = sleep, test = "welch")
triage <- function(x, ..., threshold = 5) {
  handler <- find_handler(x, ...)
  if (is.null(handler)) {
    return(new_triage(
      verdict = "needs_raw_data",
      reason = no_handler_reason(x),
      handler = NA_character_,
      lost = "exact reproduction of this analysis"
    ))
  }
  # The behaviour rule (plan 5.4) is that triage never crashes, so a failing
  # handler is reported as a verdict instead of an error.
  res <- tryCatch(
    triage_with(handler, x, ..., threshold = threshold),
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

triage_with <- function(handler, x, ..., threshold) {
  res <- if (is.null(handler$triage)) {
    list(verdict = handler$verdict,
         reason = if (handler$verdict == "summaries_suffice") {
           paste0("Summary statistics suffice: ", handler$needs,
                  " determine the reported results exactly.")
         } else {
           paste0("Per handler '", handler$name, "'.")
         })
  } else {
    handler$triage(x, ..., threshold = threshold)
  }
  # ADR 0004 condition 3, decided per dataset: summaries of tiny groups are
  # close to the raw data, so they protect no one and the verdict drops.
  if (res$verdict == "summaries_suffice" && !is.null(handler$disclosure)) {
    check <- handler$disclosure(handler$kit(x, ...), threshold)
    if (!check$passed) {
      res <- list(
        verdict = "needs_raw_data",
        reason = paste0(
          "The summaries would be close to the raw data (ADR 0004 condition 3 fails: ",
          paste(check$problems, collapse = "; "),
          "), so they give no real privacy gain, only less information."
        )
      )
    }
  }
  res
}

no_handler_reason <- function(x) {
  hint <- if (inherits(x, "formula")) {
    paste0(" For a formula, say which test with `test =`: ",
           paste0('"', formula_tests(), '"', collapse = ", "), ".")
  } else if (inherits(x, "htest")) {
    paste0(" A test result does not contain the data; pass the formula and data ",
           'instead, e.g. kit(y ~ group, data = d, test = "welch").')
  } else {
    ""
  }
  paste0(
    "No handler for objects of class '", paste(class(x), collapse = "/"),
    "', so reprokit cannot show that summary statistics are enough.", hint
  )
}

# Tests a formula can be matched to, collected from the registered handlers.
formula_tests <- function() {
  unlist(lapply(handlers(), function(n) get_handler(n)$formula_tests), use.names = FALSE)
}

find_handler <- function(x, ...) {
  for (name in handlers()) {
    h <- get_handler(name)
    if (isTRUE(tryCatch(h$matches(x, ...), error = function(e) FALSE))) return(h)
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
