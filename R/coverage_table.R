# "r (reason)" items, one per distinct reason, joined with "; ".
with_reasons <- function(h, quantities, reasons) {
  if (length(quantities) == 0L) return("\u2014")
  reason <- unname(reasons[quantities])
  paste(vapply(unique(reason), function(why) {
    paste0(paste(display_quantities(h, quantities[reason == why]), collapse = ", "),
           " (", why, ")")
  }, character(1)), collapse = "; ")
}

# The shared reasons, with the handler's own wording where it declares one.
handler_missing_reasons <- function(h) {
  reasons <- missing_reasons()
  reasons[names(h$missing_why)] <- h$missing_why
  reasons
}

list_or_dash <- function(x, sep = ", ") if (length(x) == 0L) "\u2014" else paste(x, collapse = sep)

# A handler's declared wording for a coverage column, else the generated one.
coverage_text <- function(h, column, generated) {
  if (column %in% names(h$coverage)) h$coverage[[column]] else generated
}

#' Coverage table generated from the handler registry
#'
#' One row per registered test that has an APA design, generated from the
#' registry so it always matches what the package does. Missing,
#' checkable and the choices from relations are computed; only the reasons and
#' extra choices are declared by each handler.
#'
#' @return A data frame of character columns, one row per test:
#'   * `test`: the analysis;
#'   * `apa`: what APA style asks a paper to report;
#'   * `sufficient`: the quantities that determine every reported result;
#'   * `missing`: sufficient but not in APA, each with its reason;
#'   * `checkable`: APA numbers the relations derive from `sufficient`;
#'   * `not_checkable`: APA numbers they do not reach, each with its reason;
#'   * `choices`: what the author must declare.
#'
#'   An empty cell shows an em dash. Handlers without a design are left out.
#' @export
#' @examples
#' coverage_table()
coverage_table <- function() {
  cols <- c("test", "apa", "sufficient", "missing", "checkable", "not_checkable", "choices")
  hs <- Filter(function(h) !is.null(h$design), lapply(handlers(), get_handler))
  if (length(hs) == 0L) {
    return(as.data.frame(stats::setNames(replicate(length(cols), character(), FALSE), cols)))
  }
  hs <- hs[order(vapply(hs, `[[`, character(1), "family"),
                 vapply(hs, `[[`, character(1), "analysis"))]
  rows <- lapply(hs, function(h) {
    data.frame(
      test = h$analysis,
      apa = list_or_dash(display_quantities(h, h$apa)),
      sufficient = coverage_text(h, "sufficient",
                                 list_or_dash(display_quantities(h, h$sufficient))),
      missing = coverage_text(h, "missing",
                              with_reasons(h, missing_quantities(h), handler_missing_reasons(h))),
      checkable = coverage_text(h, "checkable",
                                list_or_dash(display_quantities(h, checkable_quantities(h)))),
      not_checkable = with_reasons(h, unreached_quantities(h), h$not_checkable),
      choices = list_or_dash(handler_choices(h), "; "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
