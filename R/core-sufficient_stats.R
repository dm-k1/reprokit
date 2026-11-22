#' Store the sufficient statistics of one analysis
#'
#' The central object of the share direction (ADR 0008). Users get one from
#' [sufficient_stats()]; `new_sufficient_stats()` is the low-level constructor.
#' The check functions do not use it: they work from reported numbers.
#'
#' The object cannot exist without a stated reason for not sharing and a non-empty
#' list of what readers lose. This keeps the tool from becoming a respectable
#' excuse for not sharing.
#'
#' @param handler Name of the registered handler that made the object.
#' @param verdict One of [verdicts()].
#' @param verdict_reason Why this verdict, in one sentence.
#' @param stats Named list of sufficient statistics, stored at full precision
#'   (means, covariance matrix, counts, per-cluster summaries, ...).
#' @param meta Named list of metadata: complete-case `n`, `contrasts`,
#'   `effect_size_conventions`, `rounding`, `ss_type`.
#' @param disclosure Named list of disclosure flags (small cells, small
#'   clusters, extreme values). A risk reduction, never a guarantee.
#' @param sharing_reason Required: the author's stated reason for not sharing
#'   the raw data.
#' @param lost Required: character vector of what readers lose compared with
#'   the raw data.
#' @return An object of class `reprokit_sufficient_stats`.
#' @export
#' @examples
#' s <- new_sufficient_stats(
#'   handler = "example",
#'   verdict = "summaries_suffice",
#'   verdict_reason = "Group means and SDs determine the t statistic.",
#'   stats = list(n = c(20, 22), mean = c(5.1, 4.3), sd = c(1.2, 1.4)),
#'   sharing_reason = "Consent form did not cover public sharing.",
#'   lost = c("outlier inspection", "alternative analyses")
#' )
#' s
new_sufficient_stats <- function(handler,
                                 verdict,
                                 verdict_reason,
                                 stats = list(),
                                 meta = list(),
                                 disclosure = list(),
                                 sharing_reason,
                                 lost) {
  s <- structure(
    list(
      handler = handler,
      verdict = verdict,
      verdict_reason = verdict_reason,
      stats = stats,
      meta = utils::modifyList(default_meta(), meta),
      disclosure = disclosure,
      sharing_reason = sharing_reason,
      lost = lost
    ),
    class = "reprokit_sufficient_stats"
  )
  validate_sufficient_stats(s)
}

default_meta <- function() {
  list(
    n = NA_integer_,
    contrasts = NULL,
    effect_size_conventions = NULL,
    rounding = NULL,
    ss_type = NULL
  )
}

#' @rdname new_sufficient_stats
#' @param x An object.
#' @export
is_sufficient_stats <- function(x) inherits(x, "reprokit_sufficient_stats")

validate_sufficient_stats <- function(s) {
  is_single_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

  if (!is_single_string(s$handler) || !nzchar(s$handler)) {
    stop("`handler` must be a single non-empty string.", call. = FALSE)
  }
  check_verdict(s$verdict)
  if (!is_single_string(s$verdict_reason) || !nzchar(trimws(s$verdict_reason))) {
    stop("`verdict_reason` must explain the verdict in one sentence.", call. = FALSE)
  }
  if (!is_single_string(s$sharing_reason) || !nzchar(trimws(s$sharing_reason))) {
    stop(
      paste("The sufficient statistics need a stated reason for not sharing the raw data",
            "(`sharing_reason`)."),
      call. = FALSE
    )
  }
  if (!is.character(s$lost) || length(s$lost) == 0L || any(!nzchar(trimws(s$lost)))) {
    stop(
      "The sufficient statistics must list what readers lose without the raw data (`lost`).",
      call. = FALSE
    )
  }
  for (field in c("stats", "meta", "disclosure")) {
    if (!is.list(s[[field]])) stop("`", field, "` must be a list.", call. = FALSE)
  }
  s
}

#' @export
print.reprokit_sufficient_stats <- function(x, ...) {
  cat("<reprokit sufficient_stats>  handler:", x$handler, "\n")
  cat("Verdict:        ", x$verdict, " -- ", x$verdict_reason, "\n", sep = "")
  cat("Not shared because:", x$sharing_reason, "\n")
  cat("Statistics stored: ",
      if (length(x$stats)) paste(names(x$stats), collapse = ", ") else "(none)",
      "\n", sep = "")
  cat("What readers lose:\n")
  cat(paste0("  * ", x$lost, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
