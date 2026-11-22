#' Create a kit
#'
#' The kit is the central object: everything reprokit does creates, uses or
#' checks one. Users normally get a kit from `kit()` (planned); `new_kit()` is
#' the low-level constructor that handlers call.
#'
#' A kit cannot exist without a stated reason for not sharing and a non-empty
#' list of what readers lose. This keeps the tool from becoming a respectable
#' excuse for not sharing.
#'
#' @param handler Name of the registered handler that made the kit.
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
#' @return An object of class `reprokit_kit`.
#' @export
#' @examples
#' k <- new_kit(
#'   handler = "example",
#'   verdict = "summaries_suffice",
#'   verdict_reason = "Group means and SDs determine the t statistic.",
#'   stats = list(n = c(20, 22), mean = c(5.1, 4.3), sd = c(1.2, 1.4)),
#'   sharing_reason = "Consent form did not cover public sharing.",
#'   lost = c("outlier inspection", "alternative analyses")
#' )
#' k
new_kit <- function(handler,
                    verdict,
                    verdict_reason,
                    stats = list(),
                    meta = list(),
                    disclosure = list(),
                    sharing_reason,
                    lost) {
  kit <- structure(
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
    class = "reprokit_kit"
  )
  validate_kit(kit)
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

#' @rdname new_kit
#' @param x An object.
#' @export
is_kit <- function(x) inherits(x, "reprokit_kit")

validate_kit <- function(kit) {
  is_single_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

  if (!is_single_string(kit$handler) || !nzchar(kit$handler)) {
    stop("`handler` must be a single non-empty string.", call. = FALSE)
  }
  check_verdict(kit$verdict)
  if (!is_single_string(kit$verdict_reason) || !nzchar(trimws(kit$verdict_reason))) {
    stop("`verdict_reason` must explain the verdict in one sentence.", call. = FALSE)
  }
  if (!is_single_string(kit$sharing_reason) || !nzchar(trimws(kit$sharing_reason))) {
    stop(
      "A kit needs a stated reason for not sharing the raw data (`sharing_reason`).",
      call. = FALSE
    )
  }
  if (!is.character(kit$lost) || length(kit$lost) == 0L || any(!nzchar(trimws(kit$lost)))) {
    stop(
      "A kit must list what readers lose without the raw data (`lost`).",
      call. = FALSE
    )
  }
  for (field in c("stats", "meta", "disclosure")) {
    if (!is.list(kit[[field]])) stop("`", field, "` must be a list.", call. = FALSE)
  }
  kit
}

#' @export
print.reprokit_kit <- function(x, ...) {
  cat("<reprokit kit>  handler:", x$handler, "\n")
  cat("Verdict:        ", x$verdict, " -- ", x$verdict_reason, "\n", sep = "")
  cat("Not shared because:", x$sharing_reason, "\n")
  cat("Statistics stored: ",
      if (length(x$stats)) paste(names(x$stats), collapse = ", ") else "(none)",
      "\n", sep = "")
  cat("What readers lose:\n")
  cat(paste0("  * ", x$lost, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
