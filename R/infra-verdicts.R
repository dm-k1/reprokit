#' The three sufficient_stats verdicts
#'
#' The verdicts are drawn around what an APA-style report already contains:
#'
#' * `apa_suffices`      -- the numbers APA style asks a paper to report make the
#'   analysis exactly reproducible (any further number the summaries need follows
#'   from them by an exact identity at printed precision)
#' * `apa_plus_suffices` -- exact reproduction needs a number beyond the APA set,
#'   such as the correlation between the two conditions of a paired design
#' * `needs_raw_data`    -- exact reproduction, diagnostics, reanalysis or
#'   fraud detection need the raw data
#'
#' Sharing the raw data remains the standard (APA Publication Manual section 1.14,
#' Ethics Code 8.14). For an analysis that has no finite summaries (a logistic or
#' multilevel model), the advice mentions synthetic data as an option outside reprokit.
#'
#' @return A character vector of verdict codes.
#' @export
#' @examples
#' verdicts()
verdicts <- function() {
  c("apa_suffices", "apa_plus_suffices", "needs_raw_data")
}

# TRUE for the two verdicts under which summaries are stored.
verdict_suffices <- function(verdict) {
  is.character(verdict) && length(verdict) == 1L &&
    verdict %in% c("apa_suffices", "apa_plus_suffices")
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

# What a handler declares for a quantity its sufficient set has beyond the APA set (ADR 0032).
# `why` is the reader-facing reason it is needed. `covered` is the exact identity at printed
# precision that makes the APA numbers determine it (NULL when none does). `applies` is a
# `function(s)` of the summary (list(stats, meta)): the addition counts only when it returns
# TRUE (the H0 proportions of a goodness-of-fit test, only when not uniform). `range` is a
# `function(s)` returning the interval the APA numbers imply for it, as a length-2 numeric
# vector, or NULL.
addition <- function(why, covered = NULL, applies = NULL, range = NULL) {
  stopifnot(is.character(why), length(why) == 1L, nzchar(why),
            is.null(covered) || (is.character(covered) && length(covered) == 1L),
            is.null(applies) || is.function(applies), is.null(range) || is.function(range))
  list(why = why, covered = covered, applies = applies, range = range)
}

# The tier follows from the declarations, never from a per-test choice: every applicable
# addition covered means the APA numbers suffice. `s` is the summary (to evaluate `applies`);
# without it every addition applies.
applicable_additions <- function(additions, s = NULL) {
  Filter(function(a) is.null(a$applies) || is.null(s) || isTRUE(a$applies(s)), additions)
}

tier_from_additions <- function(additions, s = NULL) {
  open <- Filter(function(a) is.null(a$covered), applicable_additions(additions, s))
  if (length(open) == 0L) "apa_suffices" else "apa_plus_suffices"
}
