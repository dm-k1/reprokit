#' Store the sufficient statistics of one analysis
#'
#' The central object of the share direction. Users get one from
#' [sufficient_stats()]; `new_sufficient_stats()` is the low-level constructor.
#' The check functions do not use it: they work from reported numbers.
#'
#' Authors never call this constructor: [sufficient_stats()] builds the object and fills
#' `lost` from the test.
#'
#' This constructor cannot make an object without a non-empty list of what readers
#' lose. The author's reason for not sharing is not stored here: it belongs to
#' [appendix()] alone, is optional, and is never filled in by reprokit.
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
#' @param lost Required: character vector of what readers lose compared with
#'   the raw data.
#' @return An object of class `reprokit_sufficient_stats`.
#' @export
#' @examples
#' s <- new_sufficient_stats(
#'   handler = "example",
#'   verdict = "apa_suffices",
#'   verdict_reason = "Group means and SDs determine the t statistic.",
#'   stats = list(n = c(20, 22), mean = c(5.1, 4.3), sd = c(1.2, 1.4)),
#'   lost = c("outlier inspection", "alternative analyses")
#' )
#' s
new_sufficient_stats <- function(handler,
                                 verdict,
                                 verdict_reason,
                                 stats = list(),
                                 meta = list(),
                                 disclosure = list(),
                                 lost) {
  s <- build_sufficient_stats(handler, verdict, verdict_reason, stats, meta, disclosure, lost)
  validate_sufficient_stats(s)
}

# The object itself. `sufficient_stats()` uses it with `stats = NULL` when the summaries do
# not suffice. It holds no sharing reason: that belongs to appendix() alone (ADR 0030).
build_sufficient_stats <- function(handler, verdict, verdict_reason, stats = list(),
                                   meta = list(), disclosure = list(),
                                   lost,
                                   publish = NA_character_, choices_read = list(),
                                   additions = list()) {
  s <- structure(
    list(
      handler = handler,
      verdict = verdict,
      verdict_reason = verdict_reason,
      publish = publish,
      stats = stats,
      meta = utils::modifyList(default_meta(), meta),
      disclosure = disclosure,
      choices_read = choices_read,
      additions = additions,
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
    ss_type = NULL,
    route = "formula"
  )
}

#' @rdname new_sufficient_stats
#' @param x An object.
#' @export
is_sufficient_stats <- function(x) inherits(x, "reprokit_sufficient_stats")

validate_sufficient_stats <- function(s) {
  is_single_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  suffice <- verdict_suffices(s$verdict)
  typed <- identical(s$meta$route, "typed")

  if (!(is_single_string(s$handler) && nzchar(s$handler)) &&
        !(!suffice && identical(s$handler, NA_character_))) {
    stop("`handler` must be a single non-empty string.", call. = FALSE)
  }
  check_verdict(s$verdict)
  if (!is_single_string(s$verdict_reason) || !nzchar(trimws(s$verdict_reason))) {
    stop("`verdict_reason` must explain the verdict in one sentence.", call. = FALSE)
  }
  if (!is.character(s$lost) || length(s$lost) == 0L || any(!nzchar(trimws(s$lost)))) {
    stop(
      "The sufficient statistics must list what readers lose without the raw data (`lost`).",
      call. = FALSE
    )
  }
  for (field in c("meta", "disclosure")) {
    if (!is.list(s[[field]])) stop("`", field, "` must be a list.", call. = FALSE)
  }
  # Summaries exist exactly when they suffice; otherwise there is nothing stored.
  # Typed numbers (ADR 0032) hold a verdict but no summaries.
  if (!is.list(s$stats) && !(is.null(s$stats) && (!suffice || typed))) {
    stop("`stats` must be a list (NULL only when summaries do not suffice).", call. = FALSE)
  }
  s
}

#' @export
print.reprokit_sufficient_stats <- function(x, ...) {
  cat(sufficient_stats_narration(x), sep = "\n")
  invisible(x)
}

# The narration of the verdict: what was found, why, what to publish, what is lost and, when
# a disclosure flag fired, which group or cell. Also the text of the refusal when a verb is
# given an object whose summaries do not suffice.
sufficient_stats_narration <- function(x) {
  suffice <- verdict_suffices(x$verdict)
  typed <- identical(x$meta$route, "typed")
  known <- !is.na(x$handler) && x$handler %in% handlers()
  analysis <- if (known) analysis_name(get_handler(x$handler)) else "not recognised"
  route <- x$meta$route
  header <- paste0("<reprokit sufficient_stats>  analysis: ", analysis,
                   if (!is.null(route) && known) paste0("  (input: ", route, ")"))
  verdict <- c(paste0("Verdict: ", x$verdict), paste0("Because: ", x$verdict_reason))
  flagged <- x$disclosure$problems
  disclosure <- if (length(flagged) > 0L) {
    c("Disclosure flag fired:", paste0("  * ", flagged))
  } else if (suffice && length(x$disclosure$checked) > 0L) {
    paste0("Disclosure check: passed (", x$disclosure$checked, ", threshold ",
           x$disclosure$threshold, ").")
  }
  read <- x$choices_read
  read_lines <- if (length(read) > 0L) {
    paste0("Choices read from the ", attr(read, "fn"), " result: ",
           paste0(names(read), " = ", unlist(read), collapse = ", "), ".")
  }
  standard <- paste0("Sharing the raw data remains the standard ",
                     "(APA Publication Manual section 1.14, Ethics Code 8.14).")
  tier <- if (suffice && known) tier_lines(get_handler(x$handler), x$verdict, x$additions)
  if (typed) {
    c(header, verdict, tier,
      paste("Nothing is stored: typed numbers hold no reproducible summaries, so appendix()",
            "needs a model or data."),
      "What readers lose compared with the raw data:", paste0("  * ", x$lost), standard)
  } else if (suffice) {
    c(header, verdict, tier,
      paste0("To publish: ", x$publish, " (appendix() writes this out)."),
      disclosure, read_lines,
      "What readers lose compared with the raw data:", paste0("  * ", x$lost),
      paste0("Statistics stored: ", paste(names(x$stats), collapse = ", ")), standard)
  } else {
    c(header, verdict, disclosure, read_lines,
      "Nothing is stored: summary statistics do not make this analysis reproducible.",
      "What readers lose without the raw data:", paste0("  * ", x$lost),
      "Instead you could:", paste0("  * ", default_alternatives(synthetic = !known)), standard)
  }
}
