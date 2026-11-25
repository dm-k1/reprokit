# Recompute the reported results from the stored summaries (ADR 0029: internal, the engine
# under appendix() and the match check; it is not exported). Accepts what sufficient_stats()
# accepts, or its object. Refuses with abort_impossible() when summaries do not suffice.
reproduce <- function(x, ...) {
  s <- as_sufficient_stats(x, ...)
  require_summaries(s, "reproduce these results")
  get_handler(s$handler)$reproduce(s)
}

# The object a verb works from: the one it was given, or the one sufficient_stats() makes.
as_sufficient_stats <- function(x, ...) {
  if (is_sufficient_stats(x)) x else sufficient_stats(x, ...)
}

# A verb that needs the summaries refuses, with the narration, when there are none.
require_summaries <- function(s, what) {
  if (!identical(s$verdict, "summaries_suffice") || is.null(s$stats)) {
    abort_impossible(what, paste0("the verdict is ", s$verdict, ".\n",
                                  paste(sufficient_stats_narration(s), collapse = "\n")),
                     alternatives = character())
  }
  invisible(s)
}
