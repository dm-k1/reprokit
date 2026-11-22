# Registry of model handlers. Adding a model = adding a handler.
# The README coverage table is generated from this registry (coverage_table()),
# so documentation and code cannot contradict each other.

.registry <- new.env(parent = emptyenv())

#' Register a model handler
#'
#' @param name Unique id, e.g. `"t_independent"`.
#' @param family,analysis Labels for the coverage table.
#' @param verdict One of [verdicts()].
#' @param needs What the kit must store, in words.
#' @param catch Pitfalls, in words (`NA` if none).
#' @param lost Character vector: what readers lose compared with raw data.
#' @param matches `function(x)` returning `TRUE` if this handler handles `x`.
#' @param triage `function(x, ...)` returning a list with `verdict` and
#'   `reason`; `NULL` means "always `verdict`".
#' @param kit,refit Functions, or `NULL` for triage-only handlers.
#' @param example A fitted model this handler supports. Required for
#'   `summaries_suffice` handlers: the round-trip test refits it from its kit.
#' @return `name`, invisibly.
#' @keywords internal
#' @noRd
register_handler <- function(name, family, analysis, verdict, needs, catch = NA_character_,
                             lost, matches, triage = NULL, kit = NULL, refit = NULL,
                             example = NULL) {
  check_verdict(verdict)
  stopifnot(
    is.character(name), length(name) == 1L, nzchar(name),
    is.character(lost), length(lost) >= 1L,
    is.function(matches),
    is.null(triage) || is.function(triage),
    is.null(kit) || is.function(kit),
    is.null(refit) || is.function(refit)
  )
  if (verdict == "summaries_suffice" && (is.null(kit) || is.null(refit) || is.null(example))) {
    stop("Handler '", name, "' claims exact reproduction but has no kit/refit/example.",
         call. = FALSE)
  }
  assign(name, list(
    name = name, family = family, analysis = analysis, verdict = verdict,
    needs = needs, catch = catch, lost = lost,
    matches = matches, triage = triage, kit = kit, refit = refit, example = example
  ), envir = .registry)
  invisible(name)
}

unregister_handler <- function(name) {
  if (exists(name, envir = .registry, inherits = FALSE)) rm(list = name, envir = .registry)
  invisible(name)
}

get_handler <- function(name) {
  if (!exists(name, envir = .registry, inherits = FALSE)) {
    stop("No handler registered under '", name, "'.", call. = FALSE)
  }
  get(name, envir = .registry, inherits = FALSE)
}

#' List registered handlers
#'
#' @return Character vector of handler names, sorted.
#' @export
#' @examples
#' handlers()
handlers <- function() sort(ls(.registry))

#' Coverage table generated from the handler registry
#'
#' This is the table shown in the README and the class guideline. Because it
#' is generated, it always matches what the package actually does.
#'
#' @return A data frame with one row per handler.
#' @export
#' @examples
#' coverage_table()
coverage_table <- function() {
  cols <- c("family", "analysis", "verdict", "needs", "catch", "handler")
  rows <- lapply(handlers(), function(name) {
    h <- get_handler(name)
    data.frame(
      family = h$family, analysis = h$analysis, verdict = h$verdict,
      needs = h$needs, catch = h$catch, handler = h$name
    )
  })
  if (length(rows) == 0L) {
    out <- as.data.frame(stats::setNames(replicate(length(cols), character(), FALSE), cols))
  } else {
    out <- do.call(rbind, rows)
    out <- out[order(out$family, out$analysis), cols]
  }
  rownames(out) <- NULL
  out
}
