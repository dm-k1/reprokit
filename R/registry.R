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
#' @param matches `function(x, ...)` returning `TRUE` if this handler handles
#'   `x`. It receives the same `...` as [triage()], so a formula can be matched
#'   on the declared `test`.
#' @param triage `function(x, ..., threshold)` returning a list with `verdict`
#'   and `reason`; `NULL` means "always `verdict`". A `summaries_suffice`
#'   verdict is then still checked against `disclosure` (ADR 0004 condition 3).
#' @param kit `function(x, ...)` returning `list(stats = , meta = )`: the
#'   sufficient statistics at full precision. `NULL` for triage-only handlers.
#' @param refit `function(kit)` returning the named numeric vector that
#'   `reported` returns, computed from the kit alone.
#' @param reported `function(x, ...)` returning a named numeric vector of what
#'   a paper reports, computed from the raw data with base R. Required for
#'   `summaries_suffice` handlers: it is the reference in the round-trip test.
#' @param disclosure `function(k, threshold)`, where `k` is what `kit` returns
#'   (`list(stats = , meta = )`), returning
#'   `list(passed = , checked = , problems = )`. A risk reduction, never a
#'   guarantee.
#' @param conventions Character vector of choices the author must declare
#'   (test variant, SS type, effect-size formula), or `NULL`.
#' @param formula_tests Character vector of `test =` values this handler
#'   accepts with a formula (e.g. `c("student", "welch")`), or `NULL`. Used to
#'   tell users which tests exist when no handler matches.
#' @param example `function()` returning `list(x = , args = list(...))`, an
#'   input this handler supports. Required for `summaries_suffice` handlers.
#' @return `name`, invisibly.
#' @keywords internal
#' @noRd
register_handler <- function(name, family, analysis, verdict, needs, catch = NA_character_,
                             lost, matches, triage = NULL, kit = NULL, refit = NULL,
                             reported = NULL, disclosure = NULL, conventions = NULL,
                             formula_tests = NULL, example = NULL) {
  check_verdict(verdict)
  stopifnot(
    is.character(name), length(name) == 1L, nzchar(name),
    is.character(lost), length(lost) >= 1L,
    is.function(matches),
    is.null(triage) || is.function(triage),
    is.null(kit) || is.function(kit),
    is.null(refit) || is.function(refit),
    is.null(reported) || is.function(reported),
    is.null(disclosure) || is.function(disclosure),
    is.null(conventions) || is.character(conventions),
    is.null(formula_tests) || is.character(formula_tests),
    is.null(example) || is.function(example)
  )
  exact_parts <- list(kit = kit, refit = refit, reported = reported, example = example)
  missing_parts <- names(exact_parts)[vapply(exact_parts, is.null, logical(1))]
  if (verdict == "summaries_suffice" && length(missing_parts) > 0L) {
    stop("Handler '", name, "' claims exact reproduction but has no ",
         paste(missing_parts, collapse = "/"), ".", call. = FALSE)
  }
  assign(name, list(
    name = name, family = family, analysis = analysis, verdict = verdict,
    needs = needs, catch = catch, lost = lost, matches = matches, triage = triage,
    kit = kit, refit = refit, reported = reported, disclosure = disclosure,
    conventions = conventions, formula_tests = formula_tests, example = example
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
