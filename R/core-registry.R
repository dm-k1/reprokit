# Registry of model handlers. Adding a model = adding a handler.
# The README coverage table is generated from this registry (coverage_table()),
# so documentation and code cannot contradict each other.

.registry <- new.env(parent = emptyenv())

#' Register a model handler
#'
#' @param name Unique id, e.g. `"t_independent"`.
#' @param family,analysis Labels for the coverage table.
#' @param verdict One of [verdicts()].
#' @param needs What must be stored, in words.
#' @param catch Pitfalls, in words (`NA` if none).
#' @param lost Character vector: what readers lose compared with raw data.
#' @param matches `function(x, ...)` returning `TRUE` if this handler handles
#'   `x`. It receives the same `...` as [triage()], so a formula can be matched
#'   on the declared `test`.
#' @param triage `function(x, ..., threshold)` returning a list with `verdict`
#'   and `reason`; `NULL` means "always `verdict`". A `summaries_suffice`
#'   verdict is then still checked against `disclosure` (ADR 0004 condition 3).
#' @param summarise `function(x, ...)` returning `list(stats = , meta = )`: the
#'   sufficient statistics at full precision. `meta$n` must be the total sample
#'   size (used by the general minimum-n check, ADR 0006). It may also return
#'   `check`, information for `disclosure` only, which is not stored.
#'   `NULL` for triage-only handlers.
#' @param reproduce `function(s)` returning the named numeric vector that
#'   `reported` returns, computed from `s` (made by [sufficient_stats()]) alone.
#' @param reported `function(x, ...)` returning a named numeric vector of what
#'   a paper reports, computed from the raw data with base R. Required for
#'   `summaries_suffice` handlers: it is the reference in the round-trip test.
#' @param disclosure `function(summary, threshold)`, where `summary` is what `summarise` returns
#'   (`list(stats = , meta = , check = )`), returning
#'   `list(passed = , checked = , problems = )`. A risk reduction, never a
#'   guarantee.
#' @param conventions Character vector of choices the author must declare
#'   (test variant, SS type, effect-size formula), or `NULL`.
#' @param formula_tests Character vector of `test =` values this handler
#'   accepts with a formula (e.g. `c("student", "welch")`), or `NULL`. Used to
#'   tell users which tests exist when no handler matches.
#' @param example `function()` returning `list(x = , args = list(...))`, an
#'   input this handler supports. Required for `summaries_suffice` handlers.
#' @param design An `apa_design()`: which quantity is the test statistic, which
#'   the effect size, and how many conditions have their own n, M and SD. The
#'   quantities APA style asks a paper to report (`apa`) are built from it by
#'   one rule, `apa_quantities()`; a handler cannot list them by hand. `NULL`
#'   for handlers that do not check.
#' @param reproducible Character vector of the quantities that suffice to
#'   reproduce the analysis (names from `quantity_names()`), or `NULL`.
#' @param relations List of `relation()`s between quantities, used by
#'   `check_reported()`, or `NULL`.
#' @return `name`, invisibly.
#' @keywords internal
#' @noRd
register_handler <- function(name, family, analysis, verdict, needs, catch = NA_character_,
                             lost, matches, triage = NULL, summarise = NULL, reproduce = NULL,
                             reported = NULL, disclosure = NULL, conventions = NULL,
                             formula_tests = NULL, example = NULL, design = NULL,
                             reproducible = NULL, relations = NULL) {
  check_verdict(verdict)
  stopifnot(
    is.character(name), length(name) == 1L, nzchar(name),
    is.character(lost), length(lost) >= 1L,
    is.function(matches),
    is.null(triage) || is.function(triage),
    is.null(summarise) || is.function(summarise),
    is.null(reproduce) || is.function(reproduce),
    is.null(reported) || is.function(reported),
    is.null(disclosure) || is.function(disclosure),
    is.null(conventions) || is.character(conventions),
    is.null(formula_tests) || is.character(formula_tests),
    is.null(example) || is.function(example),
    is.null(design) || is_apa_design(design),
    is.null(reproducible) || is.character(reproducible),
    is.null(relations) || (is.list(relations) && all(vapply(relations, is_relation, logical(1))))
  )
  if (!is.null(design)) {
    check_quantity_names(c(design$statistic, design$effect_size), "quantity name in `design`")
  }
  if (!is.null(reproducible)) check_quantity_names(reproducible, "quantity name in `reproducible`")
  exact_parts <- list(summarise = summarise, reproduce = reproduce, reported = reported,
                      example = example)
  missing_parts <- names(exact_parts)[vapply(exact_parts, is.null, logical(1))]
  if (verdict == "summaries_suffice" && length(missing_parts) > 0L) {
    stop("Handler '", name, "' claims exact reproduction but has no ",
         paste(missing_parts, collapse = "/"), ".", call. = FALSE)
  }
  assign(name, list(
    name = name, family = family, analysis = analysis, verdict = verdict,
    needs = needs, catch = catch, lost = lost, matches = matches, triage = triage,
    summarise = summarise, reproduce = reproduce, reported = reported, disclosure = disclosure,
    conventions = conventions, formula_tests = formula_tests, example = example,
    design = design, apa = if (is.null(design)) NULL else apa_quantities(design),
    cells = design$cells, reproducible = reproducible, relations = relations
  ), envir = .registry)
  invisible(name)
}

# What a handler declares about its design: the test statistic, the effect size
# (the same quantity when the statistic is the effect size, as r is for a
# correlation), and `cells`, how many conditions or variables have their own n,
# M and SD (NULL for none). The arity of m and sd follows from `cells`.
apa_design <- function(statistic, effect_size, cells = NULL) {
  stopifnot(is.character(statistic), length(statistic) == 1L,
            is.character(effect_size), length(effect_size) == 1L,
            is.null(cells) || (is.numeric(cells) && length(cells) == 1L && cells >= 1))
  list(statistic = statistic, effect_size = effect_size,
       cells = if (is.null(cells)) NULL else as.integer(cells))
}

is_apa_design <- function(x) {
  is.list(x) && identical(names(x), c("statistic", "effect_size", "cells"))
}

# The one rule for what APA style asks a paper to report: the test statistic, its
# degrees of freedom and p, the effect size with its confidence interval, and n,
# M and SD for each condition when the design has them (APA Publication Manual,
# 7th ed.; ADR 0010 decision 1).
apa_quantities <- function(design) {
  unique(c(if (!is.null(design$cells)) c("n", "m", "sd"),
           design$statistic, "df", "p", design$effect_size, "ci_lower", "ci_upper"))
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
