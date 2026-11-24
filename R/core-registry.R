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
#'   verdict is then still checked against `disclosure`, so that summaries of tiny
#'   groups do not count as sufficient.
#' @param summarise `function(x, ...)` returning `list(stats = , meta = )`: the
#'   sufficient statistics at full precision. `meta$n` must be the total sample
#'   size (used by the general minimum-n check). It may also return
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
#' @param choices Character vector of the choices the author must declare that
#'   no relation varies over (SS type, centering, contrasts), or `NULL`. The
#'   variant kinds of the relations (test, tail, level, convention) are added
#'   by `handler_choices()`.
#' @param not_checkable Named character vector, quantity name to reason, for
#'   each APA quantity the relations cannot reach from `sufficient`, or `NULL`.
#'   A test fails when an unreached quantity has no reason.
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
#' @param sufficient Character vector of the quantities that suffice to
#'   reproduce the analysis (names from `quantity_names()`), or `NULL`.
#' @param relations List of `relation()`s between quantities, used by
#'   `check_reported()`, or `NULL`. A handler whose relations depend on the test
#'   value `mu` passes `function(mu)` instead, returning that list; the
#'   registry keeps the list for `mu = 0` and the function (`relations_at`),
#'   and `check_reported(mu = )` builds the relations at check time.
#' @param per_cell Character vector of the quantities that have one value per
#'   condition (`cells` of them), or `NULL` for the default, `m` and `sd`. A
#'   handler whose n differs between conditions (independent groups) adds `n`.
#' @return `name`, invisibly.
#' @keywords internal
#' @noRd
register_handler <- function(name, family, analysis, verdict, needs, catch = NA_character_,
                             lost, matches, triage = NULL, summarise = NULL, reproduce = NULL,
                             reported = NULL, disclosure = NULL, choices = NULL,
                             formula_tests = NULL, example = NULL, design = NULL,
                             sufficient = NULL, relations = NULL, per_cell = NULL,
                             not_checkable = NULL) {
  check_verdict(verdict)
  relations_at <- NULL
  if (is.function(relations)) {
    relations_at <- relations
    relations <- relations_at(0)
  }
  stopifnot(
    is.character(name), length(name) == 1L, nzchar(name),
    is.character(lost), length(lost) >= 1L,
    is.function(matches),
    is.null(triage) || is.function(triage),
    is.null(summarise) || is.function(summarise),
    is.null(reproduce) || is.function(reproduce),
    is.null(reported) || is.function(reported),
    is.null(disclosure) || is.function(disclosure),
    is.null(choices) || is.character(choices),
    is.null(not_checkable) || (is.character(not_checkable) && !is.null(names(not_checkable)) &&
                                 all(nzchar(names(not_checkable)))),
    is.null(formula_tests) || is.character(formula_tests),
    is.null(example) || is.function(example),
    is.null(design) || is_apa_design(design),
    is.null(sufficient) || is.character(sufficient),
    is.null(relations) || (is.list(relations) && all(vapply(relations, is_relation, logical(1)))),
    is.null(per_cell) || is.character(per_cell)
  )
  if (!is.null(design)) {
    check_quantity_names(c(design$statistic, design$effect_size), "quantity name in `design`")
  }
  if (!is.null(sufficient)) check_quantity_names(sufficient, "quantity name in `sufficient`")
  if (!is.null(not_checkable)) {
    check_quantity_names(names(not_checkable), "quantity name in `not_checkable`")
  }
  if (!is.null(per_cell)) check_quantity_names(per_cell, "quantity name in `per_cell`")
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
    choices = choices, not_checkable = not_checkable,
    formula_tests = formula_tests, example = example,
    design = design, apa = if (is.null(design)) NULL else apa_quantities(design),
    cells = design$cells, sufficient = sufficient, relations = relations,
    relations_at = relations_at,
    per_cell = if (is.null(per_cell)) per_cell_quantities() else per_cell
  ), envir = .registry)
  invisible(name)
}

# What a handler declares about its design: the test statistic, the effect size
# (the same quantity when the statistic is the effect size, as r is for a
# correlation), `cells`, how many conditions or variables have their own n, M and
# SD (NULL for none; NA for a variable number of groups, as in a one-way ANOVA, where
# the count is read from what a paper reports), and `ci`, whether APA style asks for a
# confidence interval of the effect size (FALSE for eta squared, whose CI is rarely
# reported). The arity of m and sd follows from `cells`.
apa_design <- function(statistic, effect_size, cells = NULL, ci = TRUE) {
  stopifnot(is.character(statistic), length(statistic) == 1L,
            is.character(effect_size), length(effect_size) == 1L,
            is.null(cells) || (is.numeric(cells) && length(cells) == 1L &&
                                 (is.na(cells) || cells >= 1)),
            is.logical(ci), length(ci) == 1L, !is.na(ci))
  list(statistic = statistic, effect_size = effect_size,
       cells = if (is.null(cells)) NULL else as.integer(cells), ci = ci)
}

is_apa_design <- function(x) {
  is.list(x) && identical(names(x), c("statistic", "effect_size", "cells", "ci"))
}

# The one rule for what APA style asks a paper to report: the test statistic, its
# degrees of freedom and p, the effect size with its confidence interval, and n,
# M and SD for each condition when the design has them (APA Publication Manual,
# 7th ed.; ADR 0010 decision 1). F has two degrees of freedom; a design without
# `ci` leaves the interval out.
apa_quantities <- function(design) {
  df <- if (identical(design$statistic, "f")) c("df1", "df2") else "df"
  unique(c(if (!is.null(design$cells)) c("n", "m", "sd"),
           design$statistic, df, "p", design$effect_size,
           if (isTRUE(design$ci)) c("ci_lower", "ci_upper")))
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

# Derived sets (ADR 0013) -------------------------------------------------------
# Each takes a handler, as get_handler() returns it. Only the reasons and the
# extra choices are declared; everything else is computed from `apa`,
# `sufficient` and `relations`.

# What the sufficient set needs that an APA-style paper does not report.
missing_quantities <- function(h) setdiff(h$sufficient, h$apa)

# Everything the relations reach from `sufficient`, itself included: add the
# output of every relation whose inputs are all known until nothing changes.
# Meta that is not a quantity (the test value mu) is not an input, and the
# relations are the ones at their defaults (mu = 0); per-cell quantities count
# as one name, as in `relation()`.
reached_quantities <- function(h) {
  reached <- h$sufficient
  repeat {
    outputs <- unlist(lapply(h$relations, function(rel) {
      if (all(rel$inputs %in% reached)) rel$output
    }))
    grown <- union(reached, outputs)
    if (length(grown) == length(reached)) return(reached)
    reached <- grown
  }
}

# The APA quantities that are derived from the sufficient set, in APA order. The
# ones in `sufficient` itself are the data, not a check, so they are left out.
checkable_quantities <- function(h) {
  setdiff(intersect(h$apa, reached_quantities(h)), h$sufficient)
}

# The APA quantities that are neither in `sufficient` nor reached.
unreached_quantities <- function(h) {
  setdiff(h$apa, c(h$sufficient, reached_quantities(h)))
}

# The choices to declare: each kind of variant the relations vary over, with
# its variants, then the handler's declared extras.
handler_choices <- function(h) {
  labels <- c(tail = "tails", level = "confidence level", test = "test",
              convention = "effect-size convention")
  kinds <- relation_variant_kinds()
  from_relations <- unlist(lapply(kinds, function(kind) {
    of_kind <- Filter(function(rel) kind %in% names(rel$variants), h$relations)
    if (length(of_kind) == 0L) return(NULL)
    codes <- unique(unlist(lapply(of_kind, function(rel) rel$variants[[kind]])))
    paste0(labels[[kind]], " (", paste(variant_label(codes), collapse = ", "), ")")
  }))
  c(from_relations, h$choices)
}

# Display ---------------------------------------------------------------------

# Quantities as a reader writes them: "M, SD per condition" for the ones with
# one value per condition (when there is more than one), "CI" for both bounds.
display_quantities <- function(h, quantities) {
  if (length(quantities) == 0L) return(character())
  unit <- if (identical(h$design$effect_size, "r")) "variable" else "condition"
  labels <- c(m = "M", sd = "SD", f = "F", chisq = "chisq", eta2 = "eta2")
  label <- function(q) if (q %in% names(labels)) labels[[q]] else q
  grouped <- if (is.null(h$cells) || (!is.na(h$cells) && h$cells < 2L)) character() else
    intersect(quantities, h$per_cell)
  items <- character()
  for (q in quantities) {
    if (q %in% grouped) {
      if (q == grouped[1L]) {
        items <- c(items, paste0(paste(vapply(grouped, label, character(1)), collapse = ", "),
                                 " per ", unit))
      }
    } else if (q == "ci_upper" && "ci_lower" %in% quantities) {
      next
    } else {
      items <- c(items, if (q == "ci_lower" && "ci_upper" %in% quantities) "CI" else label(q))
    }
  }
  items
}

# "r (reason)" items, one per distinct reason, joined with "; ".
with_reasons <- function(h, quantities, reasons) {
  if (length(quantities) == 0L) return("\u2014")
  reason <- unname(reasons[quantities])
  paste(vapply(unique(reason), function(why) {
    paste0(paste(display_quantities(h, quantities[reason == why]), collapse = ", "),
           " (", why, ")")
  }, character(1)), collapse = "; ")
}

list_or_dash <- function(x, sep = ", ") if (length(x) == 0L) "\u2014" else paste(x, collapse = sep)

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
      sufficient = list_or_dash(display_quantities(h, h$sufficient)),
      missing = with_reasons(h, missing_quantities(h), missing_reasons()),
      checkable = list_or_dash(display_quantities(h, checkable_quantities(h))),
      not_checkable = with_reasons(h, unreached_quantities(h), h$not_checkable),
      choices = list_or_dash(handler_choices(h), "; "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
