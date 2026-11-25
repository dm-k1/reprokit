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
#'   `x`. It receives the same `...` as [sufficient_stats()], so a formula can be matched
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
#'   value `mu`, or on the table shape `dims` and the H0 proportions `p0`, passes a
#'   function of those test arguments instead (each with a default), returning
#'   that list; the registry keeps the list at the defaults and the function
#'   (`relations_at`), and `check_reported()` builds the relations at check time.
#' @param test_args Character vector of the test arguments the handler's
#'   `relations` function takes (`"mu"`, `"dims"`, `"p0"`, `"effect"`, `"balanced"`),
#'   or `NULL` for the names of its formals. Each is a plain number or numeric
#'   vector, except `effect` (a string) and `balanced` (TRUE or FALSE, set by
#'   `check_reported()` from the reported n); a test argument the handler does not
#'   declare is an error (`relations_at()`).
#' @param domains Named list of quantity domains that replace the default
#'   `quantity_domain()` for this handler (`n = c(0, Inf)` for counts that may
#'   be 0), or `NULL`.
#' @param labels Named character vector, quantity name to the words the coverage
#'   table and the appendix use for it (`n = "the contingency table"`), or `NULL`.
#' @param appendix_table `function(s)` returning the markdown lines of the
#'   counts table that replaces the n, M and SD table in block A of the
#'   appendix, or `NULL`.
#' @param appendix_effects `function(s, answers, digits)` returning the markdown
#'   lines that replace the single result table of block A (a design with several
#'   effects, each with its own F), or `NULL`. `answers` is the author's answer per
#'   kind of variant, as `appendix()` has it.
#' @param per_cell Character vector of the quantities that have one value per
#'   condition (`cells` of them), or `NULL` for the default, `m` and `sd`. A
#'   handler whose n differs between conditions (independent groups) adds `n`.
#' @param coverage Named character vector overriding the generated text of the
#'   `sufficient`, `missing` or `checkable` column of the coverage table, or `NULL`.
#'   Only for a row whose generated wording would differ from the plan's (a
#'   mediation row with named paths); the content still comes from the registry.
#' @param missing_why Named character vector overriding the shared reason (see
#'   `missing_reasons()`) why a quantity is missing from the APA set, for this handler
#'   only (appendix block B and the coverage table), or `NULL`.
#' @param fallback `TRUE` for a general handler that only handles what no specific
#'   handler matches (the general `lm`). `find_handler()` tries the other handlers
#'   first, so a narrow handler wins whatever the alphabetical order of the names.
#' @return `name`, invisibly.
#' @keywords internal
#' @noRd
register_handler <- function(name, family, analysis, verdict, needs, catch = NA_character_,
                             lost, matches, triage = NULL, summarise = NULL, reproduce = NULL,
                             reported = NULL, disclosure = NULL, choices = NULL,
                             formula_tests = NULL, example = NULL, design = NULL,
                             sufficient = NULL, relations = NULL, per_cell = NULL,
                             not_checkable = NULL, test_args = NULL, domains = NULL,
                             labels = NULL, appendix_table = NULL, appendix_effects = NULL,
                             fallback = FALSE, coverage = NULL, missing_why = NULL) {
  check_verdict(verdict)
  relations_at <- NULL
  if (is.function(relations)) {
    relations_at <- relations
    if (is.null(test_args)) test_args <- names(formals(relations_at))
    relations <- relations_at()
  }
  stopifnot(
    is.character(name), length(name) == 1L, nzchar(name),
    is.character(lost), length(lost) >= 1L, !anyNA(lost), all(nzchar(trimws(lost))),
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
    is.null(per_cell) || is.character(per_cell),
    is.null(test_args) || (is.character(test_args) && all(test_args %in% test_arg_names())),
    is.null(domains) || (is.list(domains) && !is.null(names(domains)) &&
                           all(vapply(domains, function(d) is.numeric(d) && length(d) == 2L,
                                      logical(1)))),
    is.null(labels) || (is.character(labels) && !is.null(names(labels))),
    is.null(appendix_table) || is.function(appendix_table),
    is.null(appendix_effects) || is.function(appendix_effects),
    is.null(missing_why) || (is.character(missing_why) && !is.null(names(missing_why))),
    is.logical(fallback), length(fallback) == 1L, !is.na(fallback),
    is.null(coverage) || (is.character(coverage) && !is.null(names(coverage)) &&
                            all(names(coverage) %in% c("sufficient", "missing", "checkable")))
  )
  if (length(test_args) > 0L && is.null(relations_at)) {
    stop("Handler '", name, "' declares test arguments but its `relations` is not a function.",
         call. = FALSE)
  }
  if (!is.null(design)) {
    check_quantity_names(c(design$statistic, design$effect_size, design$descriptives,
                           design$extra), "quantity name in `design`", terms = FALSE)
  }
  if (!is.null(sufficient)) check_quantity_names(sufficient, "quantity name in `sufficient`")
  if (!is.null(not_checkable)) {
    check_quantity_names(names(not_checkable), "quantity name in `not_checkable`")
  }
  if (!is.null(per_cell)) check_quantity_names(per_cell, "quantity name in `per_cell`")
  if (!is.null(domains)) check_quantity_names(names(domains), "quantity name in `domains`")
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
    formula_tests = formula_tests, example = example, fallback = fallback,
    design = design, apa = if (is.null(design)) NULL else apa_quantities(design),
    cells = design$cells, sufficient = sufficient, relations = relations,
    relations_at = relations_at, test_args = test_args, domains = domains,
    labels = labels, appendix_table = appendix_table, appendix_effects = appendix_effects,
    coverage = coverage, missing_why = missing_why,
    per_cell = if (is.null(per_cell)) per_cell_quantities() else per_cell
  ), envir = .registry)
  invisible(name)
}

# What a handler declares about its design: the test statistic, the effect size
# (the same quantity when the statistic is the effect size, as r is for a
# correlation; NULL when APA style asks for none, as for a goodness-of-fit
# chi-squared), `cells`, how many conditions or variables have their own n, M and
# SD (NULL for none; NA for a variable number of groups, as in a one-way ANOVA, where
# the count is read from what a paper reports), and `ci`, whether APA style asks for a
# confidence interval of the effect size (FALSE for eta squared, whose CI is rarely
# reported). The arity of m and sd follows from `cells`. `descriptives` are the
# quantities reported per cell (a chi-squared table reports only n) and `extra`
# further quantities APA asks for after p (N for chi-squared).
# A regression reports numbers per coefficient and for the model (ADR 0020): `terms`
# is the number of coefficients, NULL for none, NA for a variable number (like
# `cells = NA`). A terms design asks for b, SE, the statistic (t), p and, with `ci`,
# the CI of every coefficient, and `model = TRUE` adds the model level: R squared
# (the effect size), F, df1, df2 and the model p. Without `terms` and `model` (all
# the older designs) nothing changes.
apa_design <- function(statistic, effect_size, cells = NULL, ci = TRUE,
                       descriptives = c("n", "m", "sd"), extra = NULL, terms = NULL,
                       model = FALSE) {
  stopifnot(is.character(statistic), length(statistic) == 1L,
            is.null(effect_size) || (is.character(effect_size) && length(effect_size) == 1L),
            is.null(cells) || (is.numeric(cells) && length(cells) == 1L &&
                                 (is.na(cells) || cells >= 1)),
            is.logical(ci), length(ci) == 1L, !is.na(ci),
            is.character(descriptives), length(descriptives) >= 1L,
            is.null(extra) || is.character(extra),
            is.null(terms) || (is.numeric(terms) && length(terms) == 1L &&
                                 (is.na(terms) || terms >= 1)),
            is.logical(model), length(model) == 1L, !is.na(model),
            !isTRUE(model) || !is.null(terms))
  list(statistic = statistic, effect_size = effect_size,
       cells = if (is.null(cells)) NULL else as.integer(cells), ci = ci,
       descriptives = descriptives, extra = extra,
       terms = if (is.null(terms)) NULL else as.integer(terms), model = model)
}

is_apa_design <- function(x) {
  is.list(x) && identical(names(x), c("statistic", "effect_size", "cells", "ci",
                                      "descriptives", "extra", "terms", "model"))
}

# The one rule for what APA style asks a paper to report: the test statistic, its
# degrees of freedom and p, the effect size with its confidence interval, and n,
# M and SD for each condition when the design has them (APA Publication Manual,
# 7th ed.; ADR 0010 decision 1). F has two degrees of freedom; a design without
# `ci` leaves the interval out.
apa_quantities <- function(design) {
  if (!is.null(design$terms)) return(apa_quantities_terms(design))
  df <- if (identical(design$statistic, "f")) c("df1", "df2") else "df"
  unique(c(if (!is.null(design$cells)) design$descriptives,
           design$statistic, df, "p", design$extra, design$effect_size,
           if (isTRUE(design$ci)) c("ci_lower", "ci_upper")))
}

# A regression design (ADR 0020): the per-coefficient numbers (b, SE, the statistic,
# p, the CI), then `extra`, then the model level. The residual df are the model's
# df2 (or, for mediation, follow from n), so no per-coefficient df is asked for.
apa_quantities_terms <- function(design) {
  unique(c(if (!is.null(design$cells)) design$descriptives,
           "b", "se", design$statistic, "p",
           if (isTRUE(design$ci)) c("ci_lower", "ci_upper"),
           design$extra,
           if (isTRUE(design$model)) c(design$effect_size, "f", "df1", "df2", "p_model")))
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
# A regression reports b, SE and the model's R squared, which the relations take as
# inputs: what an APA set reaches through them counts as reported, so the
# sufficient quantities that follow from it (r from b and the SDs) are not missing.
missing_quantities <- function(h) {
  if (is.null(h$design$terms)) return(setdiff(h$sufficient, h$apa))
  setdiff(h$sufficient, closure_under_relations(h, h$apa))
}

# The relations' input and output names without the coefficient ("b:wt" -> "b").
relation_bases <- function(names) unique(quantity_base(names))

closure_under_relations <- function(h, known) {
  repeat {
    outputs <- unlist(lapply(h$relations, function(rel) {
      if (all(relation_bases(rel$inputs) %in% known)) quantity_base(rel$output)
    }))
    grown <- union(known, outputs)
    if (length(grown) == length(known)) return(known)
    known <- grown
  }
}

# Everything the relations reach from `sufficient`, itself included: add the
# output of every relation whose inputs are all known until nothing changes.
# Meta that is not a quantity (the test value mu) is not an input, and the
# relations are the ones at their defaults (mu = 0); per-cell quantities count
# as one name, as in `relation()`. A regression's coefficients (b, SE) and model R
# squared are inputs nothing computes: they are what a paper reports and the
# relations cohere, so a terms design starts from them too, and the relations
# reach the rest (t, p, the CI, F).
reached_quantities <- function(h) {
  reached <- h$sufficient
  if (!is.null(h$design$terms)) {
    inputs <- unlist(lapply(h$relations, function(rel) relation_bases(rel$inputs)))
    outputs <- unlist(lapply(h$relations, function(rel) quantity_base(rel$output)))
    reached <- union(reached, setdiff(inputs, outputs))
  }
  repeat {
    outputs <- unlist(lapply(h$relations, function(rel) {
      if (all(relation_bases(rel$inputs) %in% reached)) quantity_base(rel$output)
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
    if (kind == "test" && all(codes %in% ss_type_codes())) {
      labels[["test"]] <- "SS type for unbalanced designs"
    }
    paste0(labels[[kind]], " (", paste(variant_label(codes), collapse = ", "), ")")
  }))
  c(from_relations, h$choices)
}

# The test arguments a handler's relations can take (`test_args`).
test_arg_names <- function() c("mu", "dims", "p0", "effect", "balanced", "k", "terms")

# Display ---------------------------------------------------------------------

# Quantities as a reader writes them: "M, SD per condition" for the ones with
# one value per condition (when there is more than one), "CI" for both bounds.
display_quantities <- function(h, quantities) {
  if (length(quantities) == 0L) return(character())
  unit <- if (identical(h$design$effect_size, "r") || !is.null(h$design$terms)) {
    "variable"
  } else {
    "condition"
  }
  labels <- c(m = "M", sd = "SD", f = "F", chisq = "chisq", eta2 = "eta2", n_total = "N",
              v = "V", p0 = "H0 proportions", b = "b", se = "SE", r2 = "R\u00b2",
              p_model = "model p", indirect = "a\u00b7b", r_all = "the correlations",
              xz = "the X\u00b7Z moments")
  labels[names(h$labels)] <- h$labels
  label <- function(q) if (q %in% names(labels)) labels[[q]] else q
  grouped <- if (is.null(h$cells) || (!is.na(h$cells) && h$cells < 2L)) character() else
    setdiff(intersect(quantities, h$per_cell), names(h$labels))
  items <- character()
  for (q in quantities) {
    if (q %in% grouped) {
      if (q == grouped[1L]) {
        items <- c(items, paste0(paste(vapply(grouped, label, character(1)), collapse = ", "),
                                 " per ", unit))
      }
    } else if (q == "ci_upper" && "ci_lower" %in% quantities) {
      next
    } else if (q %in% c("indirect_ci_lower", "indirect_ci_upper")) {
      # The indirect effect and its CI read as one item (ADR 0020 point 7).
      if ("indirect" %in% quantities) next
      items <- c(items, if (q == "indirect_ci_lower") "the CI of a\u00b7b" else next)
    } else if (q == "indirect" && any(c("indirect_ci_lower", "indirect_ci_upper") %in% quantities)) {
      items <- c(items, "a\u00b7b and its CI")
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
