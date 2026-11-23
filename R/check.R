# check_reported(): do the numbers a paper reports agree with each other?
# In one line: parse the reported numbers into rounding intervals, evaluate each
# of the handler's relations whose inputs are all reported, compare the derived
# range with the reported output, and summarise per number, per variant and as a
# coverage level (ADR 0010). It never says which number is wrong, only whether
# the set can be correct as reported.

#' Check reported statistics against each other
#'
#' Takes the numbers a paper reports for an analysis and tests whether they can
#' all be right at once, given how precisely they were reported ("2.85" means
#' anything from 2.845 to 2.855). Where one input is missing but the output is
#' reported, it gives the range the missing input must lie in.
#'
#' @param handler Name of a registered handler, such as `"t_paired"`.
#' @param ... Reported numbers, named by quantity (see [quantity_names()]).
#'   Pass strings, so the decimals count: `t = "2.85"`, `p = "< .001"`,
#'   `df = "29"`. A quantity with one value per group or condition is a
#'   character vector (`m = c("5.1", "4.2")`). Integer-valued numbers are taken
#'   as exact.
#' @param variants Character vector restricting which variants to try (such as
#'   `"two.sided"` or `"0.95"`), or `NULL` for all. A restriction applies to
#'   the relations that have one of the named variants; the others keep all of
#'   theirs.
#' @return A `reprokit_check` object: `$numbers` (status of each reported
#'   number), `$level` (coverage, 0 to 3), `$missing` (what would raise the
#'   level, and the range implied for it), `$variants` (a list with `tail` and
#'   `level`, each a data frame of the variants tried and whether they fit),
#'   `$relations`, `$implied` and `$checked` (whether any relation could be
#'   compared with a reported number; without one there is no verdict).
#'
#'   A reported number is only ever compared within what it can be: `r` lies
#'   in -1 to 1, `sd` and `df` are not negative, `p` is in 0 to 1, `n` is a
#'   whole number. A relation whose result is not finite for the reported values
#'   (`"1.00"` for `r` gives an infinite t) is skipped, as if an input were
#'   missing; it never counts as a failure.
#' @export
#' @examples
#' check_reported("t_paired", n = "20", t = "2.85", df = "19", p = ".010")
check_reported <- function(handler, ..., variants = NULL) {
  if (missing(handler)) {
    stop("Name the handler first: check_reported(\"t_paired\", t = ...). Handlers that can ",
         "check: ", paste(handlers_with_relations(), collapse = ", "), ".", call. = FALSE)
  }
  entry <- get_handler(handler)
  dots <- list(...)
  if (length(entry$relations) == 0L) {
    stop("Handler '", handler, "' has no relations to check against. Handlers that do: ",
         paste(handlers_with_relations(), collapse = ", "), ".", call. = FALSE)
  }
  reported <- dots
  if (length(reported) > 0L && (is.null(names(reported)) || any(!nzchar(names(reported))))) {
    stop("Every reported number must be named by its quantity, for example t = \"2.85\".",
         call. = FALSE)
  }
  check_quantity_names(names(reported), "reported quantity")
  check_arity(reported, entry$cells)
  intervals <- Map(function(x, quantity) {
    as_count <- quantity %in% c("n", "df")
    restrict_to_domain(interval_bounds(reported_value(x, as_count = as_count)), quantity, x)
  }, reported, names(reported))
  evaluations <- evaluate_relations(entry$relations, intervals, variants)

  number_status <- number_statuses(reported, evaluations, entry$relations)
  level <- coverage_level(entry, names(reported), unique(evaluations$relation))
  implied <- implied_ranges(entry, intervals, evaluations)
  structure(list(
    handler = handler,
    numbers = number_status,
    level = level,
    missing = missing_numbers(entry, names(reported), level, implied),
    variants = variant_fits(evaluations),
    relations = evaluations,
    implied = implied,
    checked = any(evaluations$checked),
    consistent = !any(number_status$status == "inconsistent")
  ), class = "reprokit_check")
}

#' @rdname check_reported
#' @export
check_t <- function(..., handler = "t_paired", variants = NULL) {
  check_reported(handler = handler, ..., variants = variants)
}

#' @rdname check_reported
#' @export
check_r <- function(..., handler = "cor_pearson", variants = NULL) {
  check_reported(handler = handler, ..., variants = variants)
}

handlers_with_relations <- function() {
  Filter(function(name) length(get_handler(name)$relations) > 0L, handlers())
}

# One row per relation and variant whose inputs are all reported. `checked` says
# the output is reported too, so `fits` is a comparison; otherwise only the
# derived range is recorded.
evaluate_relations <- function(relations, intervals, variants) {
  known_variants <- unique(unlist(lapply(relations, `[[`, "variants")))
  unknown <- setdiff(variants, known_variants)
  if (length(unknown) > 0L) {
    stop("Unknown variant: ", paste0("'", unknown, "'", collapse = ", "),
         ". This handler has: ", paste(known_variants, collapse = ", "), ".", call. = FALSE)
  }
  rows <- list()
  # An unreported output becomes available as an input to later relations with
  # the range derived for it (so p can be checked from r and n through t). Such
  # a number counts as descriptive-based: it was derived from n, m, sd or r.
  available <- intervals
  basis <- intersect(names(intervals), descriptive_quantities())
  done <- character()
  repeat {
    progressed <- FALSE
    for (rel in relations) {
      if (rel$name %in% done || !all(rel$inputs %in% names(available))) next
      done <- c(done, rel$name)
      progressed <- TRUE
      to_run <- if (is.null(rel$variants)) NA_character_ else rel$variants
      if (length(intersect(to_run, variants)) > 0L) to_run <- intersect(to_run, variants)
      first_row <- length(rows) + 1L
      for (variant in to_run) {
        derived <- relation_range(rel, available, if (is.na(variant)) NULL else variant)
        # A range that is not finite (NaN or infinite) is "not evaluable", never
        # "does not fit": skip it as if an input were missing.
        if (!all(is.finite(derived))) next
        checked <- rel$output %in% names(intervals)
        fits <- NA
        if (checked) {
          target <- interval_bounds(intervals[[rel$output]])
          fits <- intervals_overlap(derived[["lower"]], derived[["upper"]],
                                    target$lower, target$upper)
        }
        row <- evaluation_row(rel, variant, checked, fits, derived,
                              from_descriptives = any(rel$inputs %in% basis))
        rows[[length(rows) + 1L]] <- row
      }
      if (!rel$output %in% names(available) && length(rows) >= first_row) {
        # Widest range over variants, so no variant is ruled out here.
        mine <- do.call(rbind, rows[first_row:length(rows)])
        available[[rel$output]] <- list(lower = min(mine$derived_lower),
                                        upper = max(mine$derived_upper))
        if (any(rel$inputs %in% basis)) basis <- c(basis, rel$output)
      }
    }
    if (!progressed) break
  }
  if (length(rows) == 0L) {
    return(data.frame(relation = character(), variant = character(), kind = character(),
                      output = character(), inputs = character(), from_descriptives = logical(),
                      checked = logical(), fits = logical(), derived_lower = numeric(),
                      derived_upper = numeric(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

evaluation_row <- function(rel, variant, checked, fits, derived, from_descriptives) {
  data.frame(relation = rel$name, variant = variant, kind = rel$variant_kind,
             output = rel$output,
             inputs = paste(rel$inputs, collapse = ", "),
             from_descriptives = from_descriptives, checked = checked, fits = fits,
             derived_lower = derived[["lower"]], derived_upper = derived[["upper"]],
             stringsAsFactors = FALSE)
}

# A relation holds when at least one of its variants fits.
relation_outcomes <- function(evaluations) {
  checked <- evaluations[evaluations$checked, , drop = FALSE]
  names_checked <- unique(checked$relation)
  data.frame(
    relation = names_checked,
    holds = vapply(names_checked, function(name) any(checked$fits[checked$relation == name]),
                   logical(1)),
    stringsAsFactors = FALSE
  )
}

# Status of each reported number from the relations that involve it, as an
# input or as the output:
#   inconsistent  - involved in a relation that fails under every variant (we
#                   cannot tell which of its numbers is the wrong one)
#   verified      - the OUTPUT of a holding relation computed from descriptives
#                   (n, m, sd, r), reported or derived from reported numbers,
#                   i.e. recomputed from more basic numbers
#   coherent      - holds, but only as an input of such a relation, or through
#                   relations among derived numbers
#   unconfirmed   - none of the above, but it belongs to a relation whose output
#                   is reported and which could not be evaluated because another
#                   input is missing; `checked_against` says which
#   not checkable - no relation reaches it
number_statuses <- function(reported, evaluations, relations) {
  outcomes <- relation_outcomes(evaluations)
  detail <- unique(evaluations[evaluations$checked,
                               c("relation", "output", "inputs", "from_descriptives")])
  # What an unevaluated relation lacks: inputs neither reported nor derived.
  have <- c(names(reported), evaluations$output)
  stalled <- Filter(function(rel) {
    !rel$name %in% evaluations$relation && rel$output %in% names(reported)
  }, relations)
  per_quantity <- lapply(names(reported), function(quantity) {
    involved <- detail[detail$output == quantity |
                         vapply(strsplit(detail$inputs, ", "), function(i) quantity %in% i,
                                logical(1)), , drop = FALSE]
    holds <- outcomes$holds[match(involved$relation, outcomes$relation)]
    waiting <- Filter(function(rel) quantity %in% c(rel$inputs, rel$output), stalled)
    if (nrow(involved) > 0L) {
      status <- if (any(!holds)) {
        "inconsistent"
      } else if (any(involved$from_descriptives & involved$output == quantity)) {
        "verified"
      } else {
        "coherent"
      }
      list(status = status, against = paste(involved$relation, collapse = "; "))
    } else if (length(waiting) > 0L) {
      against <- vapply(waiting, function(rel) {
        lacking <- setdiff(rel$inputs, have)
        paste0(rel$name, if (length(lacking) > 0L) {
          paste0(" (needs ", paste(lacking, collapse = ", "), ")")
        } else {
          " (not evaluable for these values)"
        })
      }, character(1))
      list(status = "unconfirmed", against = paste(against, collapse = "; "))
    } else {
      list(status = "not checkable", against = NA_character_)
    }
  })
  data.frame(
    quantity = names(reported),
    reported = vapply(reported, function(x) paste(x, collapse = ", "), character(1)),
    status = vapply(per_quantity, `[[`, character(1), "status"),
    checked_against = vapply(per_quantity, `[[`, character(1), "against"),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

statistic_quantities <- function() c("t", "f", "chisq")

# Level from which quantities are reported (ADR 0010 decision 2), by the highest
# one reached:
#   3  the reported set contains the handler's `reproducible` set
#   2  some statistic (t, F, chi-square) can be recomputed from descriptives
#   1  some relation has all its numbers, output included, reported (and could
#      be evaluated)
#   0  none
# The level describes coverage, not agreement; consistency is reported apart.
# `evaluable` names the relations that could be evaluated, or NULL for all.
coverage_level <- function(handler, reported_names, evaluable = NULL) {
  if (!is.null(handler$reproducible) && all(handler$reproducible %in% reported_names)) {
    return(3L)
  }
  usable <- Filter(function(rel) {
    all(c(rel$inputs, rel$output) %in% reported_names) &&
      (is.null(evaluable) || rel$name %in% evaluable)
  }, handler$relations)
  if (any(vapply(usable, function(rel) {
    rel$output %in% statistic_quantities() && any(rel$inputs %in% descriptive_quantities())
  }, logical(1)))) {
    return(2L)
  }
  if (length(usable) > 0L) 1L else 0L
}

# For every relation with exactly one missing input and a reported output, the
# range of that input that would make the output fit (solve_for), intersected
# over the relations that constrain the same quantity.
implied_ranges <- function(handler, intervals, evaluations) {
  implied <- list()
  for (rel in handler$relations) {
    missing_inputs <- setdiff(rel$inputs, names(intervals))
    quantity <- missing_inputs[1L]
    if (length(missing_inputs) != 1L || !rel$output %in% names(intervals) ||
          is.null(solvable_bounds()[[quantity]])) {
      next
    }
    for (variant in if (is.null(rel$variants)) NA_character_ else rel$variants) {
      found <- solve_for(rel, intervals, quantity, intervals[[rel$output]],
                         solvable_bounds()[[quantity]], if (is.na(variant)) NULL else variant)
      previous <- implied[[quantity]]
      implied[[quantity]] <- if (is.null(previous)) found else intersect_implied(previous, found)
    }
  }
  implied
}

intersect_implied <- function(first, second) {
  if (!first$feasible || !second$feasible) {
    return(list(lower = NA_real_, upper = NA_real_, feasible = FALSE,
                reaches_lower = FALSE, reaches_upper = FALSE, resolution = first$resolution))
  }
  lower <- max(first$lower, second$lower)
  upper <- min(first$upper, second$upper)
  list(lower = lower, upper = upper, feasible = lower <= upper,
       reaches_lower = first$reaches_lower && second$reaches_lower,
       reaches_upper = first$reaches_upper && second$reaches_upper,
       resolution = first$resolution)
}

format_implied <- function(found) {
  if (!found$feasible) return("no value is compatible")
  digits <- max(0L, ceiling(-log10(found$resolution)))
  limit <- if (found$reaches_lower || found$reaches_upper) " (up to the limit of its range)" else ""
  paste0(formatC(found$lower, format = "f", digits = digits), " to ",
         formatC(found$upper, format = "f", digits = digits), limit)
}

# Unreported quantities of the handler that would raise the level if reported.
missing_numbers <- function(handler, reported_names, level, implied) {
  candidates <- setdiff(union(handler$apa, handler$reproducible), reported_names)
  reach <- vapply(candidates, function(quantity) {
    coverage_level(handler, c(reported_names, quantity))
  }, integer(1))
  candidates <- candidates[reach > level]
  data.frame(
    quantity = candidates,
    would_reach_level = unname(reach[candidates]),
    implied_range = vapply(candidates, function(quantity) {
      if (is.null(implied[[quantity]])) NA_character_ else format_implied(implied[[quantity]])
    }, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Which variants fit: a variant fits when no relation evaluated under it fails.
# Grouped by kind, since a confidence level and a tail are different choices.
variant_fits <- function(evaluations) {
  checked <- evaluations[evaluations$checked & !is.na(evaluations$variant), , drop = FALSE]
  lapply(stats::setNames(nm = c("tail", "level")), function(kind) {
    of_kind <- checked[checked$kind == kind, , drop = FALSE]
    variants <- unique(of_kind$variant)
    data.frame(
      variant = variants,
      fits = vapply(variants, function(v) all(of_kind$fits[of_kind$variant == v]), logical(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  })
}

level_words <- function(level) {
  words <- c(
    "Level 0: nothing could be checked with the reported numbers.",
    paste("Level 1: internal coherence only (statistic, df, p and CI are checked",
          "against each other, not against the descriptives)."),
    "Level 2: the test statistic was recomputed from the descriptives.",
    paste("Level 3: reproducible from what is reported (the numbers contain",
          "everything needed to reproduce the analysis; that is not itself a check).")
  )
  words[level + 1L]
}

#' @export
print.reprokit_check <- function(x, ...) {
  if (!x$checked) {
    cat("No check was possible with the reported statistics.\n")
  } else if (x$consistent) {
    cat("The analysis is consistent with the reported statistics.\n")
  } else {
    cat("The analysis is inconsistent with the reported statistics.\n")
  }
  cat(level_words(x$level), "\n", sep = "")
  for (kind in c("tail", "level")) {
    tried <- x$variants[[kind]]
    if (nrow(tried) == 0L) next
    fitting <- tried$variant[tried$fits]
    fitting_text <- if (length(fitting) > 0L) paste(fitting, collapse = ", ") else "none"
    label <- if (kind == "tail") "Variants that fit" else "Confidence levels that fit"
    cat(label, ": ", fitting_text, "\n", sep = "")
  }
  if (nrow(x$numbers) > 0L) {
    cat("\n")
    print(x$numbers, row.names = FALSE, na.print = "-")
  }
  if (length(x$implied) > 0L) {
    cat("\nImplied ranges for numbers that were not reported:\n")
    for (quantity in names(x$implied)) {
      cat("  ", quantity, ": ", format_implied(x$implied[[quantity]]), "\n", sep = "")
    }
  }
  if (nrow(x$missing) > 0L) {
    cat("\nReporting these would raise the level: ",
        paste0(x$missing$quantity, " (to level ", x$missing$would_reach_level, ")",
               collapse = ", "), ".\n", sep = "")
  }
  cat("\nAn inconsistency shows the numbers cannot all be correct as reported, ",
      "not which one is wrong or why.\n", sep = "")
  invisible(x)
}
