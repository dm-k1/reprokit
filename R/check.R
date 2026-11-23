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
#'   `"two.sided"`, `"0.95"` or `"welch"`), or `NULL` for all. Each value
#'   restricts only its own kind of variant (tail, confidence level, test,
#'   convention): `c("welch", "0.95")` fixes the test and the level. A kind
#'   that no value names keeps all its variants.
#' @param mu The test value the mean (or mean difference) is compared with, for
#'   handlers whose t depends on it (`t_one_sample`, `t_paired`). Default 0. A
#'   handler without a test value errors on `mu` other than 0.
#' @return A `reprokit_check` object: `$numbers` (status of each reported
#'   number), `$level` (coverage, 0 to 3), `$missing` (what would raise the
#'   level, and the range implied for it), `$variants` (a list with one data
#'   frame per kind of variant the handler has, each of the variants tried and
#'   whether they fit: `tail`, `level`, `test` (Student or Welch) and
#'   `convention` (how an effect size was computed)), `$relations` (one row per
#'   relation and variant; `$variant` and `$kind` join the values and kinds of a
#'   relation that varies over several with "+", and there is one column per
#'   kind, `tail`, `level`, `test`, `convention`, holding the row's value or
#'   `NA`), `$implied`,
#'   `$implied_inputs` (numbers derived from reported ones, such as `n` from `df`)
#'   and `$checked` (whether any relation that can falsify was compared with a
#'   reported number; without one there is no verdict).
#'
#'   The reported numbers come from one analysis, so they share one test and one
#'   confidence level: a variant fits only if one choice per kind explains every
#'   relation that can falsify. A CI whose bounds fit different levels, or a t that
#'   fits only Student next to a CI that fits only Welch, is inconsistent even
#'   though each relation holds on its own.
#'
#'   Some relations cannot falsify: how `d` was computed (pooled SD, average SD,
#'   Glass) is rarely declared, so a `d` that no convention reproduces gets the
#'   status `"not reproducible"`, never `"inconsistent"`, and `$consistent` is
#'   not affected.
#'
#'   A reported number is only ever compared within what it can be: `r` lies
#'   in -1 to 1, `sd` and `df` are not negative, `p` is in 0 to 1, `n` is a
#'   whole number. A relation whose result is not finite for the reported values
#'   (`"1.00"` for `r` gives an infinite t) is skipped, as if an input were
#'   missing; it never counts as a failure.
#'
#'   When `n` is not reported but `df` is, `n` is derived from `df` for the
#'   handlers where that is exact (a correlation, a paired or one-sample t): it
#'   is used by the other relations, shown as `$implied_inputs`, and counts for
#'   the coverage level, but `df` is never checked against it.
#' @export
#' @examples
#' check_reported("t_paired", n = "20", t = "2.85", df = "19", p = ".010")
check_reported <- function(handler, ..., variants = NULL, mu = 0) {
  if (missing(handler)) {
    stop("Name the handler first: check_reported(\"t_paired\", t = ...). Handlers that can ",
         "check: ", paste(handlers_with_relations(), collapse = ", "), ".", call. = FALSE)
  }
  entry <- get_handler(handler)
  entry$relations <- relations_at_mu(entry, mu)
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
  check_arity(reported, entry$cells, entry$per_cell)
  intervals <- Map(function(x, quantity) {
    as_count <- quantity %in% c("n", "df")
    restrict_to_domain(interval_bounds(reported_value(x, as_count = as_count)), quantity, x)
  }, reported, names(reported))
  implied_inputs <- derive_implied_inputs(entry$relations, intervals)
  evaluations <- evaluate_relations(entry$relations, intervals, variants, implied_inputs)

  number_status <- number_statuses(reported, evaluations, entry$relations, implied_inputs)
  known <- c(names(reported), names(implied_inputs))
  level <- coverage_level(entry, known, unique(evaluations$relation))
  implied <- implied_ranges(entry, intervals, evaluations)
  structure(list(
    handler = handler,
    numbers = number_status,
    level = level,
    missing = missing_numbers(entry, known, level, implied),
    variants = variant_fits(evaluations),
    relations = evaluations,
    implied = implied,
    implied_inputs = implied_inputs,
    checked = any(evaluations$checked & evaluations$falsifies),
    consistent = !any(number_status$status == "inconsistent")
  ), class = "reprokit_check")
}

#' @rdname check_reported
#' @export
check_t <- function(..., handler = "t_paired", variants = NULL, mu = 0) {
  check_reported(handler = handler, ..., variants = variants, mu = mu)
}

#' @rdname check_reported
#' @export
check_r <- function(..., handler = "cor_pearson", variants = NULL) {
  check_reported(handler = handler, ..., variants = variants)
}

# The handler's relations at the test value `mu`. A handler registered with
# `relations = function(mu)` builds them here; one with a fixed list has no test
# value, so any mu but 0 is an error rather than silently ignored.
relations_at_mu <- function(entry, mu) {
  if (!is.numeric(mu) || length(mu) != 1L || !is.finite(mu)) {
    stop("`mu` must be one finite number.", call. = FALSE)
  }
  if (!is.null(entry$relations_at)) return(entry$relations_at(mu))
  if (mu != 0) {
    stop("Handler '", entry$name, "' has no test value, so `mu` must be 0.", call. = FALSE)
  }
  entry$relations
}

handlers_with_relations <- function() {
  Filter(function(name) length(get_handler(name)$relations) > 0L, handlers())
}

# Inputs the paper did not report but a reported output determines exactly: for a
# relation with an `inverse` and one input (df from n), the integer inputs whose
# output lies in the reported output's interval. Returns a named list of
# list(lower, upper, from = output quantity); empty when the snapped interval is empty.
derive_implied_inputs <- function(relations, intervals) {
  implied <- list()
  for (rel in relations) {
    input <- rel$inputs[1L]
    if (is.null(rel$inverse) || input %in% names(intervals) || input %in% names(implied) ||
          !rel$output %in% names(intervals)) {
      next
    }
    target <- interval_bounds(intervals[[rel$output]])
    if (length(target$lower) != 1L) next
    lower <- rel$inverse(target$lower)
    upper <- rel$inverse(target$upper)
    if (!is.finite(lower) || !is.finite(upper)) next
    # Floating-point slack only, as in intervals_overlap(): the input is an integer.
    slack <- 1e-9 * max(1, abs(c(lower, upper)))
    lower <- ceiling(lower - slack)
    upper <- floor(upper + slack)
    if (lower <= upper) {
      implied[[input]] <- list(lower = lower, upper = upper, from = rel$output)
    }
  }
  implied
}

# A relation that computes the very quantity an implied input was derived from
# would check that quantity against itself; it never runs.
uses_own_output <- function(rel, implied) {
  any(vapply(rel$inputs, function(input) {
    identical(implied[[input]]$from, rel$output)
  }, logical(1)))
}

# One row per relation and variant whose inputs are all reported. `checked` says
# the output is reported too, so `fits` is a comparison; otherwise only the
# derived range is recorded.
evaluate_relations <- function(relations, intervals, variants, implied = list()) {
  known_variants <- unique(unlist(lapply(relations, `[[`, "variants")))
  unknown <- setdiff(variants, known_variants)
  if (length(unknown) > 0L) {
    stop("Unknown variant: ", paste0("'", unknown, "'", collapse = ", "),
         ". This handler has: ", paste(known_variants, collapse = ", "), ".", call. = FALSE)
  }
  rows <- list()
  # Shared by the variants of one call (never across calls): see sd_stationary().
  memo <- new.env(parent = emptyenv())
  # An unreported output becomes available as an input to later relations with
  # the range derived for it (so p can be checked from r and n through t). Such
  # a number counts as descriptive-based: it was derived from n, m, sd or r.
  # An implied input (n from df) is available too, but it is not in `intervals`,
  # so nothing is checked against it, and not in `basis`: it is not a descriptive.
  available <- c(intervals, lapply(implied, function(x) x[c("lower", "upper")]))
  basis <- intersect(names(intervals), descriptive_quantities())
  done <- character()
  derived_outputs <- character()
  repeat {
    progressed <- FALSE
    for (rel in relations) {
      if (rel$name %in% done || !all(rel$inputs %in% names(available)) ||
            uses_own_output(rel, implied)) {
        next
      }
      done <- c(done, rel$name)
      progressed <- TRUE
      first_row <- length(rows) + 1L
      for (variant in variant_combinations(restrict_variants(rel$variants, variants))) {
        derived <- relation_range(rel, available, variant, memo)
        # A range that is not finite (NaN or infinite) is "not evaluable", never
        # "does not fit": the whole relation is skipped as if an input were missing.
        # Not just that combination: with the others alone, a fit of the one that
        # could not be evaluated would be missed, and a derived output (df from n
        # and sd) would be narrower than the variants allow.
        if (!all(is.finite(derived))) {
          rows <- rows[seq_len(first_row - 1L)]
          break
        }
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
      if ((!rel$output %in% names(available) || rel$output %in% derived_outputs) &&
            length(rows) >= first_row) {
        # Hull over all combinations, so no variant is ruled out here. Relations
        # that share an output (the conventions of d, from different inputs) widen
        # it across all of them: the output fits if any of them fits.
        mine <- do.call(rbind, rows[first_row:length(rows)])
        lower <- min(mine$derived_lower)
        upper <- max(mine$derived_upper)
        if (rel$output %in% derived_outputs) {
          lower <- min(lower, available[[rel$output]]$lower)
          upper <- max(upper, available[[rel$output]]$upper)
        }
        available[[rel$output]] <- list(lower = lower, upper = upper)
        derived_outputs <- union(derived_outputs, rel$output)
        if (any(rel$inputs %in% basis)) basis <- union(basis, rel$output)
      }
    }
    if (!progressed) break
  }
  if (length(rows) == 0L) {
    kind_columns <- lapply(stats::setNames(nm = relation_variant_kinds()),
                           function(kind) character())
    head_columns <- list(relation = character(), variant = character(), kind = character())
    tail_columns <- list(output = character(), inputs = character(),
                         from_descriptives = logical(), falsifies = logical(),
                         checked = logical(), fits = logical(),
                         derived_lower = numeric(), derived_upper = numeric(),
                         stringsAsFactors = FALSE)
    return(do.call(data.frame, c(head_columns, kind_columns, tail_columns)))
  }
  do.call(rbind, rows)
}

# `variants` of a relation restricted to the values the user named: each value
# restricts only its own kind, and a kind with none of the named values keeps all.
restrict_variants <- function(variants, wanted) {
  if (length(variants) == 0L) return(NULL)
  lapply(variants, function(values) {
    keep <- intersect(values, wanted)
    if (length(keep) > 0L) keep else values
  })
}

# The value of each kind for one combination, as a named character vector (empty
# for a relation without variants). `combo` is what `fn` receives.
variant_values <- function(rel, combo) {
  if (is.null(combo)) return(character())
  if (is.null(names(combo))) stats::setNames(combo, names(rel$variants)) else combo
}

# One row per relation and combination of variants. `variant` and `kind` join the
# values and kinds with "+", in the same order; one column per kind holds the
# row's value or NA.
evaluation_row <- function(rel, combo, checked, fits, derived, from_descriptives) {
  values <- variant_values(rel, combo)
  per_kind <- lapply(stats::setNames(nm = relation_variant_kinds()), function(kind) {
    if (kind %in% names(values)) values[[kind]] else NA_character_
  })
  variant_label <- if (length(values) > 0L) paste(values, collapse = "+") else NA_character_
  kind_label <- if (length(values) > 0L) paste(names(values), collapse = "+") else NA_character_
  head_columns <- list(relation = rel$name, variant = variant_label, kind = kind_label)
  tail_columns <- list(output = rel$output, inputs = paste(rel$inputs, collapse = ", "),
                       from_descriptives = from_descriptives, falsifies = rel$falsifies,
                       checked = checked, fits = fits,
                       derived_lower = derived[["lower"]], derived_upper = derived[["upper"]],
                       stringsAsFactors = FALSE)
  do.call(data.frame, c(head_columns, per_kind, tail_columns))
}

# A relation holds when at least one of its variants fits. That is the relation
# alone; shared_choice() adds the requirement that the relations agree on one
# choice per kind.
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
#   inconsistent  - involved in a relation that fails under every variant, or in a
#                   set of relations that no single choice of test, level and
#                   tail satisfies at once (we cannot tell which of its numbers
#                   is the wrong one). Relations that cannot falsify
#                   (`falsifies = FALSE`) never count here.
#   not reproducible - the output of such a relation, and none of its variants
#                   (conventions) reproduces it; a remark, not an accusation
#   verified      - the OUTPUT of a holding relation computed from descriptives
#                   (n, m, sd, r), reported or derived from reported numbers,
#                   i.e. recomputed from more basic numbers
#   coherent      - holds, but only as an input of such a relation, or through
#                   relations among derived numbers
#   unconfirmed   - none of the above, but it belongs to a relation whose output
#                   is reported and which could not be evaluated because another
#                   input is missing; `checked_against` says which
#   not checkable - no relation reaches it
number_statuses <- function(reported, evaluations, relations, implied = list()) {
  outcomes <- relation_outcomes(evaluations)
  # Relations of a component with no fitting joint choice: each holds alone, but
  # no single analysis produces them all, so none counts as holding.
  jointly_failing <- unlist(lapply(shared_choice(evaluations), function(component) {
    if (any(component$fits)) NULL else component$relations
  }))
  detail <- unique(evaluations[evaluations$checked,
                               c("relation", "output", "inputs", "from_descriptives",
                                 "falsifies")])
  # What an unevaluated relation lacks: inputs neither reported nor derived.
  have <- c(names(reported), names(implied), evaluations$output)
  stalled <- Filter(function(rel) {
    !rel$name %in% evaluations$relation && rel$output %in% names(reported) &&
      !uses_own_output(rel, implied)
  }, relations)
  per_quantity <- lapply(names(reported), function(quantity) {
    involved <- detail[detail$output == quantity |
                         vapply(strsplit(detail$inputs, ", "), function(i) quantity %in% i,
                                logical(1)), , drop = FALSE]
    holds <- outcomes$holds[match(involved$relation, outcomes$relation)] &
      !involved$relation %in% jointly_failing
    # A failing relation that cannot falsify is set aside: it neither accuses
    # nor confirms. `unreproduced` is what it says about its own output.
    unreproduced <- involved[!holds & !involved$falsifies & involved$output == quantity, ,
                             drop = FALSE]
    keep <- holds | involved$falsifies
    involved <- involved[keep, , drop = FALSE]
    holds <- holds[keep]
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
    } else if (nrow(unreproduced) > 0L) {
      list(status = "not reproducible",
           against = paste0("no ", variant_kind_noun(unreproduced$relation, relations),
                            " tried reproduces it (", paste(unreproduced$relation, collapse = "; "),
                            ")"))
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

# The kind of variant of the named relations, in words ("convention", "test").
variant_kind_noun <- function(relation_names, relations) {
  kinds <- unique(unlist(lapply(Filter(function(rel) rel$name %in% relation_names, relations),
                                function(rel) names(rel$variants))))
  if (length(kinds) == 1L) kinds else "variant"
}

statistic_quantities <- function() c("t", "f", "chisq")

# Level from which quantities are reported (ADR 0010 decision 2), by the highest
# one reached:
#   3  the reported set contains the handler's `sufficient` set
#   2  some statistic (t, F, chi-square) can be recomputed from descriptives
#   1  some relation has all its numbers, output included, reported (and could
#      be evaluated)
#   0  none
# The level describes coverage, not agreement; consistency is reported apart.
# `evaluable` names the relations that could be evaluated, or NULL for all.
coverage_level <- function(handler, reported_names, evaluable = NULL) {
  if (!is.null(handler$sufficient) && all(handler$sufficient %in% reported_names)) {
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
    # A relation that cannot falsify (the d convention) says nothing about a
    # missing input: which convention was used is unknown, so ambiguity resolves
    # towards silence.
    if (!rel$falsifies) next
    # The variants are alternatives (one of them was used), so the missing input
    # may lie in the union of their ranges; relations are all true, so they intersect.
    # This does not apply the shared-choice rule of shared_choice() (one test and
    # one level across relations): it only ever widens the implied range, which
    # can keep a number consistent but never flag one, so ambiguity resolves
    # towards silence. Only the correlation CIs reach it, and they vary over level
    # alone.
    found <- NULL
    for (variant in variant_combinations(rel$variants)) {
      one <- solve_for(rel, intervals, quantity, intervals[[rel$output]],
                       solvable_bounds()[[quantity]], variant)
      found <- if (is.null(found)) one else union_implied(found, one)
    }
    previous <- implied[[quantity]]
    implied[[quantity]] <- if (is.null(previous)) found else intersect_implied(previous, found)
  }
  implied
}

# The union of two feasible sets, as their hull (the grid scan gives intervals; a
# hull is wider, never narrower, so it can only keep a number consistent).
union_implied <- function(first, second) {
  if (!first$feasible) return(second)
  if (!second$feasible) return(first)
  list(lower = min(first$lower, second$lower), upper = max(first$upper, second$upper),
       feasible = TRUE,
       reaches_lower = (first$lower <= second$lower && first$reaches_lower) ||
         (second$lower <= first$lower && second$reaches_lower),
       reaches_upper = (first$upper >= second$upper && first$reaches_upper) ||
         (second$upper >= first$upper && second$reaches_upper),
       resolution = first$resolution)
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
  candidates <- setdiff(union(handler$apa, handler$sufficient), reported_names)
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

# The relations that can falsify, joined by the kinds of variant they share.
# Reported numbers come from one analysis, so they share one test and one level:
# letting each relation choose its variant separately accepts results that no
# single analysis produces (the lower CI bound at 90% and the upper at 99%; a t
# that only Student reproduces next to a CI that only Welch does).
# Two kinds share a component when some falsifying, checked relation varies over
# both (for the independent t: test and level, through the CI; the tail stands
# alone through p_from_t). Per component, an assignment (one value per kind) fits
# when every relation of the component fits under some row that agrees with the
# assignment on the kinds that relation varies over. Only relations that fit
# on their own take part: one that fails under every choice is already
# inconsistent, and letting it veto the joint choice would mark the numbers of
# the relations that hold (a correct lower bound, t) inconsistent with it.
# Relations that cannot falsify (the d conventions) never take part. Returns a
# list of components, each list(kinds, relations (the holding ones),
# assignments (a data frame), fits (logical)).
shared_choice <- function(evaluations) {
  rows <- evaluations[evaluations$checked & evaluations$falsifies, , drop = FALSE]
  if (nrow(rows) == 0L) return(list())
  kinds_of <- lapply(split(rows, rows$relation), function(r) {
    Filter(function(kind) any(!is.na(r[[kind]])), relation_variant_kinds())
  })
  kinds_of <- Filter(function(kinds) length(kinds) > 0L, kinds_of)
  groups <- list()
  for (kinds in kinds_of) {
    linked <- vapply(groups, function(g) any(g %in% kinds), logical(1))
    groups <- c(groups[!linked], list(union(unlist(groups[linked]), kinds)))
  }
  lapply(groups, function(group) {
    group <- intersect(relation_variant_kinds(), group)
    relations <- names(kinds_of)[vapply(kinds_of, function(k) all(k %in% group), logical(1))]
    mine <- rows[rows$relation %in% relations, , drop = FALSE]
    values <- lapply(stats::setNames(nm = group), function(kind) {
      v <- mine[[kind]]
      unique(v[!is.na(v)])
    })
    assignments <- expand.grid(values, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    holding <- relations[vapply(relations, function(relation) {
      any(mine$fits[mine$relation == relation])
    }, logical(1))]
    agrees <- function(i, relation) {
      fitting <- mine[mine$relation == relation & mine$fits, , drop = FALSE]
      any(vapply(seq_len(nrow(fitting)), function(j) {
        all(vapply(kinds_of[[relation]], function(kind) {
          identical(fitting[[kind]][j], assignments[[kind]][i])
        }, logical(1)))
      }, logical(1)))
    }
    fits <- vapply(seq_len(nrow(assignments)), function(i) {
      length(holding) > 0L && all(vapply(holding, agrees, logical(1), i = i))
    }, logical(1))
    list(kinds = group, relations = holding, assignments = assignments, fits = fits)
  })
}

# Which variants fit, by kind (a confidence level, a tail, a test and a
# convention are different choices). For a kind that takes part in a falsifying
# relation a value fits when some fitting assignment of its component uses it
# (shared_choice()); when the component has no fitting assignment no value fits.
# A kind only the d conventions have is judged value by value: a convention fits
# when every relation evaluated under it fits.
variant_fits <- function(evaluations) {
  checked <- evaluations[evaluations$checked & !is.na(evaluations$variant), , drop = FALSE]
  # tail and level are always present (possibly empty); test and convention
  # only for handlers that have them.
  present <- Filter(function(kind) any(!is.na(checked[[kind]])), relation_variant_kinds())
  kinds <- intersect(relation_variant_kinds(), c("tail", "level", present))
  components <- shared_choice(evaluations)
  lapply(stats::setNames(nm = kinds), function(kind) {
    component <- Filter(function(c) kind %in% c$kinds, components)
    if (length(component) > 0L) {
      component <- component[[1L]]
      variants <- unique(component$assignments[[kind]])
      fits <- vapply(variants, function(v) {
        any(component$fits & component$assignments[[kind]] == v)
      }, logical(1))
    } else {
      values <- checked[[kind]]
      variants <- unique(values[!is.na(values)])
      fits <- vapply(variants, function(v) {
        all(checked$fits[!is.na(values) & values == v])
      }, logical(1))
    }
    data.frame(variant = variants, fits = unname(fits), stringsAsFactors = FALSE,
               row.names = NULL)
  })
}

format_implied_input <- function(found) {
  if (found$lower == found$upper) format(found$lower) else paste(found$lower, "to", found$upper)
}

level_words <- function(level) {
  words <- c(
    "Level 0: nothing could be checked with the reported numbers.",
    paste("Level 1: internal coherence only (statistic, df, p and CI are checked",
          "against each other, not against the descriptives)."),
    "Level 2: the test statistic was recomputed from the descriptives.",
    paste("Level 3: the sufficient set is reported (the numbers contain",
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
  for (kind in names(x$variants)) {
    tried <- x$variants[[kind]]
    if (nrow(tried) == 0L) next
    fitting <- tried$variant[tried$fits]
    fitting_text <- if (length(fitting) > 0L) {
      paste(variant_label(fitting), collapse = ", ")
    } else {
      "none"
    }
    cat(variant_kind_labels()[[kind]], ": ", fitting_text, "\n", sep = "")
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
  if (length(x$implied_inputs) > 0L) {
    values <- vapply(x$implied_inputs, format_implied_input, character(1))
    sources <- vapply(x$implied_inputs, `[[`, character(1), "from")
    cat("\nDerived from the reported numbers: ",
        paste0(names(x$implied_inputs), " = ", values, " (from ", sources, ")",
               collapse = ", "), ".\n", sep = "")
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
