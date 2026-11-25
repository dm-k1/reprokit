# The range engine: one handler's relations against the reported numbers.
check_handler <- function(handler, ..., variants = NULL, mu = 0, dims = NULL, p0 = NULL,
                           effect = NULL, k = NULL) {
  entry <- get_handler(handler)
  if ("effect" %in% entry$test_args && is.null(effect)) {
    abort_input("A factorial ANOVA is checked one effect at a time: name it with ",
                "`effect = \"A\"`, `\"B\"` or `\"AB\"`.")
  }
  args <- test_arguments(mu = mu, dims = dims, p0 = p0, effect = effect, k = k)
  dots <- list(...)
  has_terms <- !is.null(entry$design$terms)
  if (has_terms) {
    # A regression names its coefficients ("b:wt"): the relations are built for them.
    coefficient_names <- names(dots)[quantity_base(names(dots)) %in% coefficient_quantities()]
    found <- unique(sub("^[^:]*:", "", coefficient_names[grepl(":", coefficient_names)]))
    if (length(found) > 0L) args$terms <- found
  }
  entry$relations <- relations_at(entry, args)
  if (length(entry$relations) == 0L) {
    abort_input("reprokit has no relations to check ", analysis_name(entry),
                " against reported numbers. Tests it can check: ",
                paste(names(check_tests()), collapse = ", "), ".")
  }
  reported <- dots
  if (length(reported) > 0L && (is.null(names(reported)) || any(!nzchar(names(reported))))) {
    abort_input("Every reported number must be named by its quantity, for example t = \"2.85\".")
  }
  check_quantity_names(names(reported), "reported quantity", terms = has_terms)
  check_arity(reported, entry$cells, entry$per_cell)
  intervals <- Map(function(x, quantity) {
    as_count <- quantity_base(quantity) %in% c("n", "df", "df1", "df2", "n_total")
    restrict_to_domain(interval_bounds(reported_value(x, as_count = as_count)), quantity, x,
                       entry$domains)
  }, reported, names(reported))
  notes <- character()
  if (has_terms && "k" %in% entry$test_args) {
    # k is the model's df1: read it when F's df1 is reported exactly, else it stays
    # unknown (never the number of terms reported).
    if (is.null(k) && !is.null(intervals$df1)) {
      bounds <- interval_bounds(intervals$df1)
      if (length(bounds$lower) == 1L && bounds$lower == bounds$upper && bounds$lower >= 1) {
        args$k <- bounds$lower
        entry$relations <- relations_at(entry, args)
      }
    }
    if (is.null(args$k) && any(c("n", "df2") %in% names(reported))) {
      notes <- paste("n and df2 are not checked against each other: that needs k, the number",
                     "of predictor columns. Give `k` or report df1.")
    }
  }
  # A balanced design has one SS type: the handler drops the variant when every cell
  # size is reported exactly and equal. Anything less certain keeps the variants.
  if ("balanced" %in% entry$test_args && is_balanced_report(intervals$n)) {
    entry$relations <- relations_at(entry, c(args, list(balanced = TRUE)))
  }
  implied_inputs <- derive_implied_inputs(entry$relations, intervals)
  evaluations <- evaluate_relations(entry$relations, intervals, variants, implied_inputs)

  number_status <- number_statuses(reported, evaluations, entry$relations, implied_inputs)
  # An H0 proportions vector the user gave is as good as a reported one for the
  # level; the uniform default is not (ADR 0016 point 7).
  known <- c(names(reported), names(implied_inputs), if (!is.null(p0)) "p0")
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
    notes = c(notes, convention_notes(number_status)),
    consistent = !any(number_status$status == "inconsistent")
  ), class = "reprokit_check")
}

# TRUE when n is reported for every cell, each exactly (a count) and all equal.
is_balanced_report <- function(n) {
  if (is.null(n)) return(FALSE)
  bounds <- interval_bounds(n)
  length(bounds$lower) > 1L && all(bounds$lower == bounds$upper) &&
    length(unique(bounds$lower)) == 1L
}

# The handler's relations at the test arguments `args` (a named list of plain
# numbers: `mu` for the t handlers, `dims` and `p0` for the chi-squared handlers; the
# factorial ANOVA also takes `effect`, a string, and the internal flag `balanced`).
# A handler registered with `relations = function(<its test arguments>)` builds
# them here; one with a fixed list has no test arguments. An argument the handler
# does not declare is an error rather than silently ignored.
relations_at <- function(entry, args = list()) {
  for (name in names(args)) {
    value <- args[[name]]
    if (name == "effect") {
      if (!(is.character(value) && length(value) == 1L && value %in% c("A", "B", "AB"))) {
        abort_input("`effect` must be one of \"A\", \"B\" or \"AB\".")
      }
    } else if (name == "terms") {
      if (!(is.character(value) && length(value) >= 1L && !anyNA(value) && all(nzchar(value)))) {
        abort_input("`terms` must be coefficient names.")
      }
    } else if (name == "k") {
      if (!(is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 1 &&
              value == round(value))) {
        abort_input("`k` must be one whole number, at least 1 (the number of predictor columns).")
      }
    } else if (name == "balanced") {
      if (!(is.logical(value) && length(value) == 1L && !is.na(value))) {
        abort_input("`balanced` must be TRUE or FALSE.")
      }
    } else if (!is.numeric(value) || length(value) < 1L || any(!is.finite(value))) {
      abort_input("`", name, "` must be ", if (name == "mu") "one finite number" else
             "finite numbers", ".")
    }
    if (name == "mu" && length(value) != 1L) abort_input("`mu` must be one finite number.")
    if (!name %in% entry$test_args) {
      abort_input("`", name, "` does not apply to ", analysis_name(entry),
           if (length(setdiff(entry$test_args, "terms")) > 0L) {
             paste0("; it takes ", paste0("`", setdiff(entry$test_args, "terms"), "`",
                                          collapse = ", "))
           } else {
             " (it takes none)"
           }, ".")
    }
  }
  if (is.null(entry$relations_at)) return(entry$relations)
  do.call(entry$relations_at, args)
}

# The test's plain name for messages, from the handler's `analysis` label without its
# parenthetical: "Independent-samples t-test (Student, Welch)" -> "independent-samples t-test".
# A name (Pearson, Student, Welch) or an acronym keeps its capital.
analysis_name <- function(handler) {
  x <- sub(" \\(.*$", "", handler$analysis)
  if (grepl("^[A-Z][a-z]", x) && !grepl("^(Pearson|Student|Welch)", x)) {
    x <- paste0(tolower(substr(x, 1L, 1L)), substring(x, 2L))
  }
  x
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
    abort_input("Unknown variant: ", paste0("'", unknown, "'", collapse = ", "),
         ". This test has: ", paste(known_variants, collapse = ", "), ".")
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
# A number also counts as involved in a checked relation when it feeds that
# relation through unchecked ones: with r, df and p reported, t is derived from
# r and df (t_from_r, not checked since t is not reported) and then compared
# with p (p_from_t). r reaches p through that chain, so it is coherent, not
# "not checkable". Such relations are named in `checked_against`.
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
  # Unchecked relations that were evaluated: their outputs are derived numbers.
  bridges <- unique(evaluations[!evaluations$checked, c("relation", "output", "inputs")])
  bridges <- bridges[!bridges$relation %in% detail$relation, , drop = FALSE]
  # Inputs of a checked relation, followed back through unchecked relations
  # whose output feeds it. `seen` guards against cycles.
  expand_inputs <- function(inputs) {
    via <- character()
    repeat {
      feeding <- bridges[bridges$output %in% inputs & !bridges$relation %in% via, ,
                         drop = FALSE]
      if (nrow(feeding) == 0L) break
      via <- c(via, feeding$relation)
      inputs <- union(inputs, unlist(strsplit(feeding$inputs, ", ")))
    }
    list(inputs = inputs, via = via)
  }
  detail$inputs_all <- detail$inputs
  detail$via <- rep("", nrow(detail))
  for (i in seq_len(nrow(detail))) {
    expanded <- expand_inputs(strsplit(detail$inputs[i], ", ")[[1L]])
    detail$inputs_all[i] <- paste(expanded$inputs, collapse = ", ")
    detail$via[i] <- paste(expanded$via, collapse = ", ")
  }
  per_quantity <- lapply(names(reported), function(quantity) {
    involved <- detail[detail$output == quantity |
                         vapply(strsplit(detail$inputs_all, ", "),
                                function(i) quantity %in% i, logical(1)), , drop = FALSE]
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
      shown <- ifelse(nzchar(involved$via) & involved$output != quantity,
                      paste0(involved$relation, " (via ", involved$via, ")"), involved$relation)
      list(status = status, against = paste(shown, collapse = "; "))
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

# The per-coefficient quantities a regression check reads names for.
coefficient_quantities <- function() c("b", "se", "t", "df", "p", "ci_lower", "ci_upper", "beta")

# What a convention that did not fit means (ADR 0020 point 6): said once, in words.
convention_notes <- function(numbers) {
  bad <- numbers$status == "not reproducible"
  c(if (any(bad & quantity_base(numbers$quantity) == "beta")) {
    paste("beta is not evaluable as labelled: papers write beta for unstandardised b and the",
          "other way round, so a beta that does not equal b * SD_x / SD_y is not an error.")
  })
}

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
    # R squared = r^2 cannot say the sign of r: no range for r from it.
    if (rel$name == "r2_from_r") next
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
  candidates <- union(handler$apa, handler$sufficient)
  if (!is.null(handler$design$terms)) {
    # A coefficient's number is missing per coefficient: "se:wt", not "se".
    coefficient_names <- reported_names[quantity_base(reported_names) %in%
                                          coefficient_quantities() &
                                          grepl(":", reported_names, fixed = TRUE)]
    found <- unique(sub("^[^:]*:", "", coefficient_names))
    candidates <- unlist(lapply(candidates, function(q) {
      if (q %in% coefficient_quantities()) paste0(q, ":", found) else q
    }))
  }
  candidates <- setdiff(candidates, reported_names)
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
