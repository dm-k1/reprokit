# The equations between reported quantities, and how to evaluate them on intervals.
# In one line: a relation computes one output quantity from input quantities;
# evaluated over the rounding intervals of its inputs it gives the range the
# output may have, and solve_for() inverts that to the range a missing input may have.

# `fn(vals, variant)` takes a named list of plain numbers (a quantity with one
# value per condition is a vector) and returns one number. A relation has
# exactly one scalar output; a CI pair is two relations, one per bound.
# `variants` names the alternatives the author must have chosen between, as a
# named list with one entry per KIND of variant: "tail" (the sidedness of a
# p-value), "level" (a confidence level), "test" (Student or Welch) or
# "convention" (which formula an effect size follows). A relation may vary over
# several kinds at once (the independent CI: `list(test = ..., level = ...)`),
# and every combination of one value per kind is a variant of it. For a relation
# of a single kind the old form is accepted too: a character vector plus
# `variant_kind`. Either way it is stored as the named list.
# `fn` receives the current variant: the plain string for a relation of one kind,
# a named character vector (`variant[["test"]]`, `variant[["level"]]`) for a
# crossed one. check_reported() additionally requires that the numbers it reports
# are explained by ONE choice per kind across all relations that can falsify
# (see shared_choice() in check.R).
# `falsifies` says whether a failure may count as an inconsistency. It is FALSE
# for a relation whose formula authors choose freely and rarely declare (the
# d convention): when none of its variants fits, the number is "not
# reproducible", never "inconsistent".
# `inverse(output_value)` is optional: for a relation with a single, integer-valued input
# (df from n) it returns the input that gives that output, increasing in the output. When the
# input is not reported but the output is, check_reported() derives the input from it (an
# "implied" input, usable by other relations but never compared with a reported number).
# `stationary(intervals)` or `stationary(intervals, variant)` is optional: it
# returns a list of extra points, each a named list of input values inside the
# box, where `fn` can have an interior extremum that the corners miss (see
# relation_range()). With two formals it receives the current variant, for
# extrema that move with it (the CI's, with the level). A third formal, `memo`,
# receives an environment that check_reported() creates for one call, so a
# stationary search that several variants share is done once (see sd_stationary()).
# `range(intervals, variant)` is optional: a function returning c(lower =, upper =), the
# exact range of the output over the box. When given, relation_range() uses it instead of
# corners and `stationary`, for relations whose extremes have no corner structure or whose
# inputs have too many values for 2^k corners (the one-way F over k groups).
relation <- function(name, inputs, output, fn, variants = NULL, variant_kind = "tail",
                     stationary = NULL, falsifies = TRUE, inverse = NULL, range = NULL) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name),
            is.character(inputs), length(inputs) >= 1L,
            is.character(output), length(output) == 1L,
            is.function(fn), identical(names(formals(fn)), c("vals", "variant")),
            is.null(stationary) ||
              (is.function(stationary) && length(formals(stationary)) %in% 1:3),
            is.logical(falsifies), length(falsifies) == 1L, !is.na(falsifies),
            is.null(inverse) || (is.function(inverse) && length(formals(inverse)) == 1L &&
                                   length(inputs) == 1L),
            is.null(range) ||
              (is.function(range) && identical(names(formals(range)), c("intervals", "variant"))))
  variants <- normalise_variants(variants, variant_kind)
  check_quantity_names(c(inputs, output), "quantity name in relation")
  list(name = name, inputs = inputs, output = output, fn = fn, variants = variants,
       stationary = stationary, falsifies = falsifies, inverse = inverse, range = range)
}

# `variants` as a named list, one character vector per kind (or NULL).
normalise_variants <- function(variants, variant_kind = "tail") {
  if (is.null(variants)) return(NULL)
  if (!is.list(variants)) {
    stopifnot(is.character(variants), length(variants) >= 1L,
              is.character(variant_kind), length(variant_kind) == 1L)
    variants <- stats::setNames(list(variants), variant_kind)
  }
  valid_values <- function(v) {
    is.character(v) && length(v) >= 1L && !anyNA(v) && !anyDuplicated(v)
  }
  stopifnot(length(variants) >= 1L, !is.null(names(variants)), !anyNA(names(variants)),
            !anyDuplicated(names(variants)),
            all(names(variants) %in% relation_variant_kinds()),
            all(vapply(variants, valid_values, logical(1))))
  variants[intersect(relation_variant_kinds(), names(variants))]
}

# Every combination of one value per kind, as a list of named character vectors
# (a plain string for a relation of one kind, as `fn` receives it); list(NULL)
# for a relation without variants.
variant_combinations <- function(variants) {
  if (is.null(variants)) return(list(NULL))
  grid <- expand.grid(variants, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(grid)), function(i) {
    combo <- vapply(grid, function(column) column[i], character(1))
    if (length(combo) == 1L) unname(combo) else combo
  })
}

# The kinds of variant, in the order they are listed, with the words used for
# the ones that fit.
relation_variant_kinds <- function() c("tail", "level", "test", "convention")

variant_kind_labels <- function() {
  c(tail = "Variants that fit", level = "Confidence levels that fit",
    test = "Tests that fit", convention = "Effect-size conventions that fit")
}

# The confidence levels a CI relation offers, as variant codes.
confidence_levels <- function() c("0.90", "0.95", "0.99")

# One readable label per variant code, shared by handler_choices() (what a paper
# should declare) and the print of a check (which variants fit). Codes are unique
# across kinds. A code without a label is a programming error, caught by
# variant_label() and by a test over every registered relation.
variant_labels <- function() {
  c(two.sided = "two-sided", less = "one-sided (less)", greater = "one-sided (greater)",
    "0.90" = "90%", "0.95" = "95%", "0.99" = "99%",
    student = "Student's t", welch = "Welch's t",
    pooled = "Cohen's d (pooled SD)", hedges_g = "Hedges' g",
    average = "Cohen's d (root mean square of the SDs)",
    glass_sd1 = "Glass's \u0394 (SD of the first group)",
    glass_sd2 = "Glass's \u0394 (SD of the second group)",
    cohen = "Cohen's d",
    d_z = "d_z (SD of the differences)", d_rm = "d_rm (repeated measures)",
    d_av = "d_av (average SD)", g_av = "g_av (average SD, Hedges' correction)",
    eta2 = "eta squared", omega2 = "omega squared",
    III = "type III", II = "type II", I = "type I",
    partial_eta2 = "partial eta squared", partial_omega2 = "partial omega squared",
    r2 = "R\u00b2", adj_r2 = "adjusted R\u00b2",
    pearson = "Pearson chi-squared", yates = "Yates' continuity correction",
    v = "Cram\u00e9r's V", w = "Cohen's w")
}

# The SS types of a factorial ANOVA, in the order they are tried; the `test` variants
# of its F and p.
ss_type_codes <- function() c("III", "II", "I")

variant_label <- function(codes) {
  labels <- variant_labels()
  unknown <- setdiff(codes, names(labels))
  if (length(unknown) > 0L) {
    stop("No label for variant: ", paste0("'", unknown, "'", collapse = ", "),
         ". Add it to variant_labels().", call. = FALSE)
  }
  unname(labels[codes])
}

is_relation <- function(x) {
  is.list(x) && identical(names(x), c("name", "inputs", "output", "fn", "variants",
                                      "stationary", "falsifies", "inverse", "range"))
}

# Slack for floating-point error only (1e-9 of the magnitude); the rounding
# of reported numbers is handled by the intervals themselves.
intervals_overlap <- function(lower_a, upper_a, lower_b, upper_b) {
  slack <- 1e-9 * max(1, abs(c(lower_a, upper_a, lower_b, upper_b)))
  isTRUE(lower_a <= upper_b + slack && lower_b <= upper_a + slack)
}

max_free_dimensions <- 14L

# Every corner of the box spanned by the intervals of `inputs`, as a list of
# named lists of input values. Exact inputs (lower == upper) add no dimension;
# every other value of a vector quantity adds one, and above 2^14 corners we stop.
corner_points <- function(intervals, inputs, what = "relation") {
  bounds <- lapply(intervals[inputs], interval_bounds)
  lows <- lapply(bounds, `[[`, "lower")
  lower <- unlist(lows, use.names = FALSE)
  upper <- unlist(lapply(bounds, `[[`, "upper"), use.names = FALSE)
  free <- which(lower != upper)
  nfree <- length(free)
  if (nfree > max_free_dimensions) {
    stop("Relation '", what, "' has ", nfree, " non-exact input values, which means ",
         2^nfree, " corners; the limit is 2^", max_free_dimensions, ".", call. = FALSE)
  }
  # Corner k takes the upper bound of free value d when bit d of k - 1 is set, the
  # order expand.grid() gives (the first free value varies fastest).
  corners <- if (nfree == 0L) {
    matrix(numeric(), nrow = 1L, ncol = 0L)
  } else {
    k <- seq_len(2^nfree) - 1L
    upper_side <- vapply(seq_len(nfree), function(d) bitwAnd(k, bitwShiftL(1L, d - 1L)) > 0L,
                         logical(length(k)))
    out <- matrix(lower[free], nrow = length(k), ncol = nfree, byrow = TRUE)
    out[upper_side] <- matrix(upper[free], nrow = length(k), ncol = nfree, byrow = TRUE)[upper_side]
    out
  }
  ends <- cumsum(lengths(lows))
  at <- Map(function(from, to) seq_len(to - from + 1L) + from - 1L, ends - lengths(lows) + 1L, ends)
  names(at) <- inputs
  lapply(seq_len(nrow(corners)), function(i) {
    point <- lower
    point[free] <- corners[i, ]
    lapply(at, function(ix) point[ix])
  })
}

# Range of the output over the box of the input intervals: `fn` evaluated at
# every corner and at the relation's `stationary` points, then min and max. For
# a smooth `fn` the extremes over a box lie at corners or at interior
# stationary points of `fn` restricted to a face of the box, so the range is
# exact once `stationary` lists those points. Without `stationary` the corners
# alone are exact only if `fn` is monotone in each input over the box. A relation
# with its own `range` skips all of that (and the corner cap).
relation_range <- function(rel, intervals, variant = NULL, memo = NULL) {
  absent <- setdiff(rel$inputs, names(intervals))
  if (length(absent) > 0L) {
    stop("Relation '", rel$name, "' needs ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(rel$range)) {
    out <- rel$range(intervals, variant)
    return(c(lower = out[["lower"]], upper = out[["upper"]]))
  }
  points <- corner_points(intervals, rel$inputs, rel$name)
  if (!is.null(rel$stationary)) {
    extra <- switch(length(formals(rel$stationary)),
                    rel$stationary(intervals),
                    rel$stationary(intervals, variant),
                    rel$stationary(intervals, variant, memo))
    points <- c(points, extra)
  }
  outputs <- vapply(points, function(point) rel$fn(point, variant), numeric(1))
  c(lower = min(outputs), upper = max(outputs))
}

# Interior extremum candidates of a one-dimensional function on (lower, upper):
# where optimize() puts its minimum and its maximum. A relation's `stationary`
# uses these when the function is smooth but has no closed-form stationary
# point. Over an interval as narrow as a rounding interval the function has at
# most one extremum, so optimize() finds it; the candidates are only points of
# the box, evaluated by the relation itself, so a poor candidate costs
# exactness, never correctness.
interior_extrema <- function(f, lower, upper) {
  tol <- (upper - lower) * 1e-6
  c(stats::optimize(f, c(lower, upper), tol = tol)$minimum,
    stats::optimize(f, c(lower, upper), tol = tol, maximum = TRUE)$maximum)
}

# A `stationary` function for relations whose extremes can lie inside the box
# of the SDs. `find(corner, i, lower, upper)` returns the values of sd[i]
# (other inputs as in `corner`) that may be extrema; a `find` with a fifth
# formal, `variant`, also receives the current variant. On every corner the
# candidates for one SD are tried alone and then together with candidates for
# the other SD. A `find` may carry an attribute `variant_key`, the names of the
# variant entries its candidates depend on; given a `memo` environment, the
# candidates are then computed once per distinct (corner, SD, box, key) and
# reused by the other variants (the Welch search for the CI does not depend on
# the test, so the Student combinations reuse it). An attribute `ignores` names
# the inputs its candidates do not depend on (the CI's extremum in an SD does not
# depend on the means: they only shift the bound), so corners that differ in
# those alone share one search.
sd_stationary <- function(inputs, what, find) {
  with_variant <- length(formals(find)) >= 5L
  key_names <- attr(find, "variant_key")
  key_ignores <- attr(find, "ignores")
  function(intervals, variant = NULL, memo = NULL) {
    candidates <- if (with_variant) {
      function(corner, i, lower, upper) find(corner, i, lower, upper, variant)
    } else {
      find
    }
    if (!is.null(memo) && !is.null(key_names)) {
      uncached <- candidates
      candidates <- function(corner, i, lower, upper) {
        keyed <- unlist(corner[setdiff(names(corner), key_ignores)])
        key <- paste(c(what, i, sprintf("%.17g", keyed),
                       sprintf("%.17g", c(lower, upper)), variant[key_names]),
                     collapse = "|")
        if (is.null(memo[[key]])) memo[[key]] <- uncached(corner, i, lower, upper)
        memo[[key]]
      }
    }
    bounds <- interval_bounds(intervals$sd)
    free <- which(bounds$lower < bounds$upper)
    inside <- function(x, i) isTRUE(x > bounds$lower[i] && x < bounds$upper[i])
    points <- list()
    for (corner in corner_points(intervals, inputs, what)) {
      for (i in free) {
        for (x in candidates(corner, i, bounds$lower[i], bounds$upper[i])) {
          if (!inside(x, i)) next
          first <- corner
          first$sd[i] <- x
          points[[length(points) + 1L]] <- first
          for (j in setdiff(free, i)) {
            for (y in candidates(first, j, bounds$lower[j], bounds$upper[j])) {
              if (!inside(y, j)) next
              second <- first
              second$sd[j] <- y
              points[[length(points) + 1L]] <- second
            }
          }
        }
      }
    }
    points
  }
}

# When exactly one input is missing but the output is reported: the range of the
# missing input for which the output range can still overlap the reported
# output interval. A grid scan rather than optimize(), because the feasible set
# is what we want, not an extremum. The grid step, (upper - lower) / 2000 of
# `bounds`, is the resolution of the answer, so do not read it more finely.
solve_for <- function(rel, intervals, missing_input, reported_output_interval, bounds,
                      variant = NULL, grid_points = 2001L) {
  if (!missing_input %in% rel$inputs || missing_input %in% names(intervals)) {
    stop("`missing_input` must be an input of '", rel$name, "' that is absent from ",
         "`intervals`.", call. = FALSE)
  }
  target <- interval_bounds(reported_output_interval)
  # The open domain: the ends of `bounds` are limits (r = 1 gives atanh(Inf)), so
  # they are not scanned; a feasible set that reaches the first or last grid
  # point is reported as reaching the limit.
  grid <- seq(bounds[1], bounds[2], length.out = grid_points + 2L)[-c(1L, grid_points + 2L)]
  overlaps <- vapply(grid, function(value) {
    intervals[[missing_input]] <- list(lower = value, upper = value)
    output <- relation_range(rel, intervals, variant)
    if (!all(is.finite(output))) return(FALSE)
    intervals_overlap(output[["lower"]], output[["upper"]], target$lower, target$upper)
  }, logical(1))
  feasible <- any(overlaps)
  reaches_lower <- feasible && overlaps[1L]
  reaches_upper <- feasible && overlaps[length(overlaps)]
  list(lower = if (!feasible) NA_real_ else if (reaches_lower) bounds[1] else min(grid[overlaps]),
       upper = if (!feasible) NA_real_ else if (reaches_upper) bounds[2] else max(grid[overlaps]),
       feasible = feasible,
       reaches_lower = reaches_lower, reaches_upper = reaches_upper,
       resolution = (bounds[2] - bounds[1]) / (grid_points + 1L))
}

# Missing inputs that solve_for() can scan, with the bounds of the scan.
solvable_bounds <- function() list(r = c(-1, 1))

# Relations shared by the t-based handlers ------------------------------------

# p from a test statistic and its df: "t" (inputs t, df; a variant per tail),
# "F" (inputs f, df1, df2) or "chisq" (inputs chisq, df); F and chisq have one
# upper tail and no variants. Reuses t_p() (handler-t_independent.R).
# The two-sided p from t peaks at t = 0 (p = 1), which lies inside the box, not at a
# corner, when the interval of t straddles 0. For F and chisq the p-value is
# decreasing in the statistic; the df are counts, exact in practice, so the corners
# of the df box are exact then.
relation_p_from_statistic <- function(statistic = c("t", "F", "chisq")) {
  statistic <- match.arg(statistic)
  if (statistic == "F") {
    return(relation("p_from_f", inputs = c("f", "df1", "df2"), output = "p",
                    fn = function(vals, variant) {
                      stats::pf(vals$f, vals$df1, vals$df2, lower.tail = FALSE)
                    }))
  }
  if (statistic == "chisq") {
    return(relation("p_from_chisq", inputs = c("chisq", "df"), output = "p",
                    fn = function(vals, variant) {
                      stats::pchisq(vals$chisq, vals$df, lower.tail = FALSE)
                    }))
  }
  at_zero <- function(intervals) {
    t_bounds <- interval_bounds(intervals$t)
    straddles_zero <- isTRUE(t_bounds$lower < 0 && t_bounds$upper > 0)
    if (!straddles_zero) return(list())
    lapply(corner_points(intervals, "df", "p_from_t"), function(point) c(list(t = 0), point))
  }
  relation("p_from_t", inputs = c("t", "df"), output = "p",
           fn = function(vals, variant) t_p(vals$t, vals$df, variant),
           variants = c("two.sided", "less", "greater"), stationary = at_zero)
}
