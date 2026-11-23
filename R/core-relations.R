# The equations between reported quantities, and how to evaluate them on intervals.
# In one line: a relation computes one output quantity from input quantities;
# evaluated over the rounding intervals of its inputs it gives the range the
# output may have, and solve_for() inverts that to the range a missing input may have.

# `fn(vals, variant)` takes a named list of plain numbers (a quantity with one
# value per condition is a vector) and returns one number. A relation has
# exactly one scalar output; a CI pair is two relations, one per bound.
# `variants` names the alternatives the author must have chosen between; `fn`
# receives the current one. `variant_kind` says what they are: "tail" (the
# sidedness of a p-value), "level" (a confidence level), "test" (Student or
# Welch) or "convention" (which formula an effect size follows). A relation has
# one kind; a relation that would need two is split in two, which chaining
# (evaluate_relations() takes the widest range over variants) makes work.
# `falsifies` says whether a failure may count as an inconsistency. It is FALSE
# for a relation whose formula authors choose freely and rarely declare (the
# d convention): when none of its variants fits, the number is "not
# reproducible", never "inconsistent".
# `stationary(intervals)` is optional: it returns a list of extra points, each
# a named list of input values inside the box, where `fn` can have an interior
# extremum that the corners miss (see relation_range()).
relation <- function(name, inputs, output, fn, variants = NULL, variant_kind = "tail",
                     stationary = NULL, falsifies = TRUE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name),
            is.character(inputs), length(inputs) >= 1L,
            is.character(output), length(output) == 1L,
            is.function(fn), identical(names(formals(fn)), c("vals", "variant")),
            is.null(variants) || (is.character(variants) && length(variants) >= 1L),
            variant_kind %in% relation_variant_kinds(),
            is.null(stationary) || is.function(stationary),
            is.logical(falsifies), length(falsifies) == 1L, !is.na(falsifies))
  check_quantity_names(c(inputs, output), "quantity name in relation")
  list(name = name, inputs = inputs, output = output, fn = fn, variants = variants,
       variant_kind = variant_kind, stationary = stationary, falsifies = falsifies)
}

# The kinds of variant, in the order they are listed, with the words used for
# the ones that fit.
relation_variant_kinds <- function() c("tail", "level", "test", "convention")

variant_kind_labels <- function() {
  c(tail = "Variants that fit", level = "Confidence levels that fit",
    test = "Tests that fit", convention = "Effect-size conventions that fit")
}

is_relation <- function(x) {
  is.list(x) && identical(names(x), c("name", "inputs", "output", "fn", "variants",
                                      "variant_kind", "stationary", "falsifies"))
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
  lower <- unlist(lapply(bounds, `[[`, "lower"), use.names = FALSE)
  upper <- unlist(lapply(bounds, `[[`, "upper"), use.names = FALSE)
  group <- factor(rep(inputs, lengths(lapply(bounds, `[[`, "lower"))), levels = inputs)
  free <- which(lower != upper)
  if (length(free) > max_free_dimensions) {
    stop("Relation '", what, "' has ", length(free), " non-exact input values, which means ",
         2^length(free), " corners; the limit is 2^", max_free_dimensions, ".", call. = FALSE)
  }
  corners <- if (length(free) == 0L) {
    matrix(numeric(), nrow = 1L, ncol = 0L)
  } else {
    as.matrix(expand.grid(lapply(free, function(i) c(lower[i], upper[i])),
                          KEEP.OUT.ATTRS = FALSE))
  }
  lapply(seq_len(nrow(corners)), function(i) {
    point <- lower
    point[free] <- corners[i, ]
    split(point, group)
  })
}

# Range of the output over the box of the input intervals: `fn` evaluated at
# every corner and at the relation's `stationary` points, then min and max. For
# a smooth `fn` the extremes over a box lie at corners or at interior
# stationary points of `fn` restricted to a face of the box, so the range is
# exact once `stationary` lists those points. Without `stationary` the corners
# alone are exact only if `fn` is monotone in each input over the box.
relation_range <- function(rel, intervals, variant = NULL) {
  absent <- setdiff(rel$inputs, names(intervals))
  if (length(absent) > 0L) {
    stop("Relation '", rel$name, "' needs ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  points <- corner_points(intervals, rel$inputs, rel$name)
  if (!is.null(rel$stationary)) points <- c(points, rel$stationary(intervals))
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
# of the SDs. `candidates(corner, i, lower, upper)` returns the values of sd[i]
# (other inputs as in `corner`) that may be extrema. On every corner the
# candidates for one SD are tried alone and then together with candidates for
# the other SD.
sd_stationary <- function(inputs, what, candidates) {
  function(intervals) {
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

# p from t and df, one variant per tail. Reuses t_p() (handler-t_independent.R).
# The two-sided p peaks at t = 0 (p = 1), which lies inside the box, not at a
# corner, when the interval of t straddles 0.
relation_p_from_t <- function() {
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
