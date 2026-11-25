# Reported numbers and their rounding intervals.
# In one line: a reported number is a string, because its decimals say how
# precisely it was reported; "2.85" stands for every value in [2.845, 2.855].
# The shared vocabulary of quantity names lives here too, so the share and
# check directions use the same words (ADR 0010).

#' Names of the quantities the checks know
#'
#' The shared vocabulary of `check_reported()`: the
#' names under which reported numbers are passed in, and the names handlers use
#' in their relations.
#'
#' @return A character vector.
#' @export
#' @examples
#' quantity_names()
quantity_names <- function() {
  c("n", "m", "sd", "r", "t", "f", "chisq", "df", "df1", "df2", "p", "eta2", "ci_lower",
    "ci_upper", "d", "n_total", "v", "p0", "b", "se", "beta", "r2", "p_model", "r_all", "xz",
    "indirect", "indirect_ci_lower", "indirect_ci_upper",
    # the cross-complete family: adjusted R squared, omega squared and the partial forms
    "adj_r2", "omega2", "partial_eta2", "partial_omega2")
}

# Regression reports one number per coefficient. These quantities then carry the
# coefficient's name after a colon ("b:wt", "se:wt", "df:a"), a pseudo-quantity
# of one scalar value that relations and statuses treat like any other (ADR 0020).
# A handler without `terms` in its design has none of them.
per_term_quantities <- function() {
  c("b", "se", "t", "df", "p", "ci_lower", "ci_upper", "beta", "sd",
    # a mediation's model-level numbers, one set per regression ("r2:a" is the R squared of
    # the model that contains path a)
    "r2", "f", "df1", "df2", "p_model")
}

# "b:wt" -> "b"; a quantity without a colon is its own base.
quantity_base <- function(x) sub(":.*$", "", x)

# Why a quantity that determines the analysis may be missing from what APA asks a
# paper to report: one plain-English reason per quantity, shared by every test
# (ADR 0013 decision 2).
missing_reasons <- function() {
  c(r = paste("the SD of the differences depends on the correlation between conditions;",
              "per-condition M and SD do not determine it"),
    p0 = "the expected counts depend on them; not needed when they are equal",
    r_all = "coefficients depend on how the predictors overlap",
    xz = "cannot be derived from X and Z summaries")
}

# Quantities that are descriptives: they come from the data, not from another
# reported statistic. A number recomputed from one of these is "verified".
descriptive_quantities <- function() c("n", "m", "sd", "r")

# Quantities with one value per group or condition; every other quantity is a
# single number for the whole analysis.
per_cell_quantities <- function() c("m", "sd")

# The values a quantity can take, as c(lower, upper). A rounding interval is
# intersected with this, so r = "1.00" means [0.995, 1], not [0.995, 1.005].
# `overrides` is a handler's own named list of domains (`domains` in
# register_handler()): chi-squared counts can be 0 where a group size cannot, and
# Cohen's w can exceed 1 where Cramer's V cannot.
quantity_domain <- function(quantity, overrides = NULL) {
  if (!is.null(overrides[[quantity]])) return(overrides[[quantity]])
  switch(quantity_base(quantity),
         r = c(-1, 1),
         p = , p0 = , p_model = , r2 = , partial_eta2 = c(0, 1),
         adj_r2 = , omega2 = , partial_omega2 = c(-1, 1),
         n = , n_total = c(1, Inf),
         sd = , se = , df = , df1 = , df2 = , f = , chisq = c(0, Inf),
         eta2 = , v = c(0, 1),
         c(-Inf, Inf))
}

# Quantities that are whole numbers: their rounding interval is snapped inward to
# the integers inside it.
whole_quantities <- function() c("n", "n_total")

domain_words <- function(quantity, domain = quantity_domain(quantity)) {
  if (quantity %in% whole_quantities()) {
    return(paste("a whole number of at least", domain[1L]))
  }
  if (domain[1L] == 0 && domain[2L] == 1) return("between 0 and 1")
  if (domain[1L] == -1 && domain[2L] == 1) return("between -1 and 1")
  "at least 0"
}

# Intersect the interval bounds of a reported quantity with its domain. A
# reported number with no value in the domain (r = "1.50") is an input error.
restrict_to_domain <- function(bounds, quantity, text, overrides = NULL) {
  domain <- quantity_domain(quantity, overrides)
  lower <- pmax(bounds$lower, domain[1L])
  upper <- pmin(bounds$upper, domain[2L])
  if (quantity %in% whole_quantities()) {
    lower <- ceiling(lower)
    upper <- floor(upper)
  }
  if (any(lower > upper)) {
    abort_input(
      "`", quantity, "` = ", paste0("\"", text, "\"", collapse = ", "),
      " is not a value ", quantity, " can take; it must be ",
      domain_words(quantity, domain), "."
    )
  }
  list(lower = lower, upper = upper)
}

# A quantity is one number or, for m and sd (and for n when the handler's
# groups differ in size: its `per_cell`), one value per condition; the handler
# says how many conditions (`cells`). `cells = NA` means a variable number of
# groups (one-way ANOVA): the reported per-group vectors need at least two values
# and the same length.
check_arity <- function(reported, cells, per_cell_names = per_cell_quantities()) {
  variable <- length(cells) == 1L && is.na(cells)
  for (quantity in names(reported)) {
    per_cell <- quantity %in% per_cell_names && !is.null(cells)
    if (per_cell && variable) next
    expected <- if (per_cell) cells else 1L
    got <- length(reported[[quantity]])
    if (got != expected) {
      abort_input(
        "`", quantity, "` needs ", expected, if (expected == 1L) " value" else " values",
        if (per_cell) " (one per condition)" else " (one number for the whole analysis)",
        ", but got ", got, "."
      )
    }
  }
  if (variable) {
    sizes <- vapply(reported[intersect(names(reported), per_cell_names)], length, integer(1))
    if (length(sizes) > 0L && (any(sizes < 2L) || length(unique(sizes)) > 1L)) {
      abort_input(
        "The per-group quantities (", paste0("`", names(sizes), "`", collapse = ", "),
        ") need the same number of values, at least 2 (one per group); got ",
        paste0(names(sizes), " = ", sizes, collapse = ", "), "."
      )
    }
  }
  invisible(reported)
}

# `terms` allows the per-coefficient form "b:wt" (a per-term quantity, a colon, a
# non-empty name); only handlers with `terms` in their design take it.
check_quantity_names <- function(x, what = "quantity name", terms = TRUE) {
  named <- grepl(":", x, fixed = TRUE)
  if (terms) {
    ok <- named & quantity_base(x) %in% per_term_quantities() & nzchar(sub("^[^:]*:", "", x))
    x <- ifelse(ok, quantity_base(x), x)
  }
  unknown <- setdiff(x, quantity_names())
  if (length(unknown) > 0L) {
    abort_input(
      "Unknown ", what, ": ", paste0("'", unknown, "'", collapse = ", "),
      ". The known quantities are: ", paste(quantity_names(), collapse = ", "), "."
    )
  }
  invisible(x)
}

# Parse reported numbers into list(point, lower, upper, relation).
#   "2.85"  -> point 2.85, [2.845, 2.855], relation "="
#   "< .001" -> relation "<", (0, 0.001); "> .05" -> relation ">", (0.05, 1).
#     The open ends 0 and 1 are the bounds of a p-value, the only quantity
#     reported with an inequality.
#   "29"    -> no decimal point: a count (df, n), exactly [29, 29]. For any other
#     quantity (`as_count = FALSE`) "29" is a number rounded to a whole unit,
#     [28.5, 29.5]: an SD or mean reported as "1" is not exactly 1.
# One value returns the list itself; several return a list of these, with names.
# A numeric needs `digits`, unless it is integer-valued (then it is exact).
reported_value <- function(text, digits = NULL, as_count = TRUE) {
  if (length(text) == 0L) abort_input("A reported value needs at least one number.")
  if (is.character(text)) {
    if (!is.null(digits)) {
      abort_input("`digits` only applies to numeric input; a string carries its own decimals.")
    }
    values <- lapply(text, parse_reported_text, as_count = as_count)
  } else if (is.numeric(text)) {
    values <- numeric_reported(text, digits, as_count)
  } else {
    abort_input("A reported value must be a string such as \"2.85\" (or a number with `digits`).")
  }
  names(values) <- names(text)
  if (length(values) == 1L) values[[1L]] else values
}

parse_reported_text <- function(text, as_count = TRUE) {
  if (is.na(text)) abort_input("A reported value cannot be NA.")
  cleaned <- trimws(gsub("\u2212", "-", text))
  relation <- "="
  if (grepl("^[<>]", cleaned)) {
    relation <- substr(cleaned, 1L, 1L)
    cleaned <- trimws(substring(cleaned, 2L))
  }
  parts <- regmatches(cleaned, regexec("^[+-]?([0-9]*)(\\.([0-9]+))?$", cleaned))[[1L]]
  if (length(parts) == 0L || !grepl("[0-9]", cleaned)) {
    abort_input(
      "Cannot read '", text, "' as a reported number. Use a string such as \"2.85\", ",
      "\".008\" or \"< .001\"."
    )
  }
  has_decimals <- nzchar(parts[3L])
  whole <- if (as_count) NA_integer_ else 0L
  reported_interval(as.numeric(cleaned), if (has_decimals) nchar(parts[4L]) else whole,
                    relation)
}

numeric_reported <- function(x, digits, as_count = TRUE) {
  if (anyNA(x)) abort_input("A reported value cannot be NA.")
  if (is.null(digits)) {
    if (any(x != round(x))) {
      abort_input(
        "A non-integer number does not say how precisely it was reported. Pass it as a ",
        "string (\"2.85\") or give `digits`."
      )
    }
    digits <- rep(if (as_count) NA_integer_ else 0L, length(x))
  } else {
    digits <- rep_len(digits, length(x))
    x <- round(x, digits)
  }
  Map(function(point, decimals) reported_interval(point, decimals, "="), x, digits)
}

# `decimals` is NA for an exact count.
reported_interval <- function(point, decimals, relation) {
  half_unit <- if (is.na(decimals)) 0 else 0.5 * 10^(-decimals)
  bounds <- switch(relation,
                   "=" = c(point - half_unit, point + half_unit),
                   "<" = c(0, point),
                   ">" = c(point, 1))
  list(point = point, lower = bounds[1], upper = bounds[2], relation = relation)
}

# The intervals of a reported quantity as list(lower = , upper = ), one entry
# per group or condition. Accepts what reported_value() returns (one value or a
# list of values) and its own output.
interval_bounds <- function(x) {
  if (is.numeric(x[["lower"]])) return(list(lower = x[["lower"]], upper = x[["upper"]]))
  list(lower = vapply(x, function(v) v[["lower"]], numeric(1)),
       upper = vapply(x, function(v) v[["upper"]], numeric(1)))
}
