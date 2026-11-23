# Reported numbers and their rounding intervals.
# In one line: a reported number is a string, because its decimals say how
# precisely it was reported; "2.85" stands for every value in [2.845, 2.855].
# The shared vocabulary of quantity names lives here too, so the share and
# check directions use the same words (ADR 0010).

#' Names of the quantities the checks know
#'
#' The shared vocabulary of `check_reported()`, `check_t()` and `check_r()`: the
#' names under which reported numbers are passed in, and the names handlers use
#' in their relations.
#'
#' @return A character vector.
#' @export
#' @examples
#' quantity_names()
quantity_names <- function() {
  c("n", "m", "sd", "r", "t", "f", "chisq", "df", "p", "ci_lower", "ci_upper", "d")
}

# Quantities that are descriptives: they come from the data, not from another
# reported statistic. A number recomputed from one of these is "verified".
descriptive_quantities <- function() c("n", "m", "sd", "r")

# Quantities with one value per group or condition; every other quantity is a
# single number for the whole analysis.
per_cell_quantities <- function() c("m", "sd")

# The values a quantity can take, as c(lower, upper). A rounding interval is
# intersected with this, so r = "1.00" means [0.995, 1], not [0.995, 1.005].
quantity_domain <- function(quantity) {
  switch(quantity,
         r = c(-1, 1),
         p = c(0, 1),
         n = c(1, Inf),
         sd = , df = , f = , chisq = c(0, Inf),
         c(-Inf, Inf))
}

domain_words <- function(quantity) {
  switch(quantity,
         n = "a whole number of at least 1",
         r = "between -1 and 1",
         p = "between 0 and 1",
         "at least 0")
}

# Intersect the interval bounds of a reported quantity with its domain. A
# reported number with no value in the domain (r = "1.50") is an input error.
restrict_to_domain <- function(bounds, quantity, text) {
  domain <- quantity_domain(quantity)
  lower <- pmax(bounds$lower, domain[1L])
  upper <- pmin(bounds$upper, domain[2L])
  if (quantity == "n") {
    lower <- ceiling(lower)
    upper <- floor(upper)
  }
  if (any(lower > upper)) {
    stop("`", quantity, "` = ", paste0("\"", text, "\"", collapse = ", "),
         " is not a value ", quantity, " can take; it must be ", domain_words(quantity), ".",
         call. = FALSE)
  }
  list(lower = lower, upper = upper)
}

# A quantity is one number or, for m and sd (and for n when the handler's
# groups differ in size: its `per_cell`), one value per condition; the handler
# says how many conditions (`cells`).
check_arity <- function(reported, cells, per_cell_names = per_cell_quantities()) {
  for (quantity in names(reported)) {
    per_cell <- quantity %in% per_cell_names && !is.null(cells)
    expected <- if (per_cell) cells else 1L
    got <- length(reported[[quantity]])
    if (got != expected) {
      stop("`", quantity, "` needs ", expected, if (expected == 1L) " value" else " values",
           if (per_cell) " (one per condition)" else " (one number for the whole analysis)",
           ", but got ", got, ".", call. = FALSE)
    }
  }
  invisible(reported)
}

check_quantity_names <- function(x, what = "quantity name") {
  unknown <- setdiff(x, quantity_names())
  if (length(unknown) > 0L) {
    stop("Unknown ", what, ": ", paste0("'", unknown, "'", collapse = ", "),
         ". The known quantities are: ", paste(quantity_names(), collapse = ", "), ".",
         call. = FALSE)
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
  if (length(text) == 0L) stop("A reported value needs at least one number.", call. = FALSE)
  if (is.character(text)) {
    if (!is.null(digits)) {
      stop("`digits` only applies to numeric input; a string carries its own decimals.",
           call. = FALSE)
    }
    values <- lapply(text, parse_reported_text, as_count = as_count)
  } else if (is.numeric(text)) {
    values <- numeric_reported(text, digits, as_count)
  } else {
    stop("A reported value must be a string such as \"2.85\" (or a number with `digits`).",
         call. = FALSE)
  }
  names(values) <- names(text)
  if (length(values) == 1L) values[[1L]] else values
}

parse_reported_text <- function(text, as_count = TRUE) {
  if (is.na(text)) stop("A reported value cannot be NA.", call. = FALSE)
  cleaned <- trimws(gsub("\u2212", "-", text))
  relation <- "="
  if (grepl("^[<>]", cleaned)) {
    relation <- substr(cleaned, 1L, 1L)
    cleaned <- trimws(substring(cleaned, 2L))
  }
  parts <- regmatches(cleaned, regexec("^[+-]?([0-9]*)(\\.([0-9]+))?$", cleaned))[[1L]]
  if (length(parts) == 0L || !grepl("[0-9]", cleaned)) {
    stop("Cannot read '", text, "' as a reported number. Use a string such as \"2.85\", ",
         "\".008\" or \"< .001\".", call. = FALSE)
  }
  has_decimals <- nzchar(parts[3L])
  whole <- if (as_count) NA_integer_ else 0L
  reported_interval(as.numeric(cleaned), if (has_decimals) nchar(parts[4L]) else whole,
                    relation)
}

numeric_reported <- function(x, digits, as_count = TRUE) {
  if (anyNA(x)) stop("A reported value cannot be NA.", call. = FALSE)
  if (is.null(digits)) {
    if (any(x != round(x))) {
      stop("A non-integer number does not say how precisely it was reported. Pass it as a ",
           "string (\"2.85\") or give `digits`.", call. = FALSE)
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
