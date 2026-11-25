# check_lm() and check_mediation(): the wrappers for the regression family (ADR 0020
# point 5). A regression reports a number per coefficient, so these take named vectors
# (`b = c(wt = "-3.19", hp = "-0.03")`) and turn them into the per-term quantity names
# check_reported() reads ("b:wt").

# Check a reported regression, or a mediation, against itself
#
# `check_lm()` tests whether the numbers a paper reports for a simple regression,
# a multiple regression or a moderation analysis agree with each other. They are
# all coherence checks, within what rounding allows: t = b / SE; p from t and the
# residual df; the confidence interval from b, SE, the residual df and the level
# (90%, 95% or 99%); F from R-squared and the two df, and the model p from F; n
# from the residual df (df2 = n - k - 1) when k is known; and for a simple
# regression also R-squared = r-squared, t-squared = F and r = b SD_x / SD_y.
#
# Two conventions never count against a paper. An adjusted R-squared reported as
# R-squared is accepted (the F is tried with both). A standardised beta that does not
# equal b SD_x / SD_y gets the status "not reproducible" and a note, because papers
# write beta for unstandardised b and the other way round.
#
# `check_mediation()` checks the four paths of a simple mediation, X to M (a), M to Y
# with X held constant (b), X to Y (c) and X to Y with M held constant (c'), each as a
# regression coefficient: t from b and SE, p from t and the path's df (n - 2 for a and
# c, n - 3 for b and c'), and a reported confidence interval of the path from b, SE,
# the df and the level (90%, 95% or 99%). The indirect effect a*b and its confidence interval are
# listed as "not checkable": they are inferred by bootstrap, which resamples cases,
# and summaries cannot. No Sobel, Aroian or Goodman test is computed, and a*b is
# never marked as fitting or not fitting.
#
# @param ... Reported numbers as strings, so the decimals count. Per-coefficient
#   quantities (`b`, `se`, `t`, `p`, `ci_lower`, `ci_upper`, `beta`) are vectors named
#   by coefficient: `b = c(wt = "-3.19", cyl = "-1.51")`; for a simple regression a
#   single unnamed value is the slope. The residual df is `df2`, the model's are
#   `df1` and `df2`, with `r2`, `f` and `p_model`. For `sd`, either one value per
#   coefficient (named) plus the outcome's under a name that is not a coefficient
#   (`sd = c(wt = "0.98", mpg = "6.03")`), or for a simple regression the unnamed pair
#   (predictor, outcome). `check_mediation()` takes `b`, `se`, `t`, `p`, `df`, `ci_lower`
#   and `ci_upper` named by path (`a`, `b`, `c`, `c_prime`), and `n`, `indirect`, `indirect_ci_lower` and
#   `indirect_ci_upper` as plain strings.
# @param handler For `check_lm()`, `"lm_multiple"` (the default), `"lm_simple"` or
#   `"lm_moderation"`.
# @param variants Variants to restrict to, as in [check_reported()].
# @param k The number of predictor columns, when F is not reported to say it. Without
#   `k` (or an exact `df1`), `n` and `df2` are not checked against each other: k is
#   never guessed from the number of coefficients reported, because a paper may
#   leave some out. A simple regression has k = 1.
# @return A `reprokit_check` object, see [check_reported()].
# @examples
# # mpg ~ wt + hp on mtcars (n = 32)
# check_lm(b = c(wt = "-3.88", hp = "-0.03"), se = c(wt = "0.63", hp = "0.01"),
#          t = c(wt = "-6.13", hp = "-3.52"), p = c(wt = "< .001", hp = ".001"),
#          r2 = ".83", f = "69.21", df1 = "2", df2 = "29", n = "32")
check_lm <- function(..., handler = "lm_multiple", variants = NULL, k = NULL) {
  if (!handler %in% c("lm_simple", "lm_multiple", "lm_moderation")) {
    stop("`handler` must be \"lm_simple\", \"lm_multiple\" or \"lm_moderation\".", call. = FALSE)
  }
  dots <- flatten_lm_report(list(...), handler)
  do.call(check_handler, c(list(handler = handler), dots, list(variants = variants, k = k)))
}

# Named vectors per coefficient become "base:term". Everything else passes through.
flatten_lm_report <- function(dots, handler) {
  if (length(dots) > 0L && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Every reported number must be named by its quantity, for example t = c(wt = \"-6.13\").",
         call. = FALSE)
  }
  per_term <- c("b", "se", "t", "p", "ci_lower", "ci_upper", "beta")
  terms <- character()
  for (q in intersect(names(dots), per_term)) {
    nm <- names(dots[[q]])
    if (is.null(nm)) {
      if (length(dots[[q]]) != 1L || handler != "lm_simple") {
        stop("`", q, "` has one value per coefficient: name them, ",
             "e.g. ", q, " = c(wt = \"...\", hp = \"...\").", call. = FALSE)
      }
    } else {
      terms <- union(terms, nm)
    }
  }
  if (handler == "lm_simple" && length(terms) == 0L) terms <- "x"
  out <- list()
  for (q in names(dots)) {
    value <- dots[[q]]
    if (q %in% per_term) {
      nm <- if (is.null(names(value))) terms[1L] else names(value)
      out[paste0(q, ":", nm)] <- as.list(unname(value))
    } else if (q == "sd" && length(value) > 1L || (q == "sd" && !is.null(names(value)))) {
      out <- c(out, flatten_lm_sd(value, terms, handler))
    } else {
      out[[q]] <- value
    }
  }
  out
}

# `sd` as one value per coefficient plus the outcome's. The outcome is the one name
# that is not a coefficient (or, for a simple regression, the second of an unnamed
# pair). With anything more ambiguous the SDs are left out: beta and r are then not
# computed, which is silence rather than a guess.
flatten_lm_sd <- function(value, terms, handler) {
  nm <- names(value)
  if (is.null(nm)) {
    if (handler == "lm_simple" && length(value) == 2L && length(terms) == 1L) {
      return(stats::setNames(as.list(unname(value)), c(paste0("sd:", terms), "sd:(response)")))
    }
    stop("`sd` needs names: the coefficients' and the outcome's, ",
         "e.g. sd = c(wt = \"0.98\", mpg = \"6.03\").", call. = FALSE)
  }
  outcome <- setdiff(nm, terms)
  if (length(outcome) > 1L) return(list())
  nm[nm %in% outcome] <- "(response)"
  stats::setNames(as.list(unname(value)), paste0("sd:", nm))
}

# @examples
# # Simple mediation (mpg ~ wt through hp, mtcars): the paths are checked, a*b is not.
# check_mediation(
#   b = c(a = "46.16", b = "-0.03", c = "-5.34", c_prime = "-3.88"),
#   se = c(a = "9.63", b = "0.01", c = "0.56", c_prime = "0.63"),
#   t = c(a = "4.80", b = "-3.52", c = "-9.56", c_prime = "-6.13"),
#   df = c(a = "30", b = "29", c = "30", c_prime = "29"),
#   ci_lower = c(a = "26.50", b = "-0.05", c = "-6.49", c_prime = "-5.17"),
#   ci_upper = c(a = "65.82", b = "-0.01", c = "-4.20", c_prime = "-2.58"),
#   n = "32", indirect = "-1.46", indirect_ci_lower = "-2.28", indirect_ci_upper = "-0.80")
check_mediation <- function(..., variants = NULL, indirect_type = NULL, estimator = NULL) {
  dots <- list(...)
  if (length(dots) > 0L && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Every reported number must be named by its quantity, for example n = \"32\".",
         call. = FALSE)
  }
  types <- c("unstandardized", "standardized", "partially_standardized")
  if (!is.null(indirect_type) && !(is.character(indirect_type) && length(indirect_type) == 1L &&
                                   indirect_type %in% types)) {
    stop("`indirect_type` must be one of: ", paste0("\"", types, "\"", collapse = ", "), ".",
         call. = FALSE)
  }
  # ADR 0028 point 1: ML/SEM estimation (z tests for the paths, or declared) is never
  # checked as regression.
  sem <- identical(tolower(estimator), "sem") || identical(tolower(estimator), "ml") ||
    identical(tolower(estimator), "lavaan") || "z" %in% names(dots)
  if (sem) {
    res <- check_handler("mediation_simple", variants = variants)
    res$notes <- c(res$notes, paste(
      "Not evaluable: the reported numbers come from ML/SEM estimation (z tests for the paths,",
      "or a declared lavaan/SEM fit). Its standard errors use the divisor n and its tests are",
      "z tests, so checking them as a regression could report a mismatch that is only an",
      "estimator difference. reprokit checks regression-based mediation only."))
    return(res)
  }
  per_path <- c("b", "se", "t", "p", "df", "ci_lower", "ci_upper", "beta")
  out <- list()
  for (q in names(dots)) {
    value <- dots[[q]]
    if (q %in% per_path) {
      paths <- names(value)
      if (is.null(paths) || !all(nzchar(paths))) {
        stop("`", q, "` has one value per path: name them a, b, c and c_prime, ",
             "e.g. ", q, " = c(a = \"...\", b = \"...\").", call. = FALSE)
      }
      paths <- sub("^c'$|^c\u2032$", "c_prime", paths)
      out[paste0(q, ":", paths)] <- as.list(unname(value))
    } else {
      out[[q]] <- value
    }
  }
  # A standardized (or partially standardized) indirect effect is another quantity than
  # b_a * b_b: it is checked only when its type is declared and the matching paths are
  # reported; otherwise it is not evaluable (ADR 0028 point 3).
  notes <- character()
  standardized_check <- NULL
  if ("indirect" %in% names(out)) {
    type <- if (is.null(indirect_type)) {
      # Without a declaration the number is taken as the plain a*b only when the plain paths
      # are reported; a paper that reports only standardized paths is not guessed at.
      if (all(c("b:a", "b:b") %in% names(out))) "unstandardized" else "undeclared"
    } else {
      indirect_type
    }
    if (type == "undeclared") {
      notes <- paste("The indirect effect is not evaluable: the plain paths a and b are not",
                     "reported, and its type (standardized or not) is not declared. Declare",
                     "`indirect_type` and report the matching a and b.")
      out$indirect <- NULL
    } else if (type == "partially_standardized") {
      notes <- paste("A partially standardized indirect effect is not evaluable: it needs the",
                     "standard deviations of the variables as well as a and b.")
      out$indirect <- NULL
    } else if (type == "standardized") {
      if (all(c("beta:a", "beta:b") %in% names(out))) {
        standardized_check <- check_handler(
          "mediation_simple", `b:a` = out[["beta:a"]], `b:b` = out[["beta:b"]],
          indirect = out$indirect, variants = variants)
      } else {
        notes <- paste("A standardized indirect effect is not evaluable without the",
                       "standardized paths: report `beta` for a and b.")
      }
      out$indirect <- NULL
    }
  }
  out[grepl("^beta:", names(out))] <- NULL
  res <- do.call(check_handler, c(list(handler = "mediation_simple"), out,
                                  list(variants = variants)))
  if (!is.null(standardized_check)) {
    ok <- standardized_check$consistent
    notes <- c(notes, paste0(
      "The standardized indirect effect is ", if (ok) "coherent" else "not coherent",
      " with beta_a * beta_b within rounding."))
    res$consistent <- res$consistent && ok
  }
  status <- res$numbers$status[res$numbers$quantity == "indirect"]
  if (length(status) == 1L && status == "inconsistent") {
    notes <- c(notes, paste(
      "The reported indirect effect is not coherent with a\u00b7b as reported: it lies outside",
      "the product of the rounding intervals of a and b. A bootstrap mean (instead of the",
      "sample a\u00b7b) is one possible reason; this says nothing about significance."))
  }
  res$notes <- c(res$notes, notes)
  if (any(c("indirect_ci_lower", "indirect_ci_upper") %in% names(out))) {
    res$notes <- c(res$notes, paste("The confidence interval of the indirect effect is not",
                                    "checkable: it is inferred by bootstrap, which resamples",
                                    "cases; summaries cannot. No Sobel, Aroian or Goodman test",
                                    "is computed."))
  }
  res
}
