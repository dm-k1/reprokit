# check_lm() and check_mediation(): the wrappers for the regression family (ADR 0020
# point 5). A regression reports a number per coefficient, so these take named vectors
# (`b = c(wt = "-3.19", hp = "-0.03")`) and turn them into the per-term quantity names
# check_reported() reads ("b:wt").

#' Check a reported regression, or a mediation, against itself
#'
#' `check_lm()` tests whether the numbers a paper reports for a simple regression,
#' a multiple regression or a moderation analysis agree with each other. They are
#' all coherence checks, within what rounding allows: t = b / SE; p from t and the
#' residual df; the confidence interval from b, SE, the residual df and the level
#' (90%, 95% or 99%); F from R-squared and the two df, and the model p from F; n
#' from the residual df (df2 = n - k - 1) when k is known; and for a simple
#' regression also R-squared = r-squared, t-squared = F and r = b SD_x / SD_y.
#'
#' Two conventions never count against a paper. An adjusted R-squared reported as
#' R-squared is accepted (the F is tried with both). A standardised beta that does not
#' equal b SD_x / SD_y gets the status "not reproducible" and a note, because papers
#' write beta for unstandardised b and the other way round.
#'
#' `check_mediation()` checks the four paths of a simple mediation, X to M (a), M to Y
#' with X held constant (b), X to Y (c) and X to Y with M held constant (c'), each as a
#' regression coefficient: t from b and SE, p from t and the path's df (n - 2 for a and
#' c, n - 3 for b and c'), and a reported confidence interval of the path from b, SE,
#' the df and the level (90%, 95% or 99%). The indirect effect a*b and its confidence interval are
#' listed as "not checkable": they are inferred by bootstrap, which resamples cases,
#' and summaries cannot. No Sobel, Aroian or Goodman test is computed, and a*b is
#' never marked as fitting or not fitting.
#'
#' @param ... Reported numbers as strings, so the decimals count. Per-coefficient
#'   quantities (`b`, `se`, `t`, `p`, `ci_lower`, `ci_upper`, `beta`) are vectors named
#'   by coefficient: `b = c(wt = "-3.19", cyl = "-1.51")`; for a simple regression a
#'   single unnamed value is the slope. The residual df is `df2`, the model's are
#'   `df1` and `df2`, with `r2`, `f` and `p_model`. For `sd`, either one value per
#'   coefficient (named) plus the outcome's under a name that is not a coefficient
#'   (`sd = c(wt = "0.98", mpg = "6.03")`), or for a simple regression the unnamed pair
#'   (predictor, outcome). `check_mediation()` takes `b`, `se`, `t`, `p`, `df`, `ci_lower`
#'   and `ci_upper` named by path (`a`, `b`, `c`, `c_prime`), and `n`, `indirect`, `indirect_ci_lower` and
#'   `indirect_ci_upper` as plain strings.
#' @param handler For `check_lm()`, `"lm_multiple"` (the default), `"lm_simple"` or
#'   `"lm_moderation"`.
#' @param variants Variants to restrict to, as in [check_reported()].
#' @param k The number of predictor columns, when F is not reported to say it. Without
#'   `k` (or an exact `df1`), `n` and `df2` are not checked against each other: k is
#'   never guessed from the number of coefficients reported, because a paper may
#'   leave some out. A simple regression has k = 1.
#' @return A `reprokit_check` object, see [check_reported()].
#' @export
#' @examples
#' # mpg ~ wt + hp on mtcars (n = 32)
#' check_lm(b = c(wt = "-3.88", hp = "-0.03"), se = c(wt = "0.63", hp = "0.01"),
#'          t = c(wt = "-6.13", hp = "-3.52"), p = c(wt = "< .001", hp = ".001"),
#'          r2 = ".83", f = "69.21", df1 = "2", df2 = "29", n = "32")
check_lm <- function(..., handler = "lm_multiple", variants = NULL, k = NULL) {
  if (!handler %in% c("lm_simple", "lm_multiple", "lm_moderation")) {
    stop("`handler` must be \"lm_simple\", \"lm_multiple\" or \"lm_moderation\".", call. = FALSE)
  }
  dots <- flatten_lm_report(list(...), handler)
  do.call(check_reported, c(list(handler = handler), dots, list(variants = variants, k = k)))
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

#' @rdname check_lm
#' @export
#' @examples
#' # Simple mediation (mpg ~ wt through hp, mtcars): the paths are checked, a*b is not.
#' check_mediation(
#'   b = c(a = "46.16", b = "-0.03", c = "-5.34", c_prime = "-3.88"),
#'   se = c(a = "9.63", b = "0.01", c = "0.56", c_prime = "0.63"),
#'   t = c(a = "4.80", b = "-3.52", c = "-9.56", c_prime = "-6.13"),
#'   df = c(a = "30", b = "29", c = "30", c_prime = "29"),
#'   ci_lower = c(a = "26.50", b = "-0.05", c = "-6.49", c_prime = "-5.17"),
#'   ci_upper = c(a = "65.82", b = "-0.01", c = "-4.20", c_prime = "-2.58"),
#'   n = "32", indirect = "-1.46", indirect_ci_lower = "-2.28", indirect_ci_upper = "-0.80")
check_mediation <- function(..., variants = NULL) {
  dots <- list(...)
  if (length(dots) > 0L && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Every reported number must be named by its quantity, for example n = \"32\".",
         call. = FALSE)
  }
  per_path <- c("b", "se", "t", "p", "df", "ci_lower", "ci_upper")
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
  res <- do.call(check_reported, c(list(handler = "mediation_simple"), out,
                                   list(variants = variants)))
  if (any(c("indirect", "indirect_ci_lower", "indirect_ci_upper") %in% names(out))) {
    res$notes <- c(res$notes, paste("The indirect effect a*b and its confidence interval are not",
                                    "checkable: they are inferred by bootstrap, which resamples",
                                    "cases; summaries cannot. They are neither confirmed nor",
                                    "contradicted."))
  }
  res
}
