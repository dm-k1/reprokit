# check_lm() and check_mediation(): internal helpers behind check_reported() for the
# regression family (ADR 0020 point 5); they are not exported. A regression reports a number per
# coefficient, so these take named vectors
# (`b = c(wt = "-3.19", hp = "-0.03")`) and turn them into the per-term quantity names
# check_reported() reads ("b:wt").

# Check a reported regression, or a mediation, against itself
#
# These helpers are reached through `check_reported("lm", ...)` and
# `check_reported("mediation", ...)`. `check_lm()` tests whether the numbers a paper reports
# for a simple regression, a multiple regression or a moderation analysis agree with each other.
# They are
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
# the df and the level (90%, 95% or 99%). The reported indirect effect is checked for
# coherence with a*b (it must lie in the product of the rounding intervals of a and b;
# ADR 0028). Its confidence interval is "not checkable": it is inferred by bootstrap,
# which resamples cases, and summaries cannot. No Sobel, Aroian or Goodman test is computed.
#
# @param ... Reported numbers as strings, so the decimals count. Per-coefficient
#   quantities (`b`, `se`, `t`, `p`, `ci_lower`, `ci_upper`, `beta`) are vectors named
#   by coefficient: `b = c(wt = "-3.19", cyl = "-1.51")`; for a simple regression a
#   single unnamed value is the slope. The residual df is `df2`, the model's are
#   `df1` and `df2`, with `r2`, `f` and `p_model`. For `sd`, either one value per
#   coefficient (named) plus the outcome's under a name that is not a coefficient
#   (`sd = c(wt = "0.98", mpg = "6.03")`), or for a simple regression the unnamed pair
#   (predictor, outcome). `check_mediation()` takes `b`, `se`, `t`, `p`, `df`, `ci_lower`
#   and `ci_upper` named by path (`a`, `b`, `c`, `c_prime`), and `n`, `indirect`,
#   `indirect_ci_lower` and
#   `indirect_ci_upper` as plain strings.
# @param handler For `check_lm()`, `"lm_multiple"` (the default), `"lm_simple"` or
#   `"lm_moderation"`.
# @param variants Variants to restrict to, as in [check_reported()].
# @param k The number of predictor columns, when F is not reported to say it. Without
#   `k` (or an exact `df1`), `n` and `df2` are not checked against each other: k is
#   never guessed from the number of coefficients reported, because a paper may
#   leave some out. A simple regression has k = 1.
# @return A `reprokit_check` object, see [check_reported()].
# Worked example: mpg ~ wt + hp on mtcars (n = 32), see test-engine-check-tests.R.
check_lm <- function(..., handler = "lm_multiple", variants = NULL, k = NULL) {
  if (!handler %in% c("lm_simple", "lm_multiple", "lm_moderation")) {
    abort_input("`handler` must be \"lm_simple\", \"lm_multiple\" or \"lm_moderation\".")
  }
  dots <- flatten_lm_report(list(...), handler)
  do.call(check_handler, c(list(handler = handler), dots, list(variants = variants, k = k)))
}

# Named vectors per coefficient become "base:term". Everything else passes through.
flatten_lm_report <- function(dots, handler) {
  if (length(dots) > 0L && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    abort_input(
      "Every reported number must be named by its quantity, for example t = c(wt = \"-6.13\")."
    )
  }
  per_term <- c("b", "se", "t", "p", "ci_lower", "ci_upper", "beta")
  terms <- character()
  for (q in intersect(names(dots), per_term)) {
    nm <- names(dots[[q]])
    if (is.null(nm)) {
      if (length(dots[[q]]) != 1L || handler != "lm_simple") {
        abort_input(
          "`", q, "` has one value per coefficient: name them, ",
          "e.g. ", q, " = c(wt = \"...\", hp = \"...\")."
        )
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
    abort_input(
      "`sd` needs names: the coefficients' and the outcome's, ",
      "e.g. sd = c(wt = \"0.98\", mpg = \"6.03\")."
    )
  }
  outcome <- setdiff(nm, terms)
  if (length(outcome) > 1L) {
    return(list())
  }
  nm[nm %in% outcome] <- "(response)"
  stats::setNames(as.list(unname(value)), paste0("sd:", nm))
}

# Worked example: simple mediation (mpg ~ wt through hp, mtcars), see the vignette.
check_mediation <- function(..., variants = NULL, indirect_type = NULL, estimator = NULL) {
  dots <- list(...)
  if (length(dots) > 0L && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    abort_input("Every reported number must be named by its quantity, for example n = \"32\".")
  }
  types <- c("unstandardized", "standardized", "partially_standardized")
  type_ok <- is.character(indirect_type) && length(indirect_type) == 1L && indirect_type %in% types
  if (!is.null(indirect_type) && !type_ok) {
    abort_input("`indirect_type` must be one of: ", paste0("\"", types, "\"", collapse = ", "), ".")
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
      "estimator difference. reprokit checks regression-based mediation only."
    ))
    return(res)
  }
  per_path <- c("b", "se", "t", "p", "df", "ci_lower", "ci_upper", "beta")
  out <- list()
  for (q in names(dots)) {
    value <- dots[[q]]
    if (q %in% per_path) {
      paths <- names(value)
      if (is.null(paths) || !all(nzchar(paths))) {
        abort_input(
          "`", q, "` has one value per path: name them a, b, c and c_prime, ",
          "e.g. ", q, " = c(a = \"...\", b = \"...\")."
        )
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
      notes <- paste(
        "The indirect effect is not evaluable: the plain paths a and b are not",
        "reported, and its type (standardized or not) is not declared. Declare",
        "`indirect_type` and report the matching a and b."
      )
      out$indirect <- NULL
    } else if (type == "partially_standardized") {
      notes <- paste(
        "A partially standardized indirect effect is not evaluable: it needs the",
        "standard deviations of the variables as well as a and b."
      )
      out$indirect <- NULL
    } else if (type == "standardized") {
      if (all(c("beta:a", "beta:b") %in% names(out))) {
        standardized_check <- check_handler(
          "mediation_simple",
          `b:a` = out[["beta:a"]], `b:b` = out[["beta:b"]],
          indirect = out$indirect, variants = variants
        )
      } else {
        notes <- paste(
          "A standardized indirect effect is not evaluable without the",
          "standardized paths: report `beta` for a and b."
        )
      }
      out$indirect <- NULL
    }
  }
  out[grepl("^beta:", names(out))] <- NULL
  res <- do.call(check_handler, c(
    list(handler = "mediation_simple"), out,
    list(variants = variants)
  ))
  if (!is.null(standardized_check)) {
    ok <- standardized_check$consistent
    notes <- c(notes, paste0(
      "The standardized indirect effect is ", if (ok) "coherent" else "not coherent",
      " with beta_a * beta_b within rounding."
    ))
    res$consistent <- res$consistent && ok
  }
  status <- res$numbers$status[res$numbers$quantity == "indirect"]
  if (length(status) == 1L && status == "inconsistent") {
    notes <- c(notes, paste(
      "The reported indirect effect is not coherent with a\u00b7b as reported: it lies outside",
      "the product of the rounding intervals of a and b. A bootstrap mean (instead of the",
      "sample a\u00b7b) is one possible reason; this says nothing about significance."
    ))
  }
  res$notes <- c(res$notes, notes)
  if (any(c("indirect_ci_lower", "indirect_ci_upper") %in% names(out))) {
    res$notes <- c(res$notes, paste(
      "The confidence interval of the indirect effect is not",
      "checkable: it is inferred by bootstrap, which resamples",
      "cases; summaries cannot. No Sobel, Aroian or Goodman test",
      "is computed."
    ))
  }
  res
}

check_t <- function(..., handler = "t_paired", variants = NULL, mu = 0) {
  check_handler(handler = handler, ..., variants = variants, mu = mu)
}

check_r <- function(..., handler = "cor_pearson", variants = NULL) {
  check_handler(handler = handler, ..., variants = variants)
}

check_F <- function(..., handler = NULL, variants = NULL, effect = NULL, # nolint: object_name_linter
                    dims = NULL) {
  dots <- list(...)
  factorial <- identical(handler, "anova_factorial") ||
    (is.null(handler) && (!is.null(effect) || !is.null(dims)))
  if (is.null(handler)) handler <- if (factorial) "anova_factorial" else "anova_oneway"
  if (!factorial) {
    return(check_handler(
      handler = handler, ..., variants = variants, effect = effect,
      dims = dims
    ))
  }
  # The cells are a x b matrices: read their shape, check that they agree, flatten.
  shapes <- lapply(dots[intersect(names(dots), c("n", "m", "sd"))], dim)
  shapes <- Filter(Negate(is.null), shapes)
  if (length(shapes) > 0L) {
    if (any(vapply(shapes, length, integer(1)) != 2L)) {
      abort_input(
        "`n`, `m` and `sd` must be matrices with one row per level of the first factor ",
        "and one column per level of the second, or plain vectors."
      )
    }
    if (length(unique(shapes)) > 1L) {
      abort_input(
        "`n`, `m` and `sd` must have the same dimensions; got ",
        paste0(names(shapes), " = ", vapply(shapes, paste, character(1), collapse = " x "),
          collapse = ", "
        ), "."
      )
    }
    shape <- shapes[[1L]]
    if (!is.null(dims) && !identical(as.integer(dims), as.integer(shape))) {
      abort_input(
        "`dims` (", paste(dims, collapse = " x "), ") does not match the dimensions of ",
        "the matrices (", paste(shape, collapse = " x "), ")."
      )
    }
    dims <- shape
  }
  for (q in intersect(names(dots), c("n", "m", "sd"))) {
    dots[[q]] <- as.vector(unclass(dots[[q]]))
    if (!is.null(dims) && length(dots[[q]]) != prod(dims)) {
      abort_input(
        "`", q, "` has ", length(dots[[q]]), " values, but `dims` says ",
        paste(dims, collapse = " x "), " = ", prod(dims), " cells."
      )
    }
  }
  if (!is.null(dims)) check_factorial_dims(dims)
  out <- do.call(check_handler, c(
    list(handler = handler), dots,
    list(variants = variants, dims = dims, effect = effect)
  ))
  out$notes <- c(out$notes, paste0("Effect checked: ", factorial_effect_words(effect), "."))
  out
}

check_chisq <- function(..., handler = NULL, variants = NULL, dims = NULL, p0 = NULL) {
  dots <- list(...)
  n <- dots[["n"]]
  notes <- character()
  if (!is.null(n)) {
    shape <- dim(n)
    # A 1-d table is a vector of counts.
    if (length(shape) == 1L) shape <- NULL
    if (length(shape) > 2L) {
      abort_input(
        "`n` must be a matrix or a vector, not an array of ", length(shape),
        " dimensions."
      )
    }
    if (!is.null(shape)) {
      if (!is.null(dims) && !identical(as.integer(dims), as.integer(shape))) {
        abort_input(
          "`dims` (", paste(dims, collapse = " x "), ") does not match the dimensions of `n` (",
          paste(shape, collapse = " x "), ")."
        )
      }
      dims <- shape
    }
    dots[["n"]] <- as.vector(unclass(n))
  }
  if (is.null(handler)) {
    handler <- if (length(dims) == 1L || (is.null(dims) && !is.null(n) && is.null(shape))) {
      if (is.null(dims) && is.null(n)) "chisq_independence" else "chisq_gof"
    } else {
      "chisq_independence"
    }
  }
  if (!handler %in% c("chisq_independence", "chisq_gof")) {
    abort_input("`handler` must be \"chisq_independence\" or \"chisq_gof\".")
  }
  if (handler == "chisq_gof" && is.null(dims) && !is.null(dots[["n"]])) dims <- length(dots[["n"]])
  if (handler == "chisq_independence" && !is.null(p0)) {
    abort_input("`p0` is for goodness of fit; a test of independence has no H0 proportions.")
  }
  if (!is.null(p0)) check_p0(p0, if (handler == "chisq_gof") dims else NULL)
  if (!is.null(dims)) check_table_dims(dims, if (handler == "chisq_gof") "gof" else "independence")
  if (handler == "chisq_independence" && !is.null(dots[["v"]]) && is.character(dots[["v"]])) {
    stripped <- sub("^\\s*[-\u2212]\\s*", "", dots[["v"]])
    if (!identical(stripped, dots[["v"]])) {
      dots[["v"]] <- stripped
      notes <- "A negative V (phi) is compared by absolute value; V is not signed."
    }
  } else if (handler == "chisq_independence" && is.numeric(dots[["v"]]) && any(dots[["v"]] < 0)) {
    dots[["v"]] <- abs(dots[["v"]])
    notes <- "A negative V (phi) is compared by absolute value; V is not signed."
  }
  out <- do.call(check_handler, c(
    list(handler = handler), dots,
    list(variants = variants, dims = dims, p0 = p0)
  ))
  out$notes <- c(out$notes, notes)
  out
}

# The test arguments the user gave, as a named list: `mu` only when it is not its
# default 0 (so a handler without a test value still accepts mu = 0), `dims`, `p0`
# and `effect` when not NULL. What a handler does not declare is rejected by
# relations_at().
test_arguments <- function(mu = 0, dims = NULL, p0 = NULL, effect = NULL, k = NULL) {
  args <- list()
  if (!(is.numeric(mu) && length(mu) == 1L && isTRUE(mu == 0))) args$mu <- mu
  if (!is.null(dims)) args$dims <- dims
  if (!is.null(p0)) args$p0 <- p0
  if (!is.null(effect)) args$effect <- effect
  if (!is.null(k)) args$k <- k
  args
}

# The vocabulary of `test`: sufficient_stats(test=) plus the regression and mediation
# checks, each mapped to the handler that carries its relations.
check_tests <- function() {
  c(
    student = "t_independent", welch = "t_independent", one_sample = "t_one_sample",
    paired = "t_paired", pearson = "cor_pearson", oneway = "anova_oneway",
    factorial = "anova_factorial", chisq = "chisq_independence", gof = "chisq_gof",
    mediation = "mediation_simple", lm = "lm_multiple", lm_simple = "lm_simple",
    lm_moderation = "lm_moderation"
  )
}
