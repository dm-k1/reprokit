# The regression family (ADR 0020): simple regression, multiple regression and
# moderation, one handler each, all on the lm share side of handler-lm_base.R.
# In one line: the three handlers match different lm fits, reuse lm_summarise(),
# lm_reproduce() and lm_reported(), and check the reported numbers against each other
# (t = b / SE, p, CI, F from R squared, n from df, and for simple regression r and
# R squared); the general `lm` keeps every other fit.
#
# Where the APA set of these handlers comes from (ADR 0018 point 2):
#  - JARS-Quant Table 1 ("all inferential tests ... minimally sufficient set ...
#    effect-size estimates and confidence intervals"): per coefficient b, SE, t, p and
#    the CI, and the model's R squared, F, its df and p. Also "descriptions of each
#    primary and secondary outcome": n and the M and SD of the outcome.
#  - Manual section 3.7 (the general means-and-SDs clause), NOT JARS: the M and SD of
#    each predictor.
# One `apa` set holds all three parts; the source of each part is this comment.

# What a fit looks like, or NULL when it is not one the narrow handlers take: an
# ordinary `lm` (class exactly "lm") with an intercept and no offset, whose predictors
# are plain variables (numeric, factor, character, logical), no spline or polynomial.
lm_shape <- function(x) {
  if (!identical(class(x), "lm")) return(NULL)
  tt <- stats::terms(x)
  if (attr(tt, "intercept") != 1L || !is.null(attr(tt, "offset")) || !is.null(x$offset)) {
    return(NULL)
  }
  labels <- attr(tt, "term.labels")
  if (length(labels) == 0L) return(NULL)
  classes <- attr(tt, "dataClasses")
  factors <- attr(tt, "factors")
  variables <- rownames(factors)
  predictors <- setdiff(variables, deparse(stats::formula(x)[[2L]]))
  plain <- c("numeric", "factor", "ordered", "logical", "character")
  if (is.null(classes) || !all(classes[intersect(predictors, names(classes))] %in% plain)) {
    return(NULL)
  }
  # nmatrix classes (poly, ns, bs) and matrices are not in `plain`; an expression such
  # as I(x^2) is "numeric", so its source variables are read as well.
  source_vars <- lapply(stats::setNames(nm = predictors), function(v) {
    all.vars(tryCatch(str2lang(v), error = function(e) as.name(v)))
  })
  list(
    labels = labels,
    order = attr(tt, "order"),
    factors = factors,
    classes = classes,
    source_vars = source_vars,
    columns = table(factor(attr(stats::model.matrix(x), "assign"),
                           levels = seq_along(labels))),
    n_columns = ncol(stats::model.matrix(x)) - 1L
  )
}

# TRUE when no source variable appears in two of the given variables.
lm_disjoint <- function(shape, variables) {
  used <- unlist(shape$source_vars[variables], use.names = FALSE)
  !anyDuplicated(used)
}

lm_matches_simple <- function(x, ...) {
  s <- lm_shape(x)
  !is.null(s) && length(s$labels) == 1L && s$order == 1L &&
    identical(unname(s$classes[s$labels]), "numeric") && s$n_columns == 1L &&
    lm_disjoint(s, s$labels)
}

lm_matches_multiple <- function(x, ...) {
  s <- lm_shape(x)
  !is.null(s) && all(s$order == 1L) && s$n_columns >= 2L && lm_disjoint(s, s$labels)
}

# Exactly one two-way term, both main effects present as terms of their own, no
# higher order, and each of the three terms one column (numeric or two-level factor).
lm_matches_moderation <- function(x, ...) {
  s <- lm_shape(x)
  if (is.null(s) || sum(s$order == 2L) != 1L || any(s$order > 2L)) return(FALSE)
  product <- which(s$order == 2L)
  parts <- rownames(s$factors)[s$factors[, product] > 0]
  mains <- s$labels[s$order == 1L]
  if (length(parts) != 2L || !all(parts %in% mains)) return(FALSE)
  if (!lm_disjoint(s, mains)) return(FALSE)
  involved <- c(match(parts, s$labels), product)
  all(s$columns[involved] == 1L)
}

# The rank-deficiency check every lm handler shares.
lm_triage <- function(x, ..., threshold) {
  if (x$rank < ncol(stats::model.matrix(x))) {
    return(list(
      verdict = "needs_raw_data",
      reason = paste0("The model is rank-deficient (some coefficients are NA), so ",
                      "the covariance matrix cannot be inverted to reproduce it.")
    ))
  }
  list(verdict = "summaries_suffice",
       reason = paste0("Summary statistics suffice: n, the means and the covariance ",
                       "matrix of the predictors and outcome determine the fit exactly."))
}

# Relations ----------------------------------------------------------------------

# The numbers of one coefficient: t = b / SE (a quotient, monotone in each input, so
# its corners are exact), the two-sided p from t and the residual df (the p peaks at
# t = 0 inside a box that straddles it, so that point is added), and with `ci` the CI
# bounds b -/+ t_crit SE (monotone in b, SE and df, corners exact). `df` is the name
# of the residual df quantity.
lm_term_relations <- function(term, df = "df2", ci = TRUE) {
  b <- paste0("b:", term)
  se <- paste0("se:", term)
  t <- paste0("t:", term)
  p <- paste0("p:", term)
  at_zero <- function(intervals) {
    bounds <- interval_bounds(intervals[[t]])
    if (!isTRUE(bounds$lower < 0 && bounds$upper > 0)) return(list())
    lapply(corner_points(intervals, df, "p_from_t"),
           function(point) c(stats::setNames(list(0), t), point))
  }
  bound <- function(sign) {
    function(vals, variant) {
      level <- as.numeric(variant)
      vals[[b]] + sign * stats::qt(1 - (1 - level) / 2, vals[[df]]) * vals[[se]]
    }
  }
  c(
    list(
      relation(paste0("t_from_b_se:", term), c(b, se), t,
               function(vals, variant) vals[[b]] / vals[[se]]),
      relation(paste0("p_from_t:", term), c(t, df), p,
               function(vals, variant) 2 * stats::pt(-abs(vals[[t]]), vals[[df]]),
               stationary = at_zero)
    ),
    if (ci) {
      list(
        relation(paste0("ci_lower_from_b_se:", term), c(b, se, df), paste0("ci_lower:", term),
                 bound(-1), variants = confidence_levels(), variant_kind = "level"),
        relation(paste0("ci_upper_from_b_se:", term), c(b, se, df), paste0("ci_upper:", term),
                 bound(1), variants = confidence_levels(), variant_kind = "level")
      )
    }
  )
}

# Adjusted R squared is an exact function of R squared, n and k, and papers often
# label it plain R squared, so every relation that links R squared to something
# else tries both (the `convention` variants r2 and adj_r2): a paper that reports
# the adjusted value as R squared is not flagged, one whose F fits neither is
# (ADR 0020 point 6).
lm_model_relations <- function() {
  list(
    relation("f_from_r2", c("r2", "df1", "df2"), "f", function(vals, variant) {
      r2 <- vals$r2
      if (variant == "adj_r2") r2 <- 1 - (1 - r2) * vals$df2 / (vals$df1 + vals$df2)
      (r2 / vals$df1) / ((1 - r2) / vals$df2)
    }, variants = c("r2", "adj_r2"), variant_kind = "convention"),
    relation("p_model_from_f", c("f", "df1", "df2"), "p_model", function(vals, variant) {
      stats::pf(vals$f, vals$df1, vals$df2, lower.tail = FALSE)
    }),
    lm_adj_r2_relation()
  )
}

# Adjusted R squared from R squared and the model's df (ADR 0031): 1 - (1 - R2) (n - 1) /
# (n - k - 1), where n - 1 = df1 + df2. Exact, so it can falsify; the convention variants of
# f_from_r2 are for a paper that reports the adjusted value under the plain label.
lm_adj_r2_relation <- function() {
  relation("adj_r2_from_r2", c("r2", "df1", "df2"), "adj_r2", function(vals, variant) {
    1 - (1 - vals$r2) * (vals$df1 + vals$df2) / vals$df2
  })
}

# n from the residual df: df2 = n - k - 1 (ADR 0012), with k the number of predictor
# columns. Only offered when k is known.
lm_df_from_n <- function(k) {
  relation("df2_from_n", "n", "df2", function(vals, variant) vals$n - k - 1,
           inverse = function(df2) df2 + k + 1)
}

# beta = b SD_x / SD_y needs the SDs of the APA set; papers use b and beta
# interchangeably, so it never falsifies (ADR 0020 point 6).
lm_beta_relation <- function(term) {
  b <- paste0("b:", term)
  sd_x <- paste0("sd:", term)
  relation(paste0("beta_from_b:", term), c(b, sd_x, "sd:(response)"), paste0("beta:", term),
           function(vals, variant) vals[[b]] * vals[[sd_x]] / vals[["sd:(response)"]],
           falsifies = FALSE)
}

# Simple regression only: r = b SD_x / SD_y, R squared = r squared (or its adjusted
# value, for a paper that calls that R squared) and t squared = F. r squared and
# t squared have their minimum at 0, inside a box that straddles it.
lm_simple_relations <- function(term) {
  b <- paste0("b:", term)
  t <- paste0("t:", term)
  sd_x <- paste0("sd:", term)
  list(
    relation("r_from_b", c(b, sd_x, "sd:(response)"), "r",
             function(vals, variant) vals[[b]] * vals[[sd_x]] / vals[["sd:(response)"]]),
    relation("r2_from_r", c("r", "n"), "r2", function(vals, variant) {
      r2 <- vals$r^2
      if (variant == "adj_r2") r2 <- 1 - (1 - r2) * (vals$n - 1) / (vals$n - 2)
      r2
    }, variants = c("r2", "adj_r2"), variant_kind = "convention",
    stationary = function(intervals) {
      bounds <- interval_bounds(intervals$r)
      if (!isTRUE(bounds$lower < 0 && bounds$upper > 0)) return(list())
      lapply(corner_points(intervals, "n", "r2_from_r"), function(point) c(list(r = 0), point))
    }),
    relation("f_from_t", t, "f", function(vals, variant) vals[[t]]^2,
             stationary = function(intervals) {
               bounds <- interval_bounds(intervals[[t]])
               if (isTRUE(bounds$lower < 0 && bounds$upper > 0)) {
                 list(stats::setNames(list(0), t))
               } else {
                 list()
               }
             })
  )
}

# The relations of a regression handler for the coefficients named in `terms`. `k` is
# the number of predictor columns, when known; simple regression has k = 1.
lm_relations <- function(terms, k = NULL, simple = FALSE) {
  if (simple && length(terms) != 1L) {
    abort_input(
      "Simple regression has one coefficient (the slope); got ",
      paste0("'", terms, "'", collapse = ", "), "."
    )
  }
  c(unlist(lapply(terms, lm_term_relations), recursive = FALSE),
    lm_model_relations(),
    if (!is.null(k)) list(lm_df_from_n(k)),
    lapply(terms, lm_beta_relation),
    if (simple) lm_simple_relations(terms))
}

relations_lm_simple <- function(terms = "x") lm_relations(terms, k = 1L, simple = TRUE)
relations_lm_multiple <- function(terms = "x", k = NULL) lm_relations(terms, k)

# The model's R squared may be an adjusted one reported under the plain label, which
# can be negative.
lm_domains <- function() list(r2 = c(-1, 1))

# Registration -------------------------------------------------------------------

register_lm_family <- function() {
  shared <- list(
    family = "Regression",
    verdict = "summaries_suffice",
    catch = "declare the contrasts; an offset is stored as outcome minus offset",
    lost = c("residual diagnostics (normality, outliers, influence)",
             "refitting with other predictors or transformations",
             "heteroscedasticity-robust standard errors"),
    triage = lm_triage,
    summarise = lm_summarise,
    reproduce = lm_reproduce,
    reported = lm_reported,
    disclosure = lm_disclosure,
    domains = lm_domains(),
    appendix_table = lm_appendix_table,
    appendix_effects = lm_appendix_effects,
    extras = list(
      beta = appendix_extra("beta", lm_beta_values, FALSE)
    )
  )
  register <- function(...) do.call(register_handler, c(shared, list(...)))
  register(
    name = "lm_simple",
    analysis = "Simple regression",
    needs = "n, means and covariance of the predictor and the outcome",
    matches = lm_matches_simple,
    # The APA set: see the comment at the top of this file for what is JARS and what
    # is Manual 3.7.
    design = apa_design(statistic = "t", effect_size = "r2", cells = 2L, ci = TRUE,
                        terms = 1L, model = TRUE),
    sufficient = c("n", "m", "sd", "r"),
    relations = relations_lm_simple,
    example = function() {
      list(x = stats::lm(mpg ~ wt, data = base_data("mtcars")), args = list())
    }
  )
  register(
    name = "lm_multiple",
    analysis = "Multiple regression",
    needs = paste0("n, means and covariance matrix of the model-matrix columns and the ",
                   "outcome (with weights: sum of weights, weighted means and cross-products)"),
    matches = lm_matches_multiple,
    choices = "factor coding",
    design = apa_design(statistic = "t", effect_size = "r2", cells = NA_integer_, ci = TRUE,
                        terms = NA_integer_, model = TRUE),
    sufficient = c("n", "m", "sd", "r_all"),
    relations = relations_lm_multiple,
    example = function() {
      list(x = stats::lm(mpg ~ wt + hp + factor(am), data = base_data("mtcars")),
           args = list())
    }
  )
  register(
    name = "lm_moderation",
    analysis = "Moderation",
    needs = paste0("n, means and covariance matrix of the model-matrix columns (the product ",
                   "term included) and the outcome"),
    matches = lm_matches_moderation,
    choices = c("centering", "coding"),
    design = apa_design(statistic = "t", effect_size = "r2", cells = NA_integer_, ci = TRUE,
                        terms = NA_integer_, model = TRUE),
    sufficient = c("n", "m", "sd", "r_all", "xz"),
    relations = relations_lm_multiple,
    example = function() {
      list(x = stats::lm(mpg ~ wt * hp, data = base_data("mtcars")), args = list())
    }
  )
}

# Appendix (block A) -------------------------------------------------------------

# n, M and SD of every model column and the outcome, their correlations, and the
# coefficient table, all from the stored statistics (never from the raw data). The
# CI of each coefficient follows from b, SE, df and the confidence level the paper
# states; the level is the author's, so the appendix gives no CI of its own.
lm_appendix_table <- function(s, digits = 2L) {
  cp <- lm_crossproducts(s)
  weighted <- isTRUE(s$meta$weighted)
  cov <- cp$s / (cp$w - 1)
  sd <- sqrt(diag(cov))
  cors <- stats::cov2cor(cov)
  vars <- names(s$stats$mean)
  shown <- sub("^\\(response\\)$", paste0(s$meta$response, " (outcome)"), vars)
  reproduced <- source_reproduce(s)
  terms <- sub("^estimate:", "", grep("^estimate:", names(reproduced), value = TRUE))
  coefs <- vapply(terms, function(term) {
    appendix_row(c(term, appendix_number(reproduced[[paste0("estimate:", term)]], digits),
                   appendix_number(reproduced[[paste0("se:", term)]], digits),
                   appendix_number(reproduced[[paste0("t:", term)]], digits),
                   appendix_p(reproduced[[paste0("p:", term)]])))
  }, character(1), USE.NAMES = FALSE)
  c(paste0("n = ", s$stats$n, if (weighted) paste0(" (weighted fit, sum of weights ",
                                                   appendix_number(cp$w, digits), ")")), "",
    appendix_row(c("Variable", "M", paste0("SD", if (weighted) " (weighted)"))),
    appendix_row(rep("---", 3L)),
    vapply(seq_along(vars), function(i) {
      appendix_row(c(shown[i], appendix_number(s$stats$mean[[i]], digits),
                     appendix_number(sd[[i]], digits)))
    }, character(1), USE.NAMES = FALSE),
    "",
    "Correlations of the model columns and the outcome:", "",
    appendix_row(c("", shown)), appendix_row(rep("---", length(vars) + 1L)),
    vapply(seq_along(vars), function(i) {
      appendix_row(c(shown[i], appendix_number(cors[i, ], digits, leading_zero = FALSE)))
    }, character(1), USE.NAMES = FALSE),
    "",
    appendix_row(c("Coefficient", "b", "SE", "t", "p")), appendix_row(rep("---", 5L)),
    coefs,
    "",
    paste0("df = ", appendix_df(reproduced[["dendf"]], digits), " for every coefficient. ",
           "Each coefficient's confidence interval is b \u00b1 t(df) \u00d7 SE at the level ",
           "the paper states."))
}

# Standardized coefficients beta = b SD_x / SD_y, one per model column (the intercept has none).
# A factor's columns are dummy variables, so their beta is the usual software default, which
# is why the relation of check_reported() cannot falsify (see lm_beta_relation).
lm_beta_values <- function(s, answers, digits) {
  cp <- lm_crossproducts(s)
  sd <- sqrt(diag(cp$s / (cp$w - 1)))
  reproduced <- source_reproduce(s)
  terms <- setdiff(sub("^estimate:", "", grep("^estimate:", names(reproduced), value = TRUE)),
                   "(Intercept)")
  if (length(terms) == 0L || !all(terms %in% names(sd))) return(NULL)
  stats::setNames(vapply(terms, function(term) {
    reproduced[[paste0("estimate:", term)]] * sd[[term]] / sd[["(response)"]]
  }, numeric(1)), terms)
}

lm_appendix_effects <- function(s, answers, digits) {
  reproduced <- source_reproduce(s)
  if (is.null(reproduced[["F"]])) return(character())
  c(appendix_row(c("Model", "Value")), appendix_row(c("---", "---")),
    appendix_row(c("R\u00b2", appendix_number(reproduced[["r.squared"]], digits,
                                              leading_zero = FALSE))),
    appendix_row(c("Adjusted R\u00b2", appendix_number(reproduced[["adj.r.squared"]], digits,
                                                       leading_zero = FALSE))),
    appendix_row(c(paste0("F(", appendix_df(reproduced[["numdf"]], digits), ", ",
                          appendix_df(reproduced[["dendf"]], digits), ")"),
                   appendix_number(reproduced[["F"]], digits))),
    appendix_row(c("p (model)", appendix_p(stats::pf(reproduced[["F"]], reproduced[["numdf"]],
                                                     reproduced[["dendf"]], lower.tail = FALSE)))))
}
