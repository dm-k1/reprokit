# Simple mediation: X -> M -> Y, written as `y ~ x` with `test = "mediation"` and
# `mediator = "m"` (ADR 0020 point 2). It is three regressions: M on X (path a), Y on
# X (path c) and Y on X and M (paths b and c'). The indirect effect a*b is inferred
# by a bootstrap that resamples cases, which summaries cannot do, so it is neither
# reproduced nor checked (ADR 0020 point 7); the bootstrap CI is in `lost`.
# In one line: n, the means and the covariance of X, M and Y determine the three
# regressions exactly.
#
# Where the APA set comes from (ADR 0018 point 2):
#  - JARS-Quant Table 1: per path b, SE, t, df, p (the tests of the model's
#    coefficients) and n with the M and SD of the outcomes;
#  - Manual section 3.7, NOT JARS: the M and SD of X.
# The indirect effect and its CI are reported by every mediation paper (JARS asks
# for the effect-size estimate with its CI), but they are not checkable here.

mediation_paths <- function() c("a", "b", "c", "c_prime")

register_mediation_simple <- function() {
  register_handler(
    name = "mediation_simple",
    family = "Regression",
    analysis = "Simple mediation",
    verdict = "summaries_suffice",
    needs = "n, means and covariance matrix of X, M and Y",
    catch = "give the mediator by name: test = \"mediation\", mediator = \"m\"",
    lost = c("the bootstrap (or Monte Carlo) confidence interval of the indirect effect a*b",
             "residual diagnostics (normality, outliers, influence)",
             "refitting with covariates, other mediators or transformations"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && identical(test, "mediation")
    },
    summarise = mediation_summarise,
    reproduce = mediation_reproduce,
    reported = mediation_reported,
    formula_tests = "mediation",
    design = apa_design(statistic = "t", effect_size = "indirect", cells = 3L, ci = TRUE,
                        terms = 4L, model = FALSE,
                        extra = c("indirect", "indirect_ci_lower", "indirect_ci_upper")),
    sufficient = c("n", "m", "sd", "r_all"),
    # The wording of plan section 4 (named paths cannot be generated from the quantities).
    coverage = c(sufficient = "n, M, SD and correlations of X, M, Y",
                 missing = "the correlations of X, M, Y",
                 checkable = "a, b, c, c\u2032 (b, SE, t, p, CI each)"),
    not_checkable = c(
      indirect = "inferred by bootstrap, which resamples cases; summaries cannot",
      indirect_ci_lower = "inferred by bootstrap, which resamples cases; summaries cannot",
      indirect_ci_upper = "inferred by bootstrap, which resamples cases; summaries cannot"
    ),
    relations = relations_mediation,
    appendix_table = mediation_appendix_table,
    appendix_effects = function(s, answers, digits) character(),
    example = function() {
      list(x = mpg ~ wt, args = list(data = base_data("mtcars"), test = "mediation",
                                     mediator = "hp"))
    }
  )
}

# Each path is a regression coefficient with its own df: n - 2 for a and c (one
# predictor), n - 3 for b and c' (two). Each path's CI is the lm CI relation
# (b -/+ t_crit SE at the df of the path, levels of ADR 0014). No relation involves a*b.
relations_mediation <- function(terms = mediation_paths()) {
  unknown <- setdiff(terms, mediation_paths())
  if (length(unknown) > 0L) {
    stop("Unknown mediation path ", paste0("'", unknown, "'", collapse = ", "),
         ". The paths are a, b, c and c_prime.", call. = FALSE)
  }
  unlist(lapply(terms, function(path) {
    k <- if (path %in% c("a", "c")) 1 else 2
    c(
      list(relation(paste0("df_from_n:", path), "n", paste0("df:", path),
                    function(vals, variant) vals$n - k - 1,
                    inverse = function(df) df + k + 1)),
      lm_term_relations(path, df = paste0("df:", path))
    )
  }), recursive = FALSE)
}

mediation_columns <- function(x, data, mediator) {
  if (!inherits(x, "formula") || length(x) != 3L) stop("The formula must be `y ~ x`.", call. = FALSE)
  if (is.null(data)) stop("Give the data: mediation needs `data`.", call. = FALSE)
  if (!is.character(mediator) || length(mediator) != 1L) {
    stop("Name the mediator with `mediator = \"m\"` (one column of the data).", call. = FALSE)
  }
  y_name <- deparse(x[[2L]])
  x_name <- deparse(x[[3L]])
  if (!all(c(y_name, x_name, mediator) %in% names(data))) {
    stop("The formula must be `y ~ x` with plain column names, and the mediator a column ",
         "of the data.", call. = FALSE)
  }
  if (anyDuplicated(c(y_name, x_name, mediator))) {
    stop("X, M and Y must be three different columns.", call. = FALSE)
  }
  cols <- data[, c(x_name, mediator, y_name), drop = FALSE]
  if (!all(vapply(cols, is.numeric, logical(1)))) {
    stop("Mediation needs numeric X, M and Y.", call. = FALSE)
  }
  list(data = stats::na.omit(cols), names = c(x = x_name, m = mediator, y = y_name))
}

mediation_summarise <- function(x, data = NULL, test = NULL, mediator = NULL, ...) {
  cols <- mediation_columns(x, data, mediator)
  z <- as.matrix(cols$data)
  n <- nrow(z)
  list(
    stats = list(n = n, mean = colMeans(z), cov = stats::cov(z)),
    meta = list(n = n, test = "mediation", x = cols$names[["x"]], m = cols$names[["m"]],
                y = cols$names[["y"]])
  )
}

# A regression of `response` on `predictors`, as the stat object lm_reproduce() takes.
mediation_sub <- function(s, predictors, response) {
  keep <- c(predictors, response)
  mean <- s$stats$mean[keep]
  names(mean)[length(keep)] <- "(response)"
  cov <- s$stats$cov[keep, keep, drop = FALSE]
  dimnames(cov) <- list(names(mean), names(mean))
  list(stats = list(n = s$stats$n, mean = mean, cov = cov), meta = list(intercept = TRUE))
}

mediation_pack <- function(estimate, se, t, p, df) {
  paths <- mediation_paths()
  out <- unlist(lapply(paths, function(path) {
    stats::setNames(c(estimate[[path]], se[[path]], t[[path]], p[[path]], df[[path]]),
                    paste0(c("estimate", "se", "t", "p", "df"), ":", path))
  }))
  out
}

mediation_reproduce <- function(s) {
  xn <- s$meta$x
  mn <- s$meta$m
  yn <- s$meta$y
  m_on_x <- lm_reproduce(mediation_sub(s, xn, mn))
  y_on_x <- lm_reproduce(mediation_sub(s, xn, yn))
  y_on_xm <- lm_reproduce(mediation_sub(s, c(xn, mn), yn))
  mediation_gather(m_on_x, y_on_x, y_on_xm, xn, mn)
}

mediation_gather <- function(m_on_x, y_on_x, y_on_xm, xn, mn) {
  pick <- function(fit, what, term) fit[[paste0(what, ":", term)]]
  paths <- list(a = list(m_on_x, xn), b = list(y_on_xm, mn), c = list(y_on_x, xn),
                c_prime = list(y_on_xm, xn))
  per <- function(what) vapply(paths, function(p) pick(p[[1L]], what, p[[2L]]), numeric(1))
  df <- vapply(paths, function(p) p[[1L]][["dendf"]], numeric(1))
  mediation_pack(per("estimate"), per("se"), per("t"), per("p"), df)
}

mediation_reported <- function(x, data = NULL, test = NULL, mediator = NULL, ...) {
  cols <- mediation_columns(x, data, mediator)
  d <- cols$data
  xn <- cols$names[["x"]]
  mn <- cols$names[["m"]]
  yn <- cols$names[["y"]]
  fit <- function(response, predictors) {
    lm_reported(stats::lm(stats::reformulate(paste0("`", predictors, "`"),
                                             paste0("`", response, "`")), data = d))
  }
  mediation_gather(fit(mn, xn), fit(yn, xn), fit(yn, c(xn, mn)), xn, mn)
}

# Appendix (block A): n, M, SD and correlations of X, M and Y, and the four paths. The
# indirect effect is not in the appendix's numbers: block D says why.
mediation_appendix_table <- function(s, digits = 2L) {
  cov <- s$stats$cov
  vars <- names(s$stats$mean)
  cors <- stats::cov2cor(cov)
  reproduced <- reproduce(s)
  labels <- c(a = "a (X to M)", b = "b (M to Y, X held constant)", c = "c (X to Y)",
              c_prime = "c' (X to Y, M held constant)")
  c(paste0("n = ", s$stats$n), "",
    appendix_row(c("Variable", "M", "SD")), appendix_row(rep("---", 3L)),
    vapply(seq_along(vars), function(i) {
      appendix_row(c(paste0(vars[i], c(" (X)", " (M)", " (Y)")[i]),
                     appendix_number(s$stats$mean[[i]], digits),
                     appendix_number(sqrt(cov[i, i]), digits)))
    }, character(1), USE.NAMES = FALSE),
    "",
    "Correlations of X, M and Y:", "",
    appendix_row(c("", vars)), appendix_row(rep("---", length(vars) + 1L)),
    vapply(seq_along(vars), function(i) {
      appendix_row(c(vars[i], appendix_number(cors[i, ], digits, leading_zero = FALSE)))
    }, character(1), USE.NAMES = FALSE),
    "",
    appendix_row(c("Path", "b", "SE", "t", "df", "p")), appendix_row(rep("---", 6L)),
    vapply(names(labels), function(path) {
      appendix_row(c(labels[[path]],
                     appendix_number(reproduced[[paste0("estimate:", path)]], digits),
                     appendix_number(reproduced[[paste0("se:", path)]], digits),
                     appendix_number(reproduced[[paste0("t:", path)]], digits),
                     appendix_df(reproduced[[paste0("df:", path)]], digits),
                     appendix_p(reproduced[[paste0("p:", path)]])))
    }, character(1), USE.NAMES = FALSE))
}
