# Linear regression (OLS) fitted with lm(), numeric and factor predictors.
# In one line: n, the column means and the covariance matrix of
# [model matrix, outcome] determine every number in summary(lm) exactly.

register_lm <- function() {
  register_handler(
    name = "lm",
    family = "Linear models",
    analysis = "Linear regression, lm() (numeric and factor predictors)",
    verdict = "summaries_suffice",
    needs = "n, means and covariance matrix of the model-matrix columns and the outcome",
    catch = "declare the contrasts; no weights or offset",
    lost = c("residual diagnostics (normality, outliers, influence)",
             "refitting with other predictors or transformations"),
    # Exactly "lm": aov, glm and multivariate lm get their own handlers.
    matches = function(x, ...) identical(class(x), "lm"),
    triage = function(x, ..., threshold) {
      if (!is.null(stats::weights(x)) || !is.null(stats::model.offset(stats::model.frame(x)))) {
        return(list(
          verdict = "needs_raw_data",
          reason = paste0("Weighted regression and offsets have no validated round-trip yet ",
                          "(ADR 0004: no round-trip test, no claim).")
        ))
      }
      if (x$rank < ncol(stats::model.matrix(x))) {
        return(list(
          verdict = "needs_raw_data",
          reason = paste0("The model is rank-deficient (some coefficients are NA), so ",
                          "the covariance matrix cannot be inverted to reproduce it.")
        ))
      }
      list(verdict = "summaries_suffice",
           reason = paste0("Summary statistics suffice: n, the means and the covariance ",
                           "matrix of the predictors and outcome determine the OLS fit exactly."))
    },
    kit = lm_kit,
    refit = lm_refit,
    reported = lm_reported,
    disclosure = lm_disclosure,
    conventions = "contrasts for factor predictors (stored in the kit)",
    example = function() {
      list(x = stats::lm(mpg ~ wt + factor(cyl), data = base_data("mtcars")), args = list())
    }
  )
}

lm_kit <- function(x, ...) {
  intercept <- attr(stats::terms(x), "intercept") == 1L
  predictors <- stats::model.matrix(x)
  predictors <- predictors[, colnames(predictors) != "(Intercept)", drop = FALSE]
  y <- stats::model.response(stats::model.frame(x))
  z <- cbind(predictors, "(response)" = y)
  list(
    stats = list(
      n = nrow(z),
      mean = colMeans(z),
      cov = stats::cov(z)
    ),
    meta = list(
      n = nrow(z),
      intercept = intercept,
      response = deparse(stats::formula(x)[[2L]]),
      formula = deparse(stats::formula(x)),
      contrasts = x$contrasts,
      binary_columns = colnames(z)[apply(z, 2L, function(z) all(z %in% c(0, 1)))]
    )
  )
}

lm_refit <- function(kit) {
  n <- kit$stats$n
  y_col <- "(response)"
  x_cols <- setdiff(names(kit$stats$mean), y_col)
  p <- length(x_cols)
  m <- kit$stats$mean[x_cols]
  y_mean <- kit$stats$mean[[y_col]]
  covs <- kit$stats$cov

  if (kit$meta$intercept) {
    # Centred cross-products: (n - 1) times the covariances.
    tss <- (n - 1) * covs[y_col, y_col]
    if (p > 0L) {
      sxx <- (n - 1) * covs[x_cols, x_cols, drop = FALSE]
      sxy <- (n - 1) * covs[x_cols, y_col]
      sxx_inv <- solve(sxx)
      slopes <- drop(sxx_inv %*% sxy)
      rss <- tss - sum(sxy * slopes)
    } else {
      slopes <- numeric()
      rss <- tss
    }
    rdf <- n - p - 1
    sigma2 <- rss / rdf
    estimate <- c("(Intercept)" = y_mean - sum(m * slopes), slopes)
    var_intercept <- sigma2 * (1 / n + if (p > 0L) drop(t(m) %*% sxx_inv %*% m) else 0)
    se <- sqrt(c(var_intercept, if (p > 0L) sigma2 * diag(sxx_inv)))
    mss <- tss - rss
    df_int <- 1
  } else {
    # Uncentred cross-products, rebuilt from means and covariances.
    xtx <- (n - 1) * covs[x_cols, x_cols, drop = FALSE] + n * tcrossprod(m)
    xty <- (n - 1) * covs[x_cols, y_col] + n * m * y_mean
    yty <- (n - 1) * covs[y_col, y_col] + n * y_mean^2
    xtx_inv <- solve(xtx)
    estimate <- drop(xtx_inv %*% xty)
    rss <- yty - sum(xty * estimate)
    rdf <- n - p
    sigma2 <- rss / rdf
    se <- sqrt(sigma2 * diag(xtx_inv))
    mss <- yty - rss
    df_int <- 0
  }
  names(se) <- names(estimate)
  t <- estimate / se
  r2 <- mss / (mss + rss)
  out <- c(
    coef_vector(estimate, se, t, 2 * stats::pt(-abs(t), rdf)),
    sigma = sqrt(sigma2),
    r.squared = r2,
    adj.r.squared = 1 - (1 - r2) * ((n - df_int) / rdf)
  )
  numdf <- length(estimate) - df_int
  if (numdf > 0L) {
    out <- c(out, F = (mss / numdf) / sigma2, numdf = numdf, dendf = rdf)
  }
  out
}

lm_reported <- function(x, ...) {
  s <- summary(x)
  cf <- s$coefficients
  out <- c(
    # setNames: with one coefficient, cf[, 1L] drops to an unnamed scalar.
    coef_vector(stats::setNames(cf[, 1L], rownames(cf)), cf[, 2L], cf[, 3L], cf[, 4L]),
    sigma = s$sigma,
    r.squared = s$r.squared,
    adj.r.squared = s$adj.r.squared
  )
  if (!is.null(s$fstatistic)) {
    out <- c(out, F = unname(s$fstatistic["value"]), numdf = unname(s$fstatistic["numdf"]),
             dendf = unname(s$fstatistic["dendf"]))
  }
  out
}

# One flat named vector: estimate:<term>, se:<term>, t:<term>, p:<term>.
coef_vector <- function(estimate, se, t, p) {
  terms <- names(estimate)
  stats::setNames(c(estimate, se, t, p),
                  paste0(rep(c("estimate", "se", "t", "p"), each = length(terms)), ":", terms))
}

# Checks the counts the kit reveals. For 0/1 columns (dummies, logicals,
# binary outcome) the kit gives each column's count (n * mean) and each pair's
# 2 x 2 table ((n - 1) * cov + n * mean_i * mean_j). Any cell with
# 0 < count < threshold is flagged. Continuous columns are not checked.
lm_disclosure <- function(k, threshold) {
  n <- k$stats$n
  b <- k$meta$binary_columns
  m <- k$stats$mean[b]
  problems <- character()
  flag <- function(count, label) {
    count <- round(count)
    if (count > 0 && count < threshold) problems <<- c(problems, paste0(label, ": n = ", count))
  }
  for (i in b) {
    flag(n * m[[i]], paste0(i, " = 1"))
    flag(n - n * m[[i]], paste0(i, " = 0"))
  }
  pairs <- if (length(b) >= 2L) utils::combn(b, 2L, simplify = FALSE) else list()
  for (pair in pairs) {
    i <- pair[1]
    j <- pair[2]
    n11 <- (n - 1) * k$stats$cov[i, j] + n * m[[i]] * m[[j]]
    n1x <- n * m[[i]]
    nx1 <- n * m[[j]]
    flag(n11, paste0(i, " = 1 & ", j, " = 1"))
    flag(n1x - n11, paste0(i, " = 1 & ", j, " = 0"))
    flag(nx1 - n11, paste0(i, " = 0 & ", j, " = 1"))
    flag(n - n1x - nx1 + n11, paste0(i, " = 0 & ", j, " = 0"))
  }
  list(passed = length(problems) == 0L,
       checked = "counts of 0/1 columns and their pairwise tables",
       problems = problems)
}
