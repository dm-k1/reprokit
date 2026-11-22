# Linear regression (OLS or weighted least squares) fitted with lm(), numeric
# and factor predictors, optional weights and offset.
# In one line: n, the sum of weights, the (weighted) column means and the
# (weighted) centred cross-products of [model matrix, outcome - offset]
# determine every number in summary(lm) exactly.
# Unweighted is the special case: sum of weights = n, cross-products = (n - 1) * cov.

register_lm <- function() {
  register_handler(
    name = "lm",
    family = "Linear models",
    analysis = "Linear regression, lm() (numeric and factor predictors, weights, offset)",
    verdict = "summaries_suffice",
    needs = paste0("n, means and covariance matrix of the model-matrix columns and the outcome ",
                   "(with weights: sum of weights, weighted means and weighted cross-products)"),
    catch = "declare the contrasts; an offset is stored as outcome minus offset",
    lost = c("residual diagnostics (normality, outliers, influence)",
             "refitting with other predictors or transformations"),
    # Exactly "lm": aov, glm and multivariate lm get their own handlers.
    matches = function(x, ...) identical(class(x), "lm"),
    triage = function(x, ..., threshold) {
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
    },
    summarise = lm_summarise,
    reproduce = lm_reproduce,
    reported = lm_reported,
    disclosure = lm_disclosure,
    conventions = "contrasts for factor predictors (stored with the statistics)",
    example = function() {
      list(x = stats::lm(mpg ~ wt + factor(cyl), data = base_data("mtcars")), args = list())
    }
  )
}

# summary.lm treats an offset as part of the outcome (fitted - offset is what
# it explains) and rows with weight 0 as absent, so the summary does the same.
lm_summarise <- function(x, ...) {
  mf <- stats::model.frame(x)
  predictors <- stats::model.matrix(x)
  predictors <- predictors[, colnames(predictors) != "(Intercept)", drop = FALSE]
  offset <- stats::model.offset(mf)
  y <- stats::model.response(mf) - if (is.null(offset)) 0 else offset
  z <- cbind(predictors, "(response)" = y)
  w <- stats::weights(x)
  weighted <- !is.null(w)
  if (weighted) {
    z <- z[w > 0, , drop = FALSE]
    w <- w[w > 0]
  }
  n <- nrow(z)
  stats <- if (weighted) {
    mean_w <- colSums(w * z) / sum(w)
    centred <- sweep(z, 2L, mean_w)
    list(n = n, sum_w = sum(w), mean = mean_w, sscp = crossprod(sqrt(w) * centred))
  } else {
    list(n = n, mean = colMeans(z), cov = stats::cov(z))
  }
  list(
    stats = stats,
    meta = list(
      n = n,
      intercept = attr(stats::terms(x), "intercept") == 1L,
      weighted = weighted,
      offset = !is.null(offset),
      response = deparse(stats::formula(x)[[2L]]),
      formula = deparse(stats::formula(x)),
      contrasts = x$contrasts
    ),
    # Used by the disclosure check only; not stored.
    check = list(binary_counts = binary_counts(z))
  )
}

# Sum of weights W and centred cross-products S, for both forms (weighted, unweighted).
lm_crossproducts <- function(s) {
  if (isTRUE(s$meta$weighted)) {
    list(w = s$stats$sum_w, s = s$stats$sscp)
  } else {
    list(w = s$stats$n, s = (s$stats$n - 1) * s$stats$cov)
  }
}

lm_reproduce <- function(s) {
  n <- s$stats$n
  cp <- lm_crossproducts(s)
  sum_w <- cp$w
  sscp <- cp$s
  y_col <- "(response)"
  x_cols <- setdiff(names(s$stats$mean), y_col)
  p <- length(x_cols)
  m <- s$stats$mean[x_cols]
  y_mean <- s$stats$mean[[y_col]]

  if (s$meta$intercept) {
    tss <- sscp[y_col, y_col]
    if (p > 0L) {
      sxx <- sscp[x_cols, x_cols, drop = FALSE]
      sxy <- sscp[x_cols, y_col]
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
    var_intercept <- sigma2 * (1 / sum_w + if (p > 0L) drop(t(m) %*% sxx_inv %*% m) else 0)
    se <- sqrt(c(var_intercept, if (p > 0L) sigma2 * diag(sxx_inv)))
    mss <- tss - rss
    df_int <- 1
  } else {
    # Uncentred cross-products, rebuilt from the means and centred cross-products.
    xtx <- sscp[x_cols, x_cols, drop = FALSE] + sum_w * tcrossprod(m)
    xty <- sscp[x_cols, y_col] + sum_w * m * y_mean
    yty <- sscp[y_col, y_col] + sum_w * y_mean^2
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

# Counts of people behind the 0/1 columns (dummies, logicals, binary outcome):
# n, each column's count of 1s and each pair's count of (1, 1). Unweighted
# counts over the rows in the fit, so weights cannot hide a small cell.
binary_counts <- function(z) {
  b <- colnames(z)[apply(z, 2L, function(col) all(col %in% c(0, 1)))]
  zb <- z[, b, drop = FALSE]
  list(n = nrow(z), ones = colSums(zb), both = crossprod(zb))
}

# Any cell of a 0/1 column or of a pair's 2 x 2 table with 0 < count < threshold
# is flagged. Continuous columns are not checked.
lm_disclosure <- function(summary, threshold) {
  counts <- summary$check$binary_counts
  n <- counts$n
  ones <- counts$ones
  b <- names(ones)
  problems <- character()
  flag <- function(count, label) {
    if (count > 0 && count < threshold) problems <<- c(problems, paste0(label, ": n = ", count))
  }
  for (i in b) {
    flag(ones[[i]], paste0(i, " = 1"))
    flag(n - ones[[i]], paste0(i, " = 0"))
  }
  pairs <- if (length(b) >= 2L) utils::combn(b, 2L, simplify = FALSE) else list()
  for (pair in pairs) {
    i <- pair[1]
    j <- pair[2]
    n11 <- counts$both[i, j]
    flag(n11, paste0(i, " = 1 & ", j, " = 1"))
    flag(ones[[i]] - n11, paste0(i, " = 1 & ", j, " = 0"))
    flag(ones[[j]] - n11, paste0(i, " = 0 & ", j, " = 1"))
    flag(n - ones[[i]] - ones[[j]] + n11, paste0(i, " = 0 & ", j, " = 0"))
  }
  list(passed = length(problems) == 0L,
       checked = "counts of 0/1 columns and their pairwise tables",
       problems = problems)
}
