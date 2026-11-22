# Pearson correlation test, written as `~ x + y` (as in cor.test).
# In one line: n and r determine t, df, p and the Fisher-z CI exactly.
# Means and SDs are not needed, so they are not stored (ADR 0004 condition 3).

register_cor_pearson <- function() {
  register_handler(
    name = "cor_pearson",
    family = "Correlation",
    analysis = "Pearson correlation test",
    verdict = "summaries_suffice",
    needs = "n and r",
    lost = c("checking the scatterplot for outliers and non-linearity",
             "Spearman, Kendall or robust alternatives"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && identical(test, "pearson")
    },
    summarise = cor_pearson_summarise,
    reproduce = cor_pearson_reproduce,
    reported = cor_pearson_reported,
    formula_tests = "pearson",
    example = function() {
      list(x = ~ mpg + wt, args = list(data = base_data("mtcars"), test = "pearson"))
    }
  )
}

# The two columns of `~ x + y`, complete cases.
pearson_columns <- function(x, data) {
  mf <- stats::model.frame(x, data)
  if (length(x) != 2L || ncol(mf) != 2L) {
    stop("The formula must be `~ x + y`.", call. = FALSE)
  }
  list(x = mf[[1L]], y = mf[[2L]])
}

cor_pearson_summarise <- function(x, data, test, alternative = "two.sided", conf_level = 0.95,
                                  ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  cols <- pearson_columns(x, data)
  n <- length(cols$x)
  list(
    stats = list(n = n, r = stats::cor(cols$x, cols$y)),
    meta = list(n = n, test = test, alternative = alternative, conf_level = conf_level,
                variables = all.vars(x))
  )
}

cor_pearson_reproduce <- function(s) {
  n <- s$stats$n
  r <- s$stats$r
  df <- n - 2
  t <- sqrt(df) * r / sqrt(1 - r^2)
  # Fisher z: atanh(r) is approximately normal with SD 1 / sqrt(n - 3).
  z <- atanh(r)
  sigma <- 1 / sqrt(n - 3)
  conf <- s$meta$conf_level
  ci <- switch(s$meta$alternative,
               two.sided = tanh(z + c(-1, 1) * stats::qnorm(1 - (1 - conf) / 2) * sigma),
               less = c(-1, tanh(z + stats::qnorm(conf) * sigma)),
               greater = c(tanh(z - stats::qnorm(conf) * sigma), 1))
  c(r = r, t = t, df = df, p = t_p(t, df, s$meta$alternative),
    ci_lower = ci[1], ci_upper = ci[2])
}

cor_pearson_reported <- function(x, data, test, alternative = "two.sided", conf_level = 0.95,
                                 ...) {
  cols <- pearson_columns(x, data)
  r <- stats::cor.test(cols$x, cols$y, method = "pearson", alternative = alternative,
                       conf.level = conf_level)
  c(r = unname(r$estimate), t = unname(r$statistic), df = unname(r$parameter),
    p = r$p.value, ci_lower = r$conf.int[1], ci_upper = r$conf.int[2])
}
