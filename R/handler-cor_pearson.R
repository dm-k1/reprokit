# Pearson correlation test, written as `~ x + y` (as in cor.test).
# In one line: n and r determine t, df, p and the Fisher-z CI exactly.
# M and SD of both variables are stored too: they are what a paper's
# descriptives table reports with r (ADR 0009), and readers need them to
# interpret r.

register_cor_pearson <- function() {
  register_handler(
    name = "cor_pearson",
    family = "Correlation",
    analysis = "Pearson correlation test",
    verdict = "apa_suffices",
    needs = "n, M and SD of both variables, and r",
    lost = c("checking the scatterplot for outliers and non-linearity",
             "Spearman, Kendall or robust alternatives"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && identical(test, "pearson")
    },
    summarise = cor_pearson_summarise,
    reproduce = cor_pearson_reproduce,
    reported = cor_pearson_reported,
    disclosure = disclosure_sample_size_only,
    formula_tests = "pearson",
    design = apa_design(statistic = "r", effect_size = "r", cells = 2L),
    sufficient = c("n", "r"),
    not_checkable = c(m = "r does not depend on them", sd = "r does not depend on them"),
    extras = list(
      r2 = appendix_extra("R\u00b2", function(s, answers, digits) {
        source_reproduce(s)[["r"]]^2
      }, TRUE),
      adj_r2 = appendix_extra("adjusted R\u00b2", function(s, answers, digits) {
        1 - (1 - source_reproduce(s)[["r"]]^2) * (s$stats$n - 1) / (s$stats$n - 2)
      }, TRUE)
    ),
    relations = cor_pearson_relations(),
    example = function() {
      list(x = ~ mpg + wt, args = list(data = base_data("mtcars"), test = "pearson"))
    }
  )
}

# Relations between reported quantities (check_reported()). The CI is the
# Fisher-z interval; the confidence level is the variant (90, 95 or 99%).
cor_pearson_relations <- function() {
  fisher_bound <- function(sign) {
    function(vals, variant) {
      # The Fisher interval needs n > 3; below that it does not apply.
      if (vals$n <= 3) return(NaN)
      z_crit <- stats::qnorm(1 - (1 - as.numeric(variant)) / 2)
      tanh(atanh(vals$r) + sign * z_crit / sqrt(vals$n - 3))
    }
  }
  list(
    relation("df_from_n", "n", "df", function(vals, variant) vals$n - 2,
             inverse = function(df) df + 2),
    relation("t_from_r", c("r", "n"), "t", function(vals, variant) {
      vals$r * sqrt(vals$n - 2) / sqrt(1 - vals$r^2)
    }),
    relation_p_from_statistic("t"),
    # r squared is r squared. Its minimum is 0 when the rounding box of r straddles it, which
    # the corners miss, so the exact range is given.
    relation("r2_from_r", "r", "r2", function(vals, variant) vals$r^2,
             range = function(intervals, variant) {
               bounds <- interval_bounds(intervals$r)
               lowest <- if (bounds$lower < 0 && bounds$upper > 0) {
                 0
               } else {
                 min(bounds$lower^2, bounds$upper^2)
               }
               c(lower = lowest, upper = max(bounds$lower^2, bounds$upper^2))
             }),
    # Adjusted R squared of the simple regression of one variable on the other (Wherry).
    relation("adj_r2_from_r2", c("r2", "n"), "adj_r2", function(vals, variant) {
      1 - (1 - vals$r2) * (vals$n - 1) / (vals$n - 2)
    }),
    relation("ci_lower_from_r", c("r", "n"), "ci_lower", fisher_bound(-1),
             variants = confidence_levels(), variant_kind = "level"),
    relation("ci_upper_from_r", c("r", "n"), "ci_upper", fisher_bound(1),
             variants = confidence_levels(), variant_kind = "level")
  )
}

# The two columns of `~ x + y`, complete cases.
pearson_columns <- function(x, data) {
  mf <- stats::model.frame(x, data)
  if (length(x) != 2L || ncol(mf) != 2L) {
    abort_input("The formula must be `~ x + y`.")
  }
  list(x = mf[[1L]], y = mf[[2L]])
}

cor_pearson_summarise <- function(x, data, test, alternative = "two.sided", conf_level = 0.95,
                                  ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  cols <- pearson_columns(x, data)
  n <- length(cols$x)
  list(
    stats = list(
      n = n,
      mean = stats::setNames(c(mean(cols$x), mean(cols$y)), all.vars(x)),
      sd = stats::setNames(c(stats::sd(cols$x), stats::sd(cols$y)), all.vars(x)),
      r = stats::cor(cols$x, cols$y)
    ),
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
