# One-sample t-test against a test value mu.
# In one line: n, mean and SD of the outcome determine t, df, p and the CI exactly.

register_t_one_sample <- function() {
  register_handler(
    name = "t_one_sample",
    family = "Simple tests",
    analysis = "One-sample t-test",
    verdict = "summaries_suffice",
    needs = "n, mean and SD of the outcome, and the test value",
    catch = "declare the test value mu",
    lost = c("checking the distribution and outliers",
             "nonparametric or robust alternatives"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && identical(test, "one_sample")
    },
    summarise = t_one_sample_summarise,
    reproduce = t_one_sample_reproduce,
    reported = t_one_sample_reported,
    conventions = "test value mu (default 0)",
    formula_tests = "one_sample",
    design = apa_design(statistic = "t", effect_size = "d", cells = 1L),
    reproducible = c("n", "m", "sd"),
    relations = t_one_sample_relations,
    example = function() {
      list(x = extra ~ 1, args = list(data = base_data("sleep"), test = "one_sample", mu = 0.5))
    }
  )
}

# Relations between reported quantities (check_reported()) for the test value
# `mu`. All of t, df, p and the CI of the mean are monotone in m and sd (n is an
# exact count), so corners give the exact range. The CI of the mean does not
# depend on mu. d = (M - mu) / SD, or Hedges' g with the small-sample factor; as
# which of them a paper reports is rarely declared, d_from_descriptives has
# falsifies = FALSE (a d that neither convention reproduces is "not
# reproducible", never "inconsistent"). It is signed, like the t_independent d.
t_one_sample_relations <- function(mu = 0) {
  se <- function(vals) vals$sd / sqrt(vals$n)
  ci_bound <- function(position) {
    function(vals, variant) {
      t_ci(vals$m, se(vals), vals$n - 1, "two.sided", 0.95)[position]
    }
  }
  from <- c("n", "m", "sd")
  list(
    relation("df_from_n", "n", "df", function(vals, variant) vals$n - 1,
             inverse = function(df) df + 1),
    relation("t_from_descriptives", from, "t", function(vals, variant) {
      (vals$m - mu) / se(vals)
    }),
    relation_p_from_t(),
    relation("ci_lower_from_descriptives", from, "ci_lower", ci_bound(1L)),
    relation("ci_upper_from_descriptives", from, "ci_upper", ci_bound(2L)),
    relation("d_from_descriptives", from, "d", function(vals, variant) {
      d <- (vals$m - mu) / vals$sd
      if (variant == "hedges_g") d * (1 - 3 / (4 * (vals$n - 1) - 1)) else d
    }, variants = c("cohen", "hedges_g"), variant_kind = "convention", falsifies = FALSE)
  )
}

# The outcome of `y ~ 1`, complete cases.
one_sample_outcome <- function(x, data) {
  mf <- stats::model.frame(x, data)
  if (ncol(mf) != 1L || length(x) != 3L || !identical(x[[3L]], 1)) {
    stop("The formula must be `outcome ~ 1`.", call. = FALSE)
  }
  mf[[1L]]
}

t_one_sample_summarise <- function(x, data, test, mu = 0, alternative = "two.sided",
                                   conf_level = 0.95, ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  y <- one_sample_outcome(x, data)
  list(
    stats = list(n = length(y), mean = mean(y), sd = stats::sd(y)),
    meta = list(n = length(y), test = test, mu = mu, alternative = alternative,
                conf_level = conf_level)
  )
}

t_one_sample_reproduce <- function(s) {
  n <- s$stats$n
  df <- n - 1
  se <- s$stats$sd / sqrt(n)
  t <- (s$stats$mean - s$meta$mu) / se
  ci <- t_ci(s$stats$mean, se, df, s$meta$alternative, s$meta$conf_level)
  c(t = t, df = df, p = t_p(t, df, s$meta$alternative),
    mean = s$stats$mean, ci_lower = ci[1], ci_upper = ci[2])
}

t_one_sample_reported <- function(x, data, test, mu = 0, alternative = "two.sided",
                                  conf_level = 0.95, ...) {
  r <- stats::t.test(one_sample_outcome(x, data), mu = mu, alternative = alternative,
                     conf.level = conf_level)
  c(t = unname(r$statistic), df = unname(r$parameter), p = r$p.value,
    mean = unname(r$estimate), ci_lower = r$conf.int[1], ci_upper = r$conf.int[2])
}
