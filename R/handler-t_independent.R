# Independent-samples t-test, Student or Welch.
# In one line: per-group n, mean and SD determine t, df, p and the CI exactly.

register_t_independent <- function() {
  register_handler(
    name = "t_independent",
    family = "Simple tests",
    analysis = "Independent-samples t-test (Student, Welch)",
    verdict = "summaries_suffice",
    needs = "per-group n, mean and SD",
    catch = "declare Student or Welch",
    lost = c("checking the distribution and outliers within each group",
             "nonparametric or robust alternatives"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && is.character(test) && length(test) == 1L &&
        test %in% c("student", "welch")
    },
    summarise = t_independent_summarise,
    reproduce = t_independent_reproduce,
    reported = t_independent_reported,
    disclosure = function(summary, threshold) {
      small <- summary$stats$n[summary$stats$n < threshold]
      list(
        passed = length(small) == 0L,
        checked = "group sizes",
        problems = if (length(small)) {
          paste0("group '", names(small), "' has n = ", small, " < ", threshold)
        } else {
          character()
        }
      )
    },
    formula_tests = c("student", "welch"),
    design = apa_design(statistic = "t", effect_size = "d", cells = 2L),
    sufficient = c("n", "m", "sd"),
    per_cell = c("n", "m", "sd"),
    relations = t_independent_relations(),
    example = function() {
      list(x = extra ~ group, args = list(data = base_data("sleep"), test = "welch"))
    }
  )
}

# Relations between reported quantities (check_reported()). n, m and sd hold one
# value per group; the difference is first minus second. Student and Welch are
# the variants (kind "test") of the relations that depend on the test, and n is
# exact (an integer count), so only m and sd have a rounding box.
#
# df_from_n has inputs n and sd, not n alone, so that one relation covers both
# tests (Welch df needs the SDs). The cost: df is not checked from n alone.
#
# Effect sizes: how d was computed (pooled SD, average SD, Glass's delta, Hedges'
# g) is rarely declared, so `d_from_descriptives` has falsifies = FALSE: a d that
# no convention reproduces is "not reproducible", never "inconsistent". It is
# signed (first minus second), so a paper that reports |d| for a negative
# difference is "not reproducible" too, again without an accusation.
#
# Monotonicity (checked numerically, see inst/validation/check-range-exactness.R
# for the machinery): t and the Student CI are monotone in each m and sd, d too;
# Welch df and the Welch CI are not monotone in the SDs (df peaks where
# sd1^2 / n1 : sd2^2 / n2 = (n1 - 1) : (n2 - 1)), so they list stationary points.
t_independent_relations <- function() {
  parts <- function(vals, test) {
    n <- vals$n
    v <- vals$sd^2
    diff <- vals$m[1] - vals$m[2]
    if (test == "student") {
      df <- n[1] + n[2] - 2
      se <- sqrt(((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / df * (1 / n[1] + 1 / n[2]))
    } else {
      var_of_mean <- v / n
      se <- sqrt(sum(var_of_mean))
      df <- sum(var_of_mean)^2 / sum(var_of_mean^2 / (n - 1))
    }
    list(diff = diff, se = se, df = df)
  }
  ci_bound <- function(position) {
    function(vals, variant) {
      d <- parts(vals, variant)
      t_ci(d$diff, d$se, d$df, "two.sided", 0.95)[position]
    }
  }
  # Welch df in sd[i], others fixed, is stationary where
  # sd[i]^2 / n[i] = (sd[j]^2 / n[j]) (n[i] - 1) / (n[j] - 1).
  at_welch_df_peak <- sd_stationary(c("n", "sd"), "df_from_n", function(corner, i, lower, upper) {
    j <- 3L - i
    n <- corner$n
    corner$sd[j] * sqrt(n[i] * (n[i] - 1) / (n[j] * (n[j] - 1)))
  })
  # Welch CI bounds in sd[i]: the product of a falling qt(df) and a rising SE
  # has no closed-form stationary point, so find it numerically, for either test.
  welch_extrema <- function(bound) {
    function(corner, i, lower, upper) {
      f <- function(x) {
        corner$sd[i] <- x
        bound(corner, "welch")
      }
      interior_extrema(f, lower, upper)
    }
  }
  from <- c("n", "m", "sd")
  tests <- c("student", "welch")
  list(
    relation("df_from_n", c("n", "sd"), "df", function(vals, variant) parts(vals, variant)$df,
             variants = tests, variant_kind = "test", stationary = at_welch_df_peak),
    relation("t_from_descriptives", from, "t", function(vals, variant) {
      d <- parts(vals, variant)
      d$diff / d$se
    }, variants = tests, variant_kind = "test"),
    relation_p_from_t(),
    relation("ci_lower_from_descriptives", from, "ci_lower", ci_bound(1L), variants = tests,
             variant_kind = "test",
             stationary = sd_stationary(from, "ci_lower_from_descriptives",
                                        welch_extrema(ci_bound(1L)))),
    relation("ci_upper_from_descriptives", from, "ci_upper", ci_bound(2L), variants = tests,
             variant_kind = "test",
             stationary = sd_stationary(from, "ci_upper_from_descriptives",
                                        welch_extrema(ci_bound(2L)))),
    relation("d_from_descriptives", from, "d", function(vals, variant) {
      n <- vals$n
      v <- vals$sd^2
      diff <- vals$m[1] - vals$m[2]
      pooled <- sqrt(((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / (n[1] + n[2] - 2))
      switch(variant,
             pooled = diff / pooled,
             hedges_g = diff / pooled * (1 - 3 / (4 * (n[1] + n[2] - 2) - 1)),
             average = diff / sqrt(mean(v)),
             glass_sd1 = diff / vals$sd[1],
             glass_sd2 = diff / vals$sd[2])
    }, variants = c("pooled", "hedges_g", "average", "glass_sd1", "glass_sd2"),
    variant_kind = "convention", falsifies = FALSE)
  )
}

t_independent_summarise <- function(x, data, test, alternative = "two.sided", conf_level = 0.95,
                                    ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  mf <- stats::model.frame(x, data)
  if (ncol(mf) != 2L) stop("The formula must be `outcome ~ group`.", call. = FALSE)
  y <- mf[[1L]]
  g <- factor(mf[[2L]])
  if (nlevels(g) != 2L) {
    stop("The grouping variable must have exactly 2 groups, not ", nlevels(g), ".", call. = FALSE)
  }
  list(
    stats = list(
      n = c(table(g)),
      mean = tapply(y, g, mean),
      sd = tapply(y, g, stats::sd)
    ),
    meta = list(
      n = length(y),
      test = test,
      alternative = alternative,
      conf_level = conf_level,
      groups = levels(g)
    )
  )
}

t_independent_reproduce <- function(s) {
  n <- unname(s$stats$n)
  m <- unname(s$stats$mean)
  v <- unname(s$stats$sd)^2
  mean_diff <- m[1] - m[2]

  if (s$meta$test == "student") {
    df <- n[1] + n[2] - 2
    pooled_var <- ((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / df
    se <- sqrt(pooled_var * (1 / n[1] + 1 / n[2]))
  } else {
    var_of_mean <- v / n
    se <- sqrt(sum(var_of_mean))
    df <- sum(var_of_mean)^2 / sum(var_of_mean^2 / (n - 1))   # Welch-Satterthwaite
  }
  t <- mean_diff / se
  ci <- t_ci(mean_diff, se, df, s$meta$alternative, s$meta$conf_level)
  c(t = t, df = df, p = t_p(t, df, s$meta$alternative),
    mean_diff = mean_diff, ci_lower = ci[1], ci_upper = ci[2])
}

t_independent_reported <- function(x, data, test, alternative = "two.sided",
                                   conf_level = 0.95, ...) {
  r <- stats::t.test(x, data = data, var.equal = test == "student",
                     alternative = alternative, conf.level = conf_level)
  c(t = unname(r$statistic), df = unname(r$parameter), p = r$p.value,
    mean_diff = unname(r$estimate[1] - r$estimate[2]),
    ci_lower = r$conf.int[1], ci_upper = r$conf.int[2])
}

# p-value and CI of a t statistic, shared by the t-test handlers.
t_p <- function(t, df, alternative) {
  switch(alternative,
         two.sided = 2 * stats::pt(-abs(t), df),
         less = stats::pt(t, df),
         greater = stats::pt(t, df, lower.tail = FALSE))
}

t_ci <- function(estimate, se, df, alternative, conf_level) {
  switch(alternative,
         two.sided = estimate + c(-1, 1) * stats::qt(1 - (1 - conf_level) / 2, df) * se,
         less = c(-Inf, estimate + stats::qt(conf_level, df) * se),
         greater = c(estimate - stats::qt(conf_level, df) * se, Inf))
}
