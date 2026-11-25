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
    fractional = list(df = "welch"),
    domains = list(eta2 = c(-1, 1)),
    sufficient = c("n", "m", "sd"),
    per_cell = c("n", "m", "sd"),
    extras = t_extras(),
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
# df_from_n has inputs n and sd and covers both tests (Welch df needs the SDs).
# Student's df = n1 + n2 - 2 needs n alone: df_from_n_student, which only exists when the
# caller fixes the test to Student (`requires_fixed`). Without a declared test an honest
# Welch paper must not be held to it, so then df is checked from n and sd only.
#
# Effect sizes: how d was computed (pooled SD, average SD, Glass's delta, Hedges'
# g) is rarely declared, so `d_from_descriptives` has falsifies = FALSE: a d that
# no convention reproduces is "not reproducible", never "inconsistent". It is
# signed (first minus second), so a paper that reports |d| for a negative
# difference is "not reproducible" too, again without an accusation.
#
# The CI depends on both the test (its SE and df) and the confidence level, so
# each bound is one relation crossed over both (ADR 0014): the reported numbers
# come from one analysis, so check_reported() requires one test and one level
# across t, df and both bounds, not a choice per relation.
#
# d from t and n (`d_from_t`, Student only, again because t of a Welch test does not give the
# pooled d): d = t sqrt(1/n1 + 1/n2), times Hedges' correction for g. Like d from descriptives
# it cannot falsify. eta squared = t^2 / (t^2 + df) (`eta2_from_t`) is the same for every t
# test; omega squared is the other common convention; neither falsifies.
#
# Monotonicity (checked numerically, see inst/validation/check-range-exactness.R
# for the machinery): t and the Student CI are monotone in each m and sd, d too;
# Welch df and the Welch CI are not monotone in the SDs (df peaks where
# sd1^2 / n1 : sd2^2 / n2 = (n1 - 1) : (n2 - 1); the CI extremum has no closed
# form and depends on the level), so they list stationary points.
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
  # `variant` is c(test = , level = ) for the CI relations (crossed).
  ci_bound <- function(position) {
    function(vals, variant) {
      d <- parts(vals, variant[["test"]])
      t_ci(d$diff, d$se, d$df, "two.sided", as.numeric(variant[["level"]]))[position]
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
  # has no closed-form stationary point, so find it numerically. The extremum
  # depends on the level, so `find` gets the variant; Welch is searched for either
  # test (a poor candidate costs exactness, never correctness).
  # The candidates do not depend on the test, only on the level (`variant_key`),
  # and not on the means (`ignores`): the difference of means shifts the bound.
  welch_extrema <- function(bound) {
    find <- function(corner, i, lower, upper, variant) {
      f <- function(x) {
        corner$sd[i] <- x
        bound(corner, c(test = "welch", level = variant[["level"]]))
      }
      interior_extrema(f, lower, upper)
    }
    attr(find, "variant_key") <- "level"
    attr(find, "ignores") <- "m"
    find
  }
  from <- c("n", "m", "sd")
  tests <- c("student", "welch")
  crossed <- list(test = tests, level = confidence_levels())
  list(
    relation("df_from_n", c("n", "sd"), "df", function(vals, variant) parts(vals, variant)$df,
             variants = tests, variant_kind = "test", stationary = at_welch_df_peak),
    relation("df_from_n_student", "n", "df", function(vals, variant) vals$n[1] + vals$n[2] - 2,
             variants = "student", variant_kind = "test", requires_fixed = "test"),
    relation("t_from_descriptives", from, "t", function(vals, variant) {
      d <- parts(vals, variant)
      d$diff / d$se
    }, variants = tests, variant_kind = "test"),
    relation_p_from_statistic("t"),
    relation("ci_lower_from_descriptives", from, "ci_lower", ci_bound(1L), variants = crossed,
             stationary = sd_stationary(from, "ci_lower_from_descriptives",
                                        welch_extrema(ci_bound(1L)))),
    relation("ci_upper_from_descriptives", from, "ci_upper", ci_bound(2L), variants = crossed,
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
    variant_kind = "convention", falsifies = FALSE),
    relation("d_from_t", c("t", "n"), "d", function(vals, variant) {
      d <- vals$t * sqrt(1 / vals$n[1] + 1 / vals$n[2])
      if (variant[["convention"]] == "hedges_g") {
        d * (1 - 3 / (4 * (vals$n[1] + vals$n[2] - 2) - 1))
      } else {
        d
      }
    }, variants = list(test = "student", convention = c("pooled", "hedges_g")),
    falsifies = FALSE, requires_fixed = "test"),
    relation_eta2_from_t(),
    relation_r2_from_t(),
    relation_omega2_from_t()
  )
}

# eta squared (or omega squared) of a t test from t and its df; shared by the t handlers.
# Both are monotone in |t| and in df, but not in t itself: t^2 has its minimum at 0, so a
# box that straddles 0 gets that point.
relation_eta2_from_t <- function() {
  relation("eta2_from_t", c("t", "df"), "eta2", function(vals, variant) {
    t2 <- vals$t^2
    switch(variant,
           eta2 = t2 / (t2 + vals$df),
           omega2 = (t2 - 1) / (t2 + vals$df + 1))
  }, variants = c("eta2", "omega2"), variant_kind = "convention", falsifies = FALSE,
  stationary = function(intervals) {
    bounds <- interval_bounds(intervals$t)
    if (!isTRUE(bounds$lower < 0 && bounds$upper > 0)) return(list())
    lapply(corner_points(intervals, "df", "eta2_from_t"), function(point) c(list(t = 0), point))
  })
}

# R squared (t^2 / (t^2 + df), the squared point-biserial r for Student's test) and omega
# squared of a t test, as their own quantities (ADR 0031). Welch's t has no agreed R squared and
# omega squared has several formulas, so neither falsifies.
relation_r2_from_t <- function() {
  relation("r2_from_t", c("t", "df"), "r2", function(vals, variant) {
    t2 <- vals$t^2
    t2 / (t2 + vals$df)
  }, falsifies = FALSE, stationary = t_zero_stationary("r2_from_t"))
}

relation_omega2_from_t <- function() {
  relation("omega2_from_t", c("t", "df"), "omega2", function(vals, variant) {
    t2 <- vals$t^2
    (t2 - 1) / (t2 + vals$df + 1)
  }, falsifies = FALSE, stationary = t_zero_stationary("omega2_from_t"))
}

# The minimum in t is at 0 when the box straddles it; the corners miss it.
t_zero_stationary <- function(what) {
  function(intervals) {
    bounds <- interval_bounds(intervals$t)
    if (!isTRUE(bounds$lower < 0 && bounds$upper > 0)) return(list())
    lapply(corner_points(intervals, "df", what), function(point) c(list(t = 0), point))
  }
}

# Appendix extras of the t tests: the R squared family from t and df, which every t test
# stores (reproduce() gives both).
t_extras <- function() {
  from_t <- function(fn) function(s, answers, digits) {
    r <- reproduce(s)
    fn(r[["t"]]^2, r[["df"]])
  }
  list(
    eta2 = appendix_extra("eta2", from_t(function(t2, df) t2 / (t2 + df)), TRUE),
    omega2 = appendix_extra("omega2", from_t(function(t2, df) (t2 - 1) / (t2 + df + 1)), TRUE),
    r2 = appendix_extra("R\u00b2", from_t(function(t2, df) t2 / (t2 + df)), TRUE)
  )
}

t_independent_summarise <- function(x, data, test, alternative = "two.sided", conf_level = 0.95,
                                    ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  mf <- stats::model.frame(x, data)
  if (ncol(mf) != 2L) abort_input("The formula must be `outcome ~ group`.")
  y <- mf[[1L]]
  g <- factor(mf[[2L]])
  if (nlevels(g) != 2L) {
    abort_input("The grouping variable must have exactly 2 groups, not ", nlevels(g), ".")
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
