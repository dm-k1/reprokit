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
    kit = t_independent_kit,
    reproduce = t_independent_reproduce,
    reported = t_independent_reported,
    disclosure = function(k, threshold) {
      small <- k$stats$n[k$stats$n < threshold]
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
    conventions = "test: Student (equal variances) or Welch",
    formula_tests = c("student", "welch"),
    example = function() {
      list(x = extra ~ group, args = list(data = base_data("sleep"), test = "welch"))
    }
  )
}

t_independent_kit <- function(x, data, test, alternative = "two.sided", conf_level = 0.95, ...) {
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

t_independent_reproduce <- function(kit) {
  n <- unname(kit$stats$n)
  m <- unname(kit$stats$mean)
  v <- unname(kit$stats$sd)^2
  mean_diff <- m[1] - m[2]

  if (kit$meta$test == "student") {
    df <- n[1] + n[2] - 2
    pooled_var <- ((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / df
    se <- sqrt(pooled_var * (1 / n[1] + 1 / n[2]))
  } else {
    var_of_mean <- v / n
    se <- sqrt(sum(var_of_mean))
    df <- sum(var_of_mean)^2 / sum(var_of_mean^2 / (n - 1))   # Welch-Satterthwaite
  }
  t <- mean_diff / se
  ci <- t_ci(mean_diff, se, df, kit$meta$alternative, kit$meta$conf_level)
  c(t = t, df = df, p = t_p(t, df, kit$meta$alternative),
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
