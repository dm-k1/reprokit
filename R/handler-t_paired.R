# Paired-samples t-test, written as `Pair(first, second) ~ 1` (as in t.test).
# In one line: n, the per-condition means and SDs and r between the conditions
# determine t, df, p and the CI exactly, through
#   mean_diff = M1 - M2,   SD_diff^2 = SD1^2 + SD2^2 - 2 r SD1 SD2.
# Stored in the form a paper's descriptives table reports (ADR 0009), not as the
# differences: M and SD per condition are what readers and check_*() expect.
# Precision guard: SD_diff^2 is a difference of two near-equal numbers when r is
# close to 1. Its relative error is about 1e-16 x (SD1^2 + SD2^2) / SD_diff^2,
# so when that factor exceeds 1e6 the round trip could miss 1e-8 (ADR 0005), and
# SD_diff is stored as well. This only happens for SD_diff < ~SD / 1000.

register_t_paired <- function() {
  register_handler(
    name = "t_paired",
    family = "Longitudinal",
    analysis = "Paired-samples t-test",
    verdict = "summaries_suffice",
    needs = "n, M and SD per condition, and r between the conditions",
    catch = "r between the conditions is rarely reported; report it next to M and SD",
    lost = c("checking the distribution and outliers of the differences",
             "nonparametric or robust alternatives"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && identical(test, "paired")
    },
    summarise = t_paired_summarise,
    reproduce = t_paired_reproduce,
    reported = t_paired_reported,
    conventions = "direction of the difference: first minus second in Pair(first, second)",
    formula_tests = "paired",
    example = function() {
      d <- base_data("sleep")
      wide <- data.frame(drug1 = d$extra[d$group == 1], drug2 = d$extra[d$group == 2])
      list(x = Pair(drug1, drug2) ~ 1, args = list(data = wide, test = "paired"))
    }
  )
}

# The two paired columns of `Pair(first, second) ~ 1`, complete pairs.
paired_columns <- function(x, data) {
  mf <- stats::model.frame(x, data)
  pair <- mf[[1L]]
  if (ncol(mf) != 1L || !is.matrix(pair) || ncol(pair) != 2L) {
    stop("The formula must be `Pair(first, second) ~ 1`.", call. = FALSE)
  }
  list(first = pair[, 1L], second = pair[, 2L])
}

t_paired_summarise <- function(x, data, test, mu = 0, alternative = "two.sided",
                               conf_level = 0.95, ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  cols <- paired_columns(x, data)
  labels <- condition_labels(x)
  n <- length(cols$first)
  stats <- list(
    n = n,
    mean = stats::setNames(c(mean(cols$first), mean(cols$second)), labels),
    sd = stats::setNames(c(stats::sd(cols$first), stats::sd(cols$second)), labels),
    r = stats::cor(cols$first, cols$second)
  )
  sd_diff <- stats::sd(cols$first - cols$second)
  if (sum(stats$sd^2) > cancellation_limit * sd_diff^2) stats$sd_diff <- sd_diff
  list(
    stats = stats,
    meta = list(n = n, test = test, mu = mu, alternative = alternative,
                conf_level = conf_level)
  )
}

# Above this ratio (SD1^2 + SD2^2) / SD_diff^2, SD_diff is stored too.
cancellation_limit <- 1e6

# Names of the two conditions in `Pair(first, second) ~ 1`, as written.
condition_labels <- function(x) {
  args <- as.list(x[[2L]])[-1L]
  vapply(args, function(a) paste(deparse(a), collapse = ""), character(1))
}

t_paired_reproduce <- function(s) {
  n <- s$stats$n
  m <- unname(s$stats$mean)
  sd <- unname(s$stats$sd)
  r <- s$stats$r
  mean_diff <- m[1] - m[2]
  sd_diff <- if (is.null(s$stats$sd_diff)) {
    sqrt(sd[1]^2 + sd[2]^2 - 2 * r * sd[1] * sd[2])
  } else {
    s$stats$sd_diff
  }
  df <- n - 1
  se <- sd_diff / sqrt(n)
  t <- (mean_diff - s$meta$mu) / se
  ci <- t_ci(mean_diff, se, df, s$meta$alternative, s$meta$conf_level)
  c(t = t, df = df, p = t_p(t, df, s$meta$alternative),
    mean_diff = mean_diff, ci_lower = ci[1], ci_upper = ci[2])
}

t_paired_reported <- function(x, data, test, mu = 0, alternative = "two.sided",
                              conf_level = 0.95, ...) {
  cols <- paired_columns(x, data)
  r <- stats::t.test(cols$first, cols$second, paired = TRUE, mu = mu,
                     alternative = alternative, conf.level = conf_level)
  c(t = unname(r$statistic), df = unname(r$parameter), p = r$p.value,
    mean_diff = unname(r$estimate), ci_lower = r$conf.int[1], ci_upper = r$conf.int[2])
}
