# Paired-samples t-test, written as `Pair(first, second) ~ 1` (as in t.test).
# In one line: n, mean and SD of the differences (first - second) determine t,
# df, p and the CI exactly. The per-condition means and SDs are not needed, so
# they are not stored: storing less is less identifying (ADR 0004 condition 3).

register_t_paired <- function() {
  register_handler(
    name = "t_paired",
    family = "Longitudinal",
    analysis = "Paired-samples t-test",
    verdict = "summaries_suffice",
    needs = "n, mean and SD of the paired differences",
    catch = "r between conditions is rarely reported; store the differences instead",
    lost = c("checking the distribution and outliers of the differences",
             "nonparametric or robust alternatives",
             "the per-condition means and SDs (report them in the paper)"),
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
  diff <- cols$first - cols$second
  list(
    stats = list(n = length(diff), mean_diff = mean(diff), sd_diff = stats::sd(diff)),
    meta = list(n = length(diff), test = test, mu = mu, alternative = alternative,
                conf_level = conf_level)
  )
}

t_paired_reproduce <- function(s) {
  n <- s$stats$n
  df <- n - 1
  se <- s$stats$sd_diff / sqrt(n)
  t <- (s$stats$mean_diff - s$meta$mu) / se
  ci <- t_ci(s$stats$mean_diff, se, df, s$meta$alternative, s$meta$conf_level)
  c(t = t, df = df, p = t_p(t, df, s$meta$alternative),
    mean_diff = s$stats$mean_diff, ci_lower = ci[1], ci_upper = ci[2])
}

t_paired_reported <- function(x, data, test, mu = 0, alternative = "two.sided",
                              conf_level = 0.95, ...) {
  cols <- paired_columns(x, data)
  r <- stats::t.test(cols$first, cols$second, paired = TRUE, mu = mu,
                     alternative = alternative, conf.level = conf_level)
  c(t = unname(r$statistic), df = unname(r$parameter), p = r$p.value,
    mean_diff = unname(r$estimate), ci_lower = r$conf.int[1], ci_upper = r$conf.int[2])
}
