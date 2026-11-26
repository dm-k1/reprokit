# One-way ANOVA (classical F test, equal variances assumed).
# In one line: per-group n, mean and SD determine F, df1, df2, p and eta squared exactly.

register_anova_oneway <- function() {
  register_handler(
    name = "anova_oneway",
    family = "Simple tests",
    analysis = "One-way ANOVA (between-subjects)",
    verdict = "apa_suffices",
    needs = "per-group n, mean and SD",
    catch = "declare eta squared or omega squared",
    lost = c("checking the distribution and outliers within each group",
             "checking equal variances beyond the group SDs",
             "post-hoc comparisons that need the raw data or the error term of a model"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && is.character(test) && length(test) == 1L && test == "oneway"
    },
    summarise = anova_oneway_summarise,
    reproduce = anova_oneway_reproduce,
    reported = anova_oneway_reported,
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
    choices = "variance assumption: equal variances (classical F test, not Welch's)",
    formula_tests = "oneway",
    design = apa_design(statistic = "f", effect_size = "eta2", cells = NA_integer_, ci = FALSE),
    sufficient = c("n", "m", "sd"),
    per_cell = c("n", "m", "sd"),
    # Omega squared is negative when F < 1, unlike eta squared (ADR 0022).
    domains = list(eta2 = c(-1, 1)),
    extras = list(
      r2 = appendix_extra("R\u00b2", function(s, answers, digits) {
        oneway_effect_sizes(s)[["eta2"]]
      }, TRUE),
      omega2 = appendix_extra("\u03c9\u00b2", function(s, answers, digits) {
        oneway_effect_sizes(s)[["omega2"]]
      }, TRUE)
    ),
    relations = anova_oneway_relations(),
    example = function() {
      list(x = weight ~ group, args = list(data = base_data("PlantGrowth"), test = "oneway"))
    }
  )
}

# Sums of squares of one-way ANOVA from per-group n, mean and SD.
oneway_ssw <- function(n, sd) sum((n - 1) * sd^2)

oneway_ssb <- function(n, m) {
  centre <- mean(m)
  grand <- sum(n * (m - centre)) / sum(n)
  sum(n * (m - centre - grand)^2)
}

oneway_f <- function(n, m, sd) {
  k <- length(n)
  (oneway_ssb(n, m) / (k - 1)) / (oneway_ssw(n, sd) / (sum(n) - k))
}

# Largest SSb over the box of the means. SSb is convex in the means, so the
# maximum is at a corner of the box: 2^k of them.
oneway_ssb_max <- function(n, lower, upper) {
  k <- length(n)
  up <- vapply(seq_len(k), function(d) bitwAnd(seq_len(2^k) - 1L, bitwShiftL(1L, d - 1L)) > 0L,
               logical(2^k))
  corners <- matrix(lower, nrow = 2^k, ncol = k, byrow = TRUE)
  corners[up] <- matrix(upper, nrow = 2^k, ncol = k, byrow = TRUE)[up]
  centre <- mean(c(lower, upper))
  corners <- corners - centre
  grand <- as.vector(corners %*% n) / sum(n)
  max(rowSums(t(t(corners^2) * n)) - sum(n) * grand^2)
}

# Smallest SSb over the box of the means. SSb = min over c of sum(n_i (m_i - c)^2),
# so its minimum over the box is min over c of g(c) = sum(n_i dist(c, [lo_i, hi_i])^2).
# g is piecewise quadratic between the sorted interval ends; on each piece the
# minimiser is the weighted mean of the end points of the intervals on either side,
# clipped to the piece. g = 0 on a piece every interval touches (the intervals
# share a point), which is why the corners alone are not enough: they never reach 0.
oneway_ssb_min <- function(n, lower, upper) {
  g <- function(c) sum(n * pmax(lower - c, 0, c - upper)^2)
  breaks <- sort(unique(c(lower, upper)))
  best <- min(vapply(breaks, g, numeric(1)))
  for (j in seq_len(length(breaks) - 1L)) {
    from <- breaks[j]
    to <- breaks[j + 1L]
    below <- upper <= from
    above <- lower >= to
    weights <- c(n[below], n[above])
    if (length(weights) == 0L) return(0)
    points <- c(upper[below], lower[above])
    best <- min(best, g(min(max(sum(weights * points) / sum(weights), from), to)))
  }
  best
}

# Exact range of F over the box of n, m and sd. n is normally exact (a count);
# if not, the extremes are taken over its corners as well (ADR 0015). A range that
# cannot be bounded is not finite, which the engine reads as "not evaluable".
oneway_f_range <- function(intervals, variant) {
  unbounded <- c(lower = NA_real_, upper = NA_real_)
  n <- interval_bounds(intervals$n)
  m <- interval_bounds(intervals$m)
  sd <- interval_bounds(intervals$sd)
  k <- length(n$lower)
  free <- which(n$lower != n$upper)
  if (k < 2L || k > 12L || length(free) > 8L || sum(n$lower) <= k) return(unbounded)
  n_grid <- expand.grid(lapply(seq_len(k), function(i) unique(c(n$lower[i], n$upper[i]))))
  sd_lower <- pmax(sd$lower, 0)
  lows <- numeric()
  highs <- numeric()
  for (row in seq_len(nrow(n_grid))) {
    counts <- as.numeric(n_grid[row, ])
    if (sum(counts) <= k || any(counts < 2)) next
    scale <- (sum(counts) - k) / (k - 1)
    ssw_high <- oneway_ssw(counts, sd$upper)
    ssw_low <- oneway_ssw(counts, sd_lower)
    ssb_low <- oneway_ssb_min(counts, m$lower, m$upper)
    lows <- c(lows, if (ssb_low == 0) 0 else ssb_low / ssw_high * scale)
    highs <- c(highs, oneway_ssb_max(counts, m$lower, m$upper) / ssw_low * scale)
  }
  if (length(lows) == 0L) return(unbounded)
  c(lower = min(lows), upper = max(highs))
}

# Relations between reported quantities. n, m and sd hold one value per group.
#
# `f_from_descriptives` has its own exact range (oneway_f_range): SSw is monotone in
# the SDs and SSb has no corner structure for its minimum (see oneway_ssb_min).
# The df follow from n alone. The p-value is monotone in F. eta squared is
# F df1 / (F df1 + df2); omega squared is df1 (F - 1) / (df1 F + df2 + 1) (it can be
# negative). Both are monotone in each of F, df1 and df2 separately, so the corners
# of the box are exact. Authors rarely say which they report, so the relation is a
# convention with falsifies = FALSE: an eta2 that neither reproduces is "not
# reproducible", never "inconsistent".
anova_oneway_relations <- function() {
  from <- c("n", "m", "sd")
  list(
    relation("df1_from_n", "n", "df1", function(vals, variant) length(vals$n) - 1),
    relation("df2_from_n", "n", "df2", function(vals, variant) sum(vals$n) - length(vals$n)),
    relation("f_from_descriptives", from, "f",
             function(vals, variant) oneway_f(vals$n, vals$m, vals$sd),
             range = oneway_f_range),
    relation_p_from_statistic("F"),
    relation("eta2_from_f", c("f", "df1", "df2"), "eta2", function(vals, variant) {
      explained <- vals$f * vals$df1
      switch(variant,
             eta2 = explained / (explained + vals$df2),
             omega2 = vals$df1 * (vals$f - 1) / (explained + vals$df2 + 1))
    }, variants = c("eta2", "omega2"), variant_kind = "convention", falsifies = FALSE),
    # R squared of a one-way ANOVA is eta squared by definition (SSb / SST), so as the
    # named quantity r2 it can falsify (ADR 0031); omega squared stays a convention.
    relation("r2_from_f", c("f", "df1", "df2"), "r2", function(vals, variant) {
      explained <- vals$f * vals$df1
      explained / (explained + vals$df2)
    }),
    relation("omega2_from_f", c("f", "df1", "df2"), "omega2", function(vals, variant) {
      vals$df1 * (vals$f - 1) / (vals$f * vals$df1 + vals$df2 + 1)
    }, falsifies = FALSE)
  )
}

# eta squared (= R squared of the one-way model) and omega squared from the stored statistics.
oneway_effect_sizes <- function(s) {
  r <- source_reproduce(s)
  explained <- r[["f"]] * r[["df1"]]
  c(eta2 = explained / (explained + r[["df2"]]),
    omega2 = r[["df1"]] * (r[["f"]] - 1) / (explained + r[["df2"]] + 1))
}

anova_oneway_summarise <- function(x, data, test, ...) {
  mf <- stats::model.frame(x, data)
  if (ncol(mf) != 2L) abort_input("The formula must be `outcome ~ group`.")
  y <- mf[[1L]]
  g <- factor(mf[[2L]])
  if (nlevels(g) < 2L) {
    abort_input("The grouping variable must have at least 2 groups, not ", nlevels(g), ".")
  }
  n <- c(table(g))
  if (any(n < 2L)) {
    abort_input(
      "Every group needs at least 2 observations (its SD must be defined); group ",
      paste0("'", names(n)[n < 2L], "'", collapse = ", "), " has fewer."
    )
  }
  list(
    stats = list(
      n = n,
      mean = tapply(y, g, mean),
      sd = tapply(y, g, stats::sd)
    ),
    meta = list(
      n = length(y),
      test = test,
      groups = levels(g)
    )
  )
}

anova_oneway_reproduce <- function(s) {
  n <- unname(s$stats$n)
  m <- unname(s$stats$mean)
  sd <- unname(s$stats$sd)
  k <- length(n)
  ssb <- oneway_ssb(n, m)
  ssw <- oneway_ssw(n, sd)
  df1 <- k - 1
  df2 <- sum(n) - k
  f <- (ssb / df1) / (ssw / df2)
  c(f = f, df1 = df1, df2 = df2, p = stats::pf(f, df1, df2, lower.tail = FALSE),
    eta2 = ssb / (ssb + ssw))
}

anova_oneway_reported <- function(x, data, test, ...) {
  r <- stats::oneway.test(x, data = data, var.equal = TRUE)
  sums <- summary(stats::aov(x, data = data))[[1L]][["Sum Sq"]]
  c(f = unname(r$statistic), df1 = unname(r$parameter[1]), df2 = unname(r$parameter[2]),
    p = r$p.value, eta2 = sums[1] / sum(sums))
}
