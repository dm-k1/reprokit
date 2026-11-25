# Detection check for check_reported(): does it flag a paper with ONE planted error?
# Claim: check_reported() never errors on a paper with one planted error, flags it
# inconsistent whenever the error is mathematically detectable from the reported numbers, and
# never flags one that is not. "Detectable" is decided by an oracle written independently of
# the package (below), not by looking at what check_reported() does. A planted change of +-1
# or +-2 in the last printed digit is often genuinely undetectable and is reported, not
# required.
# Papers: the correct-paper generators of check-designs.R (the ones the false-positive gate
# uses: real t.test() / cor.test() output, rounded as printed). One error per paper, the type
# rotating through the handler's types. Error types:
#   t_digit     last digit of t off by 1 to 9 units, sign kept (small = 1-2, large = 3-9)
#   p_digit     one decimal digit of p replaced; p stays in (0, 1) and in the printed format
#               (small = last digit, large = an earlier one); not drawn for "< .001"
#   df_wrong_n  df from the wrong n: n (paired, one-sample), n1 + n2 (independent), n - 1 (r);
#               size is the paper's n (n<=10 or n>10): a df off by one hardly moves p when n
#               is large, so the t/df/p detection rate depends on it
#   m_change    one M (so the mean difference) off by 5 to 20 units of its last printed digit
#               (small = 5-9, large = 10-20); t-tests only
#   decision    p replaced by one across .05, t and df unchanged
#   r_digit     one decimal digit of r replaced (small = last, large = earlier); Pearson only
#   ci_change   one CI bound off by 5 to 20 units of its last printed digit (small = 5-9, large
#               = 10-20); the papers print the CI at 90%, 95% or 99% (a quarter, a half, a
#               quarter), so a CI relation that assumed 95% would flag the correct papers
# One-way ANOVA, group anova_oneway: papers from oneway_paper() (k = 2..6, balanced
# or not, eta2 or omega2 printed). Types f_digit (last digit of F off by 1-9 units; small = 1-2),
# p_digit, df_wrong_n (df2 = N or N - k + 1 instead of N - k), decision, m_change (one group M
# off by 5-20 units). Subsets: all, f/df/p, desc+f+p (n, M, SD, F, p; df is derived from n).
# Oracle (independent of the package): the F implied by the descriptives is taken over the box
# of M and SD (largest F: means at the corners of their box, SDs at their lower ends; smallest F:
# SDs at their upper ends and SSb minimised over a shared point with stats::optimize()), the p
# implied by F over its interval (a grid of 101 values) with df1 = k - 1, df2 = N - k; f/df/p uses
# the printed df. Detectable iff every path misses, as for the t papers.
# An error is planted only in a number that is in the reported subset; d is never planted
# (its relation cannot falsify). Subsets: all (every number the paper prints), t/df/p, and
# desc+t+p (n, M, SD, for a paired t also r, for Pearson n and r; plus t and p; no df),
# desc+ci (the same without t and p, plus both CI bounds), and for
# Pearson r/df/p (the APA form r(df), p: n is not reported and is implied by df).
# Outcome per case: flagged = inconsistent; unevaluable = not flagged and the planted number
# has status not checkable / unconfirmed / not reproducible; missed = called consistent.
# Oracles (base R, no check_reported code):
#   t/df/p      detectable iff, for every p tail (two-sided, less, greater), the p implied by
#               every t and df in their rounding intervals misses the rounding interval of p
#               (a grid of 101 t values x 11 df values; "< .001" is (0, .001)).
#   r/df/p      the same grid with t = r sqrt(df) / sqrt(1 - r^2) over r in its rounding interval
#               (101 values) and df exact (n = df + 2 is implied, so df_t = df).
#   desc+t+p    for t_digit, m_change and r_digit: per test variant (one for paired, one-sample
#               and Pearson; Student and Welch for an independent t, since df is not reported)
#               two paths, t from n, M, SD (r) over their rounding intervals, and p from t and
#               df (n - 1, n - 2 for r, n1 + n2 - 2 for Student, the Welch formula over the SD
#               intervals). check_reported() judges relations one at a time, so a path is
#               detectable when every variant misses on it, and the paper when either path is.
#               For an independent t the df range of both tests is hulled before p is judged
#               (R/check.R, "no variant is ruled out"), so a one-tailed p can be explained by a
#               df between Student's and Welch's: the oracle does the same.
#               "joint-only" = no path alone, but every variant fails on one of its paths
#               (needs the variants tied across relations): reported, not counted as a miss.
# Chi-squared (ADR 0016), a second section at the end with its own PASS line: papers from
# chisq_independence_paper() and chisq_gof_paper() (check-designs.R), one planted error each,
# the type rotating: count_change (one count +-1..3, the other numbers as printed for the
# original table), chisq_digit (last digits of chi-squared off by 1-9 or 10-60 units of .01),
# p_digit, df_wrong (rc - 1 for independence, k for goodness of fit) and, 2 x 2 only,
# yates_pearson (a Yates chi-squared with the p of the Pearson chi-squared). Subsets: n/chisq/
# df/p, chisq/df/p (the shape is known from `dims`) and n/chisq/p (df derived from the shape).
# Oracle (base R, independent of the package): the planted paper is consistent iff one test
# variant (Pearson or Yates in a 2 x 2) puts the table's chi-squared inside the printed
# chi-squared interval (+-.005), the table's p inside the printed p interval (+-.0005, "< .001"
# is (0, .001); independence only, goodness of fit goes from the table to chi-squared and from
# chi-squared to p), the printed df equal to the shape's, and the p implied by the chi-squared
# interval and that df overlaps the printed p interval. Criteria: no error; oracle-detectable
# and not flagged = 0; oracle-undetectable and flagged = 0; every type x subset flags something.
# PASS criterion (all of):
#   1. errors == 0 (check_reported() never throws on a paper with a planted error);
#   2. every cell the oracle covers: oracle-detectable and not flagged = 0, and
#      oracle-undetectable and flagged = 0 (a flag there would be a false accusation);
#   3. a case flagged on t/df/p, r/df/p or desc+t+p is also flagged on all (more numbers never
#      hide an error), except Welch papers printing df with decimals: the printed df interval
#      (+-0.05) is wider than the df derived from n and SD, so reporting df can loosen p_from_t.
#      That one is a known design limitation, counted and printed, not a criterion; likewise a
#      Pearson paper flagged on r/df/p only (the printed t is wider than the t derived from r);
#   4. every handler x error type x subset cell flags more than 0 cases.
# Not a criterion, printed for information: the rate at which large errors are found where
# nothing hides them (decision errors on every subset, df_wrong_n on all, M changed by 10 or more
# units on all), lowest per handler. It was a >= 90% criterion until the papers printed 90/95/99%
# CIs: with r unknown and three levels to choose from, a shifted M can still fit, so
# some large paired m_change errors became undetectable and the rate fell below 90%. The oracle
# agrees those cases are undetectable, so criterion 2 (oracle-detectable not flagged = 0) is the
# hard test; a raw rate cannot tell a regression from a genuinely undetectable error.
# Seed 20261124. Size: 1000 papers per handler group, 6 groups. Runtime about 4 minutes
# (6 cores via parallel::mclapply).
# Factorial ANOVA (ADR 0017), a third section at the end with its own PASS line: papers from
# factorial_paper() (check-designs.R: 2..3 x 2..4 cells, balanced or not, effect A, B or AB, SS
# type III, II or I, partial eta2 or omega2 printed), one planted error each, the type rotating:
# cell_mean_change (one cell M off by 5-20 units of its last digit), f_digit (last digit of F off
# by 1-9 units of .01), p_digit, df2_wrong (N - 1 or N - a - b + 1 instead of N - a b) and
# type_mix (F of type III with the p of type II, unbalanced designs where the two differ by more
# than 10% in F). Subsets: all, f/df/p, desc+f+p (n, M, SD, F, p; df is derived from the cells).
# Oracle (base R, independent of the package) for f/df/p, where the claim is exact: the planted
# paper is detectable iff the p implied by F over its rounding interval (101 values) with the
# printed df misses the p rounding interval. Criteria: no error; oracle-detectable not flagged =
# 0; oracle-undetectable flagged = 0; a case flagged on f/df/p is flagged on all; every type x
# subset cell that contains the planted number flags something.
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-planted-errors.R")'

options(width = 200)
n_papers <- 1000L
set.seed(20261124)
t_start <- Sys.time()

source("inst/validation/check-designs.R")

cases <- lapply(planted_groups, draw_cases, n_papers = n_papers)

# Oracles ---------------------------------------------------------------------------------
# A printed number as c(lo, hi): its rounding interval. exact = TRUE for integers (n, df).
interval <- function(s, exact = FALSE) {
  if (grepl("<", s, fixed = TRUE)) return(c(0, 0.001))
  v <- as.numeric(s)
  h <- if (exact && n_decimals(s) == 0L) 0 else 0.5 * 10^-n_decimals(s) # nolint: object_usage_linter
  c(v - h, v + h)
}
overlap <- function(range, target) {
  if (range[1L] > target[2L] + 1e-7 || range[2L] < target[1L] - 1e-7) return("miss")
  if (range[1L] <= target[2L] - 1e-7 && range[2L] >= target[1L] + 1e-7) return("fit")
  "edge"
}
# "detectable", "undetectable" or "edge" (a boundary case, left out of the criteria).
verdict <- function(parts) {
  if ("fit" %in% parts) return("undetectable")
  if (all(parts == "miss")) "detectable" else "edge"
}
oracle_tdfp <- function(paper) {
  ti <- interval(paper$t)
  di <- interval(paper$df, exact = TRUE)
  pi <- pmin(pmax(interval(paper$p), 0), 1)
  tg <- seq(ti[1L], ti[2L], length.out = 101L)
  dg <- if (di[1L] == di[2L]) di[1L] else seq(di[1L], di[2L], length.out = 11L)
  grid <- expand.grid(t = tg, df = dg[dg > 0])
  tails <- list(two = 2 * pt(-abs(grid$t), grid$df), less = pt(grid$t, grid$df),
                greater = pt(grid$t, grid$df, lower.tail = FALSE))
  verdict(vapply(tails, function(p) overlap(range(p), pi), ""))
}
oracle_rdfp <- function(paper) {
  ri <- pmin(pmax(interval(paper$r), -1), 1)
  di <- interval(paper$df, exact = TRUE)
  pi <- pmin(pmax(interval(paper$p), 0), 1)
  rg <- seq(ri[1L], ri[2L], length.out = 101L)
  dg <- if (di[1L] == di[2L]) di[1L] else seq(di[1L], di[2L], length.out = 11L)
  grid <- expand.grid(r = rg, df = dg[dg > 0])
  t <- grid$r * sqrt(grid$df) / sqrt(1 - grid$r^2)
  if (!all(is.finite(t))) return("edge") # |r| = 1: not evaluable, left out
  tails <- list(two = 2 * pt(-abs(t), grid$df), less = pt(t, grid$df),
                greater = pt(t, grid$df, lower.tail = FALSE))
  verdict(vapply(tails, function(p) overlap(range(p), pi), ""))
}
oracle_desc <- function(handler, paper) {
  ti <- interval(paper$t)
  pi <- pmin(pmax(interval(paper$p), 0), 1)
  five <- function(s, lower = -Inf, upper = Inf) {
    i <- pmin(pmax(interval(s), lower), upper)
    seq(i[1L], i[2L], length.out = 5L)
  }
  # A non-finite t anywhere in the box means "not evaluable": check_reported() must stay quiet.
  span <- function(x) if (all(is.finite(x))) range(x) else c(-Inf, Inf)
  n <- as.numeric(paper$n)
  tg <- seq(ti[1L], ti[2L], length.out = 101L)
  # p implied by t over its interval and a set of df values, in every tail
  p_parts <- function(df) {
    g <- expand.grid(t = tg, df = df[is.finite(df) & df > 0])
    if (!nrow(g)) return("fit")
    tails <- list(2 * pt(-abs(g$t), g$df), pt(g$t, g$df), pt(g$t, g$df, lower.tail = FALSE))
    vapply(tails, function(p) overlap(range(p), pi), "")
  }
  # Each test variant has two paths: t from the descriptives (desc_t) and p from t and df.
  # check_reported() judges each relation on its own, a number fitting if ANY variant fits it,
  # so the matching oracle is per path: a path is detectable when every variant misses on it,
  # and the paper is detectable when either path is (undetectable when both paths have a
  # variant that fits, edge otherwise). "joint-only" marks papers where no path alone is
  # detectable but every single variant fails on one of its two paths (Student's t fits the
  # descriptives, Welch's df fits p, and neither variant fits both): a limitation of judging
  # relations one at a time, and not counted against check_reported().
  variant <- function(desc_t, df) {
    list(desc = verdict(overlap(span(desc_t), ti)), p = if (!is.null(df)) verdict(p_parts(df)))
  }
  # nolint start: indentation_linter, line_length_linter
  variants <- switch(handler,
    cor_pearson = {
      r <- five(paper$r, -1, 1)
      list(variant(r * sqrt(n - 2) / sqrt(1 - r^2), n - 2))
    },
    t_one_sample = {
      g <- expand.grid(m = five(paper$m), s = five(paper$sd, 0))
      list(variant((g$m - attr(paper, "mu")) / (g$s / sqrt(n)), n - 1))
    },
    t_paired = {
      g <- expand.grid(m1 = five(paper$m[1L]), m2 = five(paper$m[2L]), s1 = five(paper$sd[1L], 0),
                       s2 = five(paper$sd[2L], 0), r = five(paper$r, -1, 1))
      list(variant((g$m1 - g$m2) /
                     (sqrt(pmax(g$s1^2 + g$s2^2 - 2 * g$r * g$s1 * g$s2, 0)) / sqrt(n)), n - 1))
    },
    t_independent = {
      g <- expand.grid(m1 = five(paper$m[1L]), m2 = five(paper$m[2L]), s1 = five(paper$sd[1L], 0),
                       s2 = five(paper$sd[2L], 0))
      pooled <- sqrt(((n[1L] - 1) * g$s1^2 + (n[2L] - 1) * g$s2^2) / (sum(n) - 2))
      v1 <- g$s1^2 / n[1L]
      v2 <- g$s2^2 / n[2L]
      welch_df <- (v1 + v2)^2 / (v1^2 / (n[1L] - 1) + v2^2 / (n[2L] - 1))
      # df is not reported: check_reported() carries the widest df range over both tests
      # into p, so p is judged against that hull.
      all_df <- c(sum(n) - 2, welch_df)
      hull <- if (all(is.finite(all_df))) seq(min(all_df), max(all_df), length.out = 11L) else NA_real_
      list(variant((g$m1 - g$m2) / (pooled * sqrt(1 / n[1L] + 1 / n[2L])), hull),
           variant((g$m1 - g$m2) / sqrt(v1 + v2), hull))
    })
  # nolint end
  path <- function(which) {
    parts <- vapply(variants, function(v) v[[which]], "")
    if ("undetectable" %in% parts) return("undetectable")
    if (all(parts == "detectable")) "detectable" else "edge"
  }
  paths <- c(path("desc"), path("p"))
  if ("detectable" %in% paths) return("detectable")
  if (all(paths == "undetectable")) {
    joint <- vapply(variants, function(v) "detectable" %in% c(v$desc, v$p), NA)
    return(if (all(joint)) "joint-only" else "undetectable")
  }
  "edge"
}

oracle_oneway <- function(paper, subset) {
  fi <- interval(paper$f)
  pi <- pmin(pmax(interval(paper$p), 0), 1)
  n <- as.numeric(paper$n)
  k <- length(n)
  df1 <- if (subset == "f/df/p") as.numeric(paper$df1) else k - 1
  df2 <- if (subset == "f/df/p") as.numeric(paper$df2) else sum(n) - k
  fg <- seq(max(fi[1L], 0), fi[2L], length.out = 101L)
  paths <- verdict(overlap(range(pf(fg, df1, df2, lower.tail = FALSE)), pi))
  if (subset == "desc+f+p") {
    m <- lapply(paper$m, interval)
    lo <- vapply(m, `[`, 0, 1L)
    hi <- vapply(m, `[`, 0, 2L)
    s <- lapply(paper$sd, interval)
    slo <- pmax(vapply(s, `[`, 0, 1L), 1e-9)
    shi <- vapply(s, `[`, 0, 2L)
    ssb <- function(x) {
      sum(n * (x - sum(n * x) / sum(n))^2)
    }
    corners <- as.matrix(expand.grid(Map(c, lo, hi)))
    f_max <- max(apply(corners, 1L, ssb)) / (k - 1) / (sum((n - 1) * slo^2) / (sum(n) - k))
    g <- function(c) sum(n * pmax(lo - c, 0, c - hi)^2)
    f_min <- optimize(g, range(c(lo, hi)), tol = 1e-12)$objective / (k - 1) /
      (sum((n - 1) * shi^2) / (sum(n) - k))
    paths <- c(paths, verdict(overlap(c(f_min, f_max), fi)))
  }
  if ("detectable" %in% paths) return("detectable")
  if (all(paths == "undetectable")) "undetectable" else "edge"
}

# Jobs: one per planted paper x subset that contains the planted number ---------------------
jobs <- do.call(c, lapply(names(cases), function(g) {
  handler <- planted_groups[[g]]$handler
  do.call(c, lapply(seq_along(cases[[g]]), function(i) {
    case <- cases[[g]][[i]]
    subsets <- report_subsets(handler, case$planted$paper)
    q <- case$planted$quantity
    applicable <- Filter(function(keys) q %in% keys, subsets)
    lapply(names(applicable), function(s) {
      list(case = paste(g, i), group = g, handler = handler, type = case$type,
           size = case$planted$size, subset = s, quantity = q, paper = case$planted$paper,
           keys = applicable[[s]])
    })
  }))
}))

run_job <- function(job) {
  res <- run_check(job$handler, job$paper, job$keys) # nolint: object_usage_linter
  if (inherits(res, "error")) return("error")
  if (!res$consistent) return("flagged")
  status <- res$numbers$status[res$numbers$quantity == job$quantity]
  if (status %in% c("not checkable", "unconfirmed", "not reproducible")) "unevaluable" else "missed"
}
oracle_job <- function(job) {
  if (job$subset == "t/df/p") return(oracle_tdfp(job$paper))
  if (job$subset == "r/df/p") return(oracle_rdfp(job$paper))
  if (job$subset == "desc+t+p" && job$type %in% c("t_digit", "m_change", "r_digit")) {
    return(oracle_desc(job$handler, job$paper))
  }
  if (job$handler == "anova_oneway" && job$subset == "f/df/p") {
    return(oracle_oneway(job$paper, job$subset))
  }
  if (job$handler == "anova_oneway" && job$subset == "desc+f+p" &&
        job$type %in% c("f_digit", "m_change")) {
    return(oracle_oneway(job$paper, job$subset))
  }
  NA_character_
}
outcome <- unlist(parallel::mclapply(jobs, run_job, mc.cores = 6L))
oracle <- unlist(parallel::mclapply(jobs, oracle_job, mc.cores = 6L))
# nolint start: indentation_linter
results <- data.frame(
  case = vapply(jobs, `[[`, "", "case"), group = vapply(jobs, `[[`, "", "group"),
  type = vapply(jobs, `[[`, "", "type"), size = vapply(jobs, `[[`, "", "size"),
  subset = vapply(jobs, `[[`, "", "subset"), outcome = outcome, oracle = oracle,
  df_decimals = vapply(jobs, function(j) {
    if (is.null(j$paper$df)) 0L else n_decimals(j$paper$df)
  }, 0L),
  level = vapply(jobs, function(j) {
    level <- attr(j$paper, "level")
    if (is.null(level)) NA_real_ else level
  }, 0),
  stringsAsFactors = FALSE)
# nolint end

summarise_cells <- function(data, by) {
  keys <- do.call(paste, c(data[by], sep = "\r"))
  rows <- lapply(split(seq_len(nrow(data)), keys), function(idx) {
    o <- data$outcome[idx]
    cbind(data[idx[1L], by, drop = FALSE],
          cases = length(idx), `% flagged` = round(100 * mean(o == "flagged"), 1),
          `% unevaluable` = round(100 * mean(o == "unevaluable"), 1),
          `% missed` = round(100 * mean(o == "missed"), 1))
  })
  out <- do.call(rbind, rows)
  out <- out[do.call(order, out[by]), ]
  rownames(out) <- NULL
  out
}

cat("Detection by handler x error type x reported subset\n")
print(summarise_cells(results, c("group", "type", "subset")), row.names = FALSE)
cat("\nDetection by size of the error (digit changes, M, df; decision errors are all large)\n")
print(summarise_cells(results[results$type != "decision", ], c("group", "type", "size", "subset")),
      row.names = FALSE)

cat("\nCI bound changed by 5 to 20 units (ci_change), by confidence level of the paper\n")
print(summarise_cells(results[results$type == "ci_change", ],
                      c("group", "subset", "level")), row.names = FALSE)
ci_rows <- results[results$type == "ci_change" & results$subset == "desc+ci", ]
ci_rate <- function(group, level = NULL) {
  d <- ci_rows[ci_rows$group == group, ]
  if (!is.null(level)) d <- d[d$level == level, ]
  100 * mean(d$outcome == "flagged")
}
cat("CI-only detection (desc+ci subset), independent t, % flagged: Student all levels",
    round(ci_rate("t_independent_student"), 1), ", Student 95%",
    round(ci_rate("t_independent_student", 0.95), 1), ", Welch all levels",
    round(ci_rate("t_independent_welch"), 1), ", Welch 95%",
    round(ci_rate("t_independent_welch", 0.95), 1), "\n")

covered <- results[!is.na(results$oracle) & !results$oracle %in% c("edge", "joint-only"), ]
cat("\nOracle check (t/df/p and r/df/p: every type; desc+t+p: t_digit, m_change, r_digit)\n")
oracle_table <- as.data.frame(table(oracle = covered$oracle, outcome = covered$outcome))
print(oracle_table[oracle_table$Freq > 0, ], row.names = FALSE)
cat("edge cases left out:", sum(!is.na(results$oracle) & results$oracle == "edge"), "\n")
joint_only <- results[!is.na(results$oracle) & results$oracle == "joint-only", ]
cat("joint-only (detectable only by tying the Student/Welch variant across relations;",
    "limitation, not a criterion):\n")
print(table(outcome = joint_only$outcome))

# Criteria ----------------------------------------------------------------------------------
errors <- sum(results$outcome == "error")
detectable_not_flagged <- sum(covered$oracle == "detectable" & covered$outcome != "flagged")
false_flags <- sum(covered$oracle == "undetectable" & covered$outcome == "flagged")
flagged_cases <- function(s) {
  unique(results$case[results$subset == s & results$outcome == "flagged"])
}
all_flagged <- flagged_cases("all")
rdfp_hidden <- setdiff(flagged_cases("r/df/p"), all_flagged)
hidden_cases <- c(setdiff(flagged_cases("t/df/p"), all_flagged),
                  setdiff(flagged_cases("desc+t+p"), all_flagged))
# A Welch paper that prints df with decimals (1 or 2) gives p_from_t a df interval of +-0.05 or
# +-0.005, wider than the df check_reported() derives from n and SD when df is not printed, so
# adding df can loosen a flag: reported, not a criterion (a design limitation, see the header).
is_welch <- results$group == "t_independent_welch"
df_rounded <- unique(results$case[is_welch & results$df_decimals > 0L])
hidden_by_df_rounding <- sum(hidden_cases %in% df_rounded)
cat("\nflagged on t/df/p or desc+t+p but not on all:", length(hidden_cases), "of which Welch df",
    "printed with decimals (wider df interval than the one derived from n, SD):",
    hidden_by_df_rounding, "\n")
# Likewise a Pearson paper flagged on r/df/p only: the t derived from r and df (r over its
# rounding interval) is narrower than the printed t (+-0.005), and p_from_t takes the printed t
# when it is reported, so printing t can loosen a flag. The r/df/p oracle confirms each such
# paper is detectable; a design limitation, counted and printed, not a criterion.
cat("flagged on r/df/p but not on all (printed t is wider than the t derived from r):",
    length(rdfp_hidden), "\n")
hidden_by_more <- length(hidden_cases) - hidden_by_df_rounding
cells <- summarise_cells(results, c("group", "type", "subset"))
dead_cells <- sum(cells$`% flagged` == 0)
rate <- function(type, subset, size = NULL) {
  d <- results[results$type == type & results$subset == subset, ]
  if (!is.null(size)) d <- d[d$size %in% size, ]
  100 * mean(d$outcome == "flagged")
}
large_rates <- c(decision_all = rate("decision", "all"),
                 decision_tdfp = rate("decision", "t/df/p"),
                 decision_desc = rate("decision", "desc+t+p"),
                 decision_rdfp = rate("decision", "r/df/p"),
                 df_wrong_n_all = rate("df_wrong_n", "all"),
                 m_large_all = rate("m_change", "all", "large"))
by_group_min <- min(unlist(lapply(unique(results$group), function(g) {
  d <- results[results$group == g, ]
  c(mean(d$outcome[d$type == "decision" & d$subset == "all"] == "flagged"),
    mean(d$outcome[d$type == "df_wrong_n" & d$subset == "all"] == "flagged"),
    if (any(d$type == "m_change")) {
      mean(d$outcome[d$type == "m_change" & d$subset == "all" & d$size == "large"] == "flagged")
    })
})) * 100)
pooled <- paste(names(large_rates), round(large_rates, 1), sep = " = ", collapse = ", ")
cat("\npooled large-error rates (%):", pooled, "\n")
cat("informational: lowest handler rate, decision/df_wrong_n/m_change-large on all (%):",
    round(by_group_min, 1), "\n")
cat("planted papers:", sum(lengths(cases)), " checks:", nrow(results), " runtime (min):",
    round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "\n")
ok <- errors == 0 && detectable_not_flagged == 0 && false_flags == 0 && hidden_by_more == 0 &&
  dead_cells == 0
cat(if (ok) "PASS" else "FAIL", ": errors = ", errors, ", oracle-detectable not flagged = ",
    detectable_not_flagged, ", oracle-undetectable flagged = ", false_flags,
    ", flagged on a subset but not on all = ", hidden_by_more, ", cells never flagged = ",
    dead_cells, " (lowest large-error rate, informational = ", round(by_group_min, 1), "%)\n",
    sep = "")

# Chi-squared planted errors -------------------------------------------------------------------
set.seed(20261124)
chisq_types <- list(chisq_independence = c("count_change", "chisq_digit", "p_digit", "df_wrong",
                                           "yates_pearson"),
                    chisq_gof = c("count_change", "chisq_digit", "p_digit", "df_wrong"))
chisq_interval <- function(text, dec) {
  if (grepl("<", text, fixed = TRUE)) return(c(0, 0.001))
  x <- as.numeric(text)
  c(max(x - 0.5 * 10^-dec, 0), x + 0.5 * 10^-dec)
}
chisq_stat_of <- function(n, dims, yates) {
  if (length(dims) == 1L) {
    return(sum((n - sum(n) / dims)^2 / (sum(n) / dims)))
  }
  suppressWarnings(unname(chisq.test(matrix(n, dims[1L]), correct = yates)$statistic))
}
chisq_oracle <- function(paper, keys) {
  dims <- attr(paper, "dims")
  p0 <- attr(paper, "p0")
  n <- as.numeric(paper$n)
  true_df <- if (length(dims) == 1L) dims - 1 else prod(dims - 1)
  variants <- if (length(dims) == 2L && all(dims == 2L)) c(FALSE, TRUE) else FALSE
  has <- function(k) k %in% keys
  ci <- if (has("chisq")) chisq_interval(paper$chisq, 2L)
  pi <- if (has("p")) chisq_interval(paper$p, 3L)
  df <- if (has("df")) as.numeric(paper$df) else true_df
  fits <- vapply(variants, function(y) {
    stat <- function() {
      if (length(dims) == 1L) {
        e <- sum(n) * (if (is.null(p0)) rep(1 / dims, dims) else p0)
        sum((n - e)^2 / e)
      } else chisq_stat_of(n, dims, y)
    }
    tab <- if (has("n")) stat()
    ok <- TRUE
    if (has("n") && has("chisq")) ok <- ok && tab >= ci[1L] - 1e-12 && tab <= ci[2L] + 1e-12
    # Goodness of fit has no table-to-p path (only chi-squared to p), as in the handler.
    if (has("n") && has("p") && length(dims) == 2L) {
      ptab <- pchisq(tab, true_df, lower.tail = FALSE)
      ok <- ok && ptab >= pi[1L] - 1e-12 && ptab <= pi[2L] + 1e-12
    }
    if (has("df")) ok <- ok && df == true_df
    if (has("chisq") && has("p")) {
      pr <- pchisq(rev(ci), df, lower.tail = FALSE)
      ok <- ok && pr[1L] <= pi[2L] + 1e-12 && pr[2L] >= pi[1L] - 1e-12
    }
    ok
  }, logical(1))
  !any(fits)
}
chisq_plant <- function(paper, handler, type) {
  dims <- attr(paper, "dims")
  p <- paper
  size <- "small"
  if (type == "count_change") {
    i <- sample(length(p$n), 1L)
    delta <- sample(1:3, 1L) * (if (as.numeric(p$n[i]) > 3 && runif(1L) < 0.5) -1 else 1)
    p$n[i] <- as.character(as.numeric(p$n[i]) + delta)
    p$n_total <- NULL
  } else if (type == "chisq_digit") {
    size <- if (runif(1L) < 0.5) "small" else "large"
    units <- if (size == "small") sample(1:9, 1L) else sample(10:60, 1L)
    new <- as.numeric(p$chisq) + sample(c(-1, 1), 1L) * units * 0.01
    if (new <= 0) return(NULL)
    p$chisq <- fmt(new, 2L)
  } else if (type == "p_digit") {
    if (grepl("<", p$p, fixed = TRUE)) return(NULL)
    pos <- sample(3:5, 1L)
    chars <- strsplit(p$p, "")[[1L]]
    chars[pos] <- sample(setdiff(as.character(0:9), chars[pos]), 1L)
    p$p <- paste(chars, collapse = "")
    size <- if (pos == 5L) "small" else "large"
  } else if (type == "df_wrong") {
    p$df <- as.character(if (length(dims) == 1L) dims else prod(dims) - 1L)
    size <- "large"
  } else if (type == "yates_pearson") {
    if (length(dims) != 2L || !all(dims == 2L) || !isTRUE(attr(paper, "truth")$yates)) return(NULL)
    pearson <- chisq_stat_of(as.numeric(p$n), dims, FALSE)
    p$p <- fmt_p(pchisq(pearson, 1, lower.tail = FALSE))
  }
  if (identical(p[names(p)], paper[names(paper)])) return(NULL)
  attributes(p) <- c(attributes(p)[c("names")], attributes(paper)[c("handler", "dims", "p0", "truth")])
  p
}
chisq_subsets_planted <- list(`n/chisq/df/p` = c("n", "chisq", "df", "p"),
                              `chisq/df/p` = c("chisq", "df", "p"),
                              `n/chisq/p` = c("n", "chisq", "p"))
chisq_rows <- list()
for (handler in names(chisq_types)) {
  make <- if (handler == "chisq_gof") chisq_gof_paper else chisq_independence_paper
  for (i in seq_len(500L)) {
    type <- chisq_types[[handler]][(i - 1L) %% length(chisq_types[[handler]]) + 1L]
    for (attempt in 1:200) {
      paper <- make()
      planted <- if (is.null(paper)) NULL else chisq_plant(paper, handler, type)
      if (!is.null(planted)) break
    }
    if (is.null(planted)) next
    for (sub in names(chisq_subsets_planted)) {
      keys <- chisq_subsets_planted[[sub]]
      if (!all(keys %in% names(planted))) next
      # The planted number must be in the subset (a changed count needs n, a df needs df).
      if ((type == "count_change" && !"n" %in% keys) ||
            (type == "df_wrong" && !"df" %in% keys)) next
      res <- run_chisq_check(planted, keys)
      outcome <- if (inherits(res, "error")) "error" else if (!res$consistent) "flagged" else "missed"
      chisq_rows[[length(chisq_rows) + 1L]] <- data.frame(
        handler = handler, type = type, subset = sub, outcome = outcome,
        oracle = if (chisq_oracle(planted, keys)) "detectable" else "undetectable")
    }
  }
}
chisq_results <- do.call(rbind, chisq_rows)
chisq_cells <- aggregate(outcome ~ handler + type + subset, chisq_results,
                         function(x) c(n = length(x), flagged = sum(x == "flagged")))
chisq_cells <- cbind(chisq_cells[1:3], as.data.frame(chisq_cells$outcome))
chisq_cells$`% flagged` <- round(100 * chisq_cells$flagged / chisq_cells$n, 1)
cat("\n== Chi-squared planted errors: detection by handler x type x subset ==\n")
print(chisq_cells, row.names = FALSE)
cat("\nOracle check\n")
print(as.data.frame(table(oracle = chisq_results$oracle, outcome = chisq_results$outcome)),
      row.names = FALSE)
chisq_errors <- sum(chisq_results$outcome == "error")
chisq_missed <- sum(chisq_results$oracle == "detectable" & chisq_results$outcome != "flagged")
chisq_false <- sum(chisq_results$oracle == "undetectable" & chisq_results$outcome == "flagged")
chisq_dead <- sum(chisq_cells$flagged == 0)
chisq_ok <- chisq_errors == 0 && chisq_missed == 0 && chisq_false == 0 && chisq_dead == 0
cat(if (chisq_ok) "\nPASS" else "\nFAIL", ": chi-squared planted errors, errors = ", chisq_errors,
    ", oracle-detectable not flagged = ", chisq_missed, ", oracle-undetectable flagged = ",
    chisq_false, ", cells never flagged = ", chisq_dead, "\n", sep = "")

# Factorial ANOVA planted errors ---------------------------------------------------------------
set.seed(20261124)
fact_types <- c("cell_mean_change", "f_digit", "p_digit", "df2_wrong", "type_mix")
fact_subsets <- function(paper) {
  list(all = names(paper), `f/df/p` = c("f", "df1", "df2", "p"),
       `desc+f+p` = c("n", "m", "sd", "f", "p"))
}
fact_plant <- function(paper, type) {
  dims <- attr(paper, "dims")
  switch(type,
    cell_mean_change = {
      i <- sample(length(paper$m), 1L)
      units <- sample(5:20, 1L) * sample(c(-1, 1), 1L)
      paper$m[i] <- fmt(as.numeric(paper$m[i]) + units * 0.01, 2L)
      list(paper = paper, quantity = "m")
    },
    f_digit = {
      f0 <- as.numeric(paper$f)
      step <- sample(c(-1, 1), 1L) * sample(1:9, 1L) / 100
      if (f0 + step <= 0) step <- abs(step)
      paper$f <- fmt(f0 + step, 2L)
      list(paper = paper, quantity = "f")
    },
    p_digit = {
      if (is.na(p_value(paper$p))) return(NULL)
      changed <- change_digit(paper$p)
      if (is.null(changed)) return(NULL)
      paper$p <- changed$value
      list(paper = paper, quantity = "p")
    },
    df2_wrong = {
      N <- sum(as.numeric(paper$n))
      wrong <- sample(c(N - 1, N - sum(dims) + 1), 1L)
      if (wrong == as.numeric(paper$df2)) return(NULL)
      paper$df2 <- as.character(wrong)
      list(paper = paper, quantity = "df2")
    })
}
fact_mix_paper <- function() {
  raw <- factorial_raw(balanced = FALSE)
  effect <- sample(c("A", "B", "AB"), 1L)
  paper <- factorial_paper(raw, effect, "III")
  if (is.null(paper)) return(NULL)
  t3 <- factorial_truth(raw$d, "III")
  t2 <- factorial_truth(raw$d, "II")
  if (abs(t3$f[[effect]] - t2$f[[effect]]) < 0.1 * t3$f[[effect]]) return(NULL)
  paper$p <- fmt_p(t2$p[[effect]])
  list(paper = paper, quantity = "p")
}
fact_oracle <- function(paper) {
  fi <- interval(paper$f)
  pi <- pmin(pmax(interval(paper$p), 0), 1)
  fg <- seq(max(fi[1L], 0), fi[2L], length.out = 101L)
  verdict(overlap(range(pf(fg, as.numeric(paper$df1), as.numeric(paper$df2),
                           lower.tail = FALSE)), pi))
}
fact_rows <- list()
n_fact <- 600L
for (i in seq_len(n_fact)) {
  type <- fact_types[(i - 1L) %% length(fact_types) + 1L]
  planted <- NULL
  for (attempt in 1:200) {
    if (type == "type_mix") {
      planted <- fact_mix_paper()
    } else {
      paper <- factorial_paper()
      planted <- if (is.null(paper)) NULL else fact_plant(paper, type)
    }
    if (!is.null(planted)) break
  }
  if (is.null(planted)) next
  subsets <- fact_subsets(planted$paper)
  for (sub in names(subsets)) {
    keys <- subsets[[sub]]
    if (!planted$quantity %in% keys) next
    res <- run_factorial_check(planted$paper, keys)
    outcome <- if (inherits(res, "error")) "error" else if (!res$consistent) "flagged" else "not flagged"
    fact_rows[[length(fact_rows) + 1L]] <- data.frame(
      case = i, type = type, subset = sub, outcome = outcome,
      oracle = if (sub == "f/df/p") fact_oracle(planted$paper) else NA_character_)
  }
}
fact_results <- do.call(rbind, fact_rows)
fact_cells <- aggregate(outcome ~ type + subset, fact_results,
                        function(o) c(n = length(o), flagged = sum(o == "flagged")))
fact_cells <- cbind(fact_cells[1:2], as.data.frame(fact_cells$outcome))
fact_cells$`% flagged` <- round(100 * fact_cells$flagged / fact_cells$n, 1)
cat("\n== Factorial ANOVA planted errors ==\n")
print(fact_cells, row.names = FALSE)
fcov <- fact_results[!is.na(fact_results$oracle) & fact_results$oracle != "edge", ]
print(as.data.frame(table(oracle = fcov$oracle, outcome = fcov$outcome)), row.names = FALSE)
fact_errors <- sum(fact_results$outcome == "error")
fact_missed <- sum(fcov$oracle == "detectable" & fcov$outcome != "flagged")
fact_false <- sum(fcov$oracle == "undetectable" & fcov$outcome == "flagged")
fl <- function(s) unique(fact_results$case[fact_results$subset == s & fact_results$outcome == "flagged"])
fact_hidden <- length(setdiff(fl("f/df/p"), fl("all")))
fact_dead <- sum(fact_cells$flagged == 0)
fact_ok <- fact_errors == 0 && fact_missed == 0 && fact_false == 0 && fact_hidden == 0 &&
  fact_dead == 0
cat(if (fact_ok) "\nPASS" else "\nFAIL", ": factorial planted errors, errors = ", fact_errors,
    ", oracle-detectable not flagged = ", fact_missed, ", oracle-undetectable flagged = ",
    fact_false, ", flagged on f/df/p but not on all = ", fact_hidden, ", cells never flagged = ",
    fact_dead, "\n", sep = "")

# Regression family planted errors (ADR 0020) ---------------------------------------------------
# Claim: check_lm() and check_mediation() never error on a paper with ONE planted error, flag it
# inconsistent whenever the error is detectable from the reported numbers, and never flag one that
# is not. Papers: real lm() fits (lm_raw/lm_paper of check-designs.R) and the three regressions of a
# simple mediation (mediation_raw/mediation_paper), rounded as printed. Error types:
#   b_digit, se_digit, t_digit  one term's last printed digits off by 5-20 units (oracle: t = b/SE
#                               over the rounding box, corners)
#   p_digit                     one decimal digit of one term's p replaced (oracle: p from the t box
#                               and df_resid, two-sided)
#   f_vs_r2                     F off by 5-50 per cent (oracle: F from the R2 box, df1, df2, or from
#                               R2 read as the adjusted R2: either fitting makes it undetectable)
#   df_wrong_k                  df2 = n - k or n - k - 2 (k = df1, or declared); always detectable
#                               since n, df1 and df2 are exact integers
#   med_df                      one mediation path's df_resid replaced by n - 1, n - 2 or n - 3
#                               (not its own); the four df share one n, so exact integers decide
#   med_ci                      one bound of one mediation path's CI shifted by 5-30 units of its
#                               last digit (oracle: b -/+ t_crit SE over the b and SE rounding
#                               boxes at the exact df; detectable when no level of 90/95/99 per
#                               cent has every bound of every path fitting)
# PASS: no errors; oracle-detectable always flagged; oracle-undetectable never flagged; every
# type x subset cell flags at least once; nothing flagged on the narrow subset is missed on all.
set.seed(20261125)
reg_types <- c("b_digit", "se_digit", "t_digit", "p_digit", "f_vs_r2", "df_wrong_k", "med_df",
               "med_ci")
reg_subsets <- list(
  b_digit = list(all = NULL, `b/se/t` = c("b", "se", "t")),
  se_digit = list(all = NULL, `b/se/t` = c("b", "se", "t")),
  t_digit = list(all = NULL, `b/se/t` = c("b", "se", "t")),
  p_digit = list(all = NULL, `t/df/p` = c("t", "p", "df2")),
  f_vs_r2 = list(all = NULL, `r2/f/df` = c("r2", "f", "df1", "df2")),
  df_wrong_k = list(all = NULL, `n/df1/df2` = c("n", "df1", "df2"), `n/df2 + k` = c("n", "df2")),
  med_df = list(all = NULL, `t/df/p` = c("t", "p", "df")),
  med_ci = list(all = NULL, `b/se/df/ci` = c("b", "se", "df", "ci_lower", "ci_upper")))
reg_shift <- function(s, units) {
  d <- n_decimals(s)
  fmt(as.numeric(s) + units * 10^-d, d)
}
reg_paper <- function(type) {
  if (type == "med_df") {
    d <- mediation_raw()
    paper <- mediation_paper(d)
    path <- sample(names(paper$b), 1L)
    n <- as.numeric(paper$n)
    right <- as.numeric(paper$df[[path]])
    wrong <- sample(setdiff(c(n - 1, n - 2, n - 3), right), 1L)
    paper$df[[path]] <- as.character(wrong)
    return(list(paper = paper, path = path, mediation = TRUE))
  }
  if (type == "med_ci") {
    paper <- mediation_paper(mediation_raw())
    path <- sample(names(paper$b), 1L)
    bound <- sample(c("ci_lower", "ci_upper"), 1L)
    paper[[bound]][[path]] <- reg_shift(paper[[bound]][[path]],
                                        sample(5:30, 1L) * sample(c(-1, 1), 1L))
    return(list(paper = paper, path = path, mediation = TRUE))
  }
  raw <- lm_raw()
  paper <- lm_paper(raw)
  terms <- setdiff(names(paper$b), "(Intercept)")
  term <- sample(terms, 1L)
  planted <- switch(type,
    b_digit = { paper$b[[term]] <- reg_shift(paper$b[[term]], sample(5:20, 1L) * sample(c(-1, 1), 1L)); TRUE },
    se_digit = {
      new <- reg_shift(paper$se[[term]], sample(5:20, 1L) * sample(c(-1, 1), 1L))
      ok <- as.numeric(new) > 0
      if (ok) paper$se[[term]] <- new
      ok },
    t_digit = {
      t0 <- as.numeric(paper$t[[term]])
      if (t0 == 0) return(NULL)
      paper$t[[term]] <- fmt(t0 + sample(c(-1, 1), 1L) * sample(5:90, 1L) / 100, 2L)
      TRUE },
    p_digit = {
      if (is.na(p_value(paper$p[[term]]))) return(NULL)
      ch <- change_digit(paper$p[[term]])
      if (is.null(ch)) return(NULL)
      paper$p[[term]] <- ch$value
      TRUE },
    f_vs_r2 = {
      f0 <- as.numeric(paper$f)
      paper$f <- fmt(f0 * (1 + sample(c(-1, 1), 1L) * sample(5:50, 1L) / 100), 2L)
      TRUE },
    df_wrong_k = {
      df2 <- as.numeric(paper$df2)
      paper$df2 <- as.character(sample(c(df2 + 1, df2 - 1), 1L))
      TRUE })
  if (!isTRUE(planted)) return(NULL)
  list(paper = paper, term = term, mediation = FALSE)
}
reg_oracle <- function(type, p) {
  paper <- p$paper
  if (type == "med_ci") {
    n <- as.numeric(paper$n)
    dfs <- c(a = n - 2, b = n - 3, c = n - 2, c_prime = n - 3)
    fits_level <- vapply(c(0.90, 0.95, 0.99), function(level) {
      parts <- unlist(lapply(names(dfs), function(path) {
        bi <- interval(paper$b[[path]])
        si <- interval(paper$se[[path]])
        tc <- qt(1 - (1 - level) / 2, dfs[[path]])
        c(lower = overlap(c(bi[1L] - tc * si[2L], bi[2L] - tc * si[1L]), interval(paper$ci_lower[[path]])),
          upper = overlap(c(bi[1L] + tc * si[1L], bi[2L] + tc * si[2L]), interval(paper$ci_upper[[path]])))
      }))
      if (all(parts == "fit")) "fit" else if (any(parts == "miss")) "miss" else "edge"
    }, "")
    return(verdict(fits_level))
  }
  if (type == "med_df") {
    # the four df are n - 2, n - 3, n - 2, n - 3 for one n: exact integers, so a path whose df
    # is not the right one for its place always contradicts the others
    n <- as.numeric(paper$n)
    right <- c(a = n - 2, b = n - 3, c = n - 2, c_prime = n - 3)[[p$path]]
    return(if (as.numeric(paper$df[[p$path]]) != right) "detectable" else "undetectable")
  }
  term <- p$term
  if (type %in% c("b_digit", "se_digit", "t_digit")) {
    bi <- interval(paper$b[[term]])
    si <- interval(paper$se[[term]])
    if (si[1L] <= 0) return("edge")
    ratios <- c(bi[1L] / si, bi[2L] / si)
    return(verdict(overlap(range(ratios), interval(paper$t[[term]]))))
  }
  if (type == "p_digit") {
    ti <- interval(paper$t[[term]])
    pi <- pmin(pmax(interval(paper$p[[term]]), 0), 1)
    tg <- seq(ti[1L], ti[2L], length.out = 101L)
    return(verdict(overlap(range(2 * pt(-abs(tg), as.numeric(paper$df2))), pi)))
  }
  if (type == "f_vs_r2") {
    ri <- pmin(pmax(interval(paper$r2), 0), 0.999999)
    d1 <- as.numeric(paper$df1)
    d2 <- as.numeric(paper$df2)
    fr <- (ri / d1) / ((1 - ri) / d2)
    # R2 may also be the adjusted R2 (a convention variant): R2 = 1 - (1 - adj) d2 / (d1 + d2)
    ra <- pmin(pmax(1 - (1 - ri) * d2 / (d1 + d2), 0), 0.999999)
    fa <- (ra / d1) / ((1 - ra) / d2)
    return(verdict(c(overlap(range(fr), interval(paper$f)), overlap(range(fa), interval(paper$f)))))
  }
  # df_wrong_k: n, df1 and df2 are exact integers
  if (as.numeric(paper$n) - as.numeric(paper$df1) - 1 != as.numeric(paper$df2)) "detectable" else "undetectable"
}
reg_check <- function(type, p, sub) {
  keys <- reg_subsets[[type]][[sub]]
  if (p$mediation) {
    keys <- if (is.null(keys)) names(p$paper) else keys
    return(run_mediation_check(p$paper, keys))
  }
  paper <- p$paper
  if (is.null(keys)) keys <- setdiff(names(paper), c("n"))
  # a simple regression fixes k = 1 and takes no k argument
  kk <- if (attr(paper, "handler") == "lm_simple") NULL else attr(paper, "k")
  if (sub == "n/df2 + k") return(run_lm_check(paper, keys, k = kk))
  if (sub == "all" && type == "df_wrong_k") keys <- names(paper)
  run_lm_check(paper, keys)
}
reg_rows <- list()
n_reg <- 700L
for (i in seq_len(n_reg)) {
  type <- reg_types[(i - 1L) %% length(reg_types) + 1L]
  planted <- NULL
  for (attempt in 1:200) {
    planted <- reg_paper(type)
    if (!is.null(planted)) break
  }
  if (is.null(planted)) next
  oracle <- reg_oracle(type, planted)
  for (sub in names(reg_subsets[[type]])) {
    res <- reg_check(type, planted, sub)
    outcome <- if (inherits(res, "error")) "error" else if (!res$consistent) "flagged" else "not flagged"
    reg_rows[[length(reg_rows) + 1L]] <- data.frame(
      case = i, type = type, subset = sub, outcome = outcome,
      oracle = if (sub != "all") oracle else NA_character_)
  }
}
reg_results <- do.call(rbind, reg_rows)
reg_cells <- aggregate(outcome ~ type + subset, reg_results,
                       function(o) c(n = length(o), flagged = sum(o == "flagged")))
reg_cells <- cbind(reg_cells[1:2], as.data.frame(reg_cells$outcome))
reg_cells$`% flagged` <- round(100 * reg_cells$flagged / reg_cells$n, 1)
cat("\n== Regression family planted errors ==\n")
print(reg_cells, row.names = FALSE)
rcov <- reg_results[!is.na(reg_results$oracle) & reg_results$oracle != "edge", ]
print(as.data.frame(table(oracle = rcov$oracle, outcome = rcov$outcome)), row.names = FALSE)
reg_errors <- sum(reg_results$outcome == "error")
reg_missed <- sum(rcov$oracle == "detectable" & rcov$outcome != "flagged")
reg_false <- sum(rcov$oracle == "undetectable" & rcov$outcome == "flagged")
rfl <- function(ty, s) unique(reg_results$case[reg_results$type == ty & reg_results$subset == s &
                                                 reg_results$outcome == "flagged"])
reg_hidden <- sum(vapply(reg_types, function(ty) {
  narrow <- setdiff(names(reg_subsets[[ty]]), "all")
  length(setdiff(unlist(lapply(narrow, function(s) rfl(ty, s))), rfl(ty, "all")))
}, numeric(1)))
reg_dead <- sum(reg_cells$flagged == 0)
reg_ok <- reg_errors == 0 && reg_missed == 0 && reg_false == 0 && reg_hidden == 0 && reg_dead == 0
cat(if (reg_ok) "\nPASS" else "\nFAIL", ": regression planted errors, errors = ", reg_errors,
    ", oracle-detectable not flagged = ", reg_missed, ", oracle-undetectable flagged = ",
    reg_false, ", flagged on a narrow subset but not on all = ", reg_hidden,
    ", cells never flagged = ", reg_dead, "\n", sep = "")
