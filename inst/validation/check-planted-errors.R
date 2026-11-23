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
# CIs (ADR 0014): with r unknown and three levels to choose from, a shifted M can still fit, so
# some large paired m_change errors became undetectable and the rate fell below 90%. The oracle
# agrees those cases are undetectable, so criterion 2 (oracle-detectable not flagged = 0) is the
# hard test; a raw rate cannot tell a regression from a genuinely undetectable error.
# Seed 20261124. Size: 1000 papers per handler group, 5 groups. Runtime about 4 minutes
# (6 cores via parallel::mclapply).
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
      # into p (ADR 0010: "no variant is ruled out"), so p is judged against that hull.
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
  NA_character_
}
outcome <- unlist(parallel::mclapply(jobs, run_job, mc.cores = 6L))
oracle <- unlist(parallel::mclapply(jobs, oracle_job, mc.cores = 6L))
# nolint start: indentation_linter
results <- data.frame(
  case = vapply(jobs, `[[`, "", "case"), group = vapply(jobs, `[[`, "", "group"),
  type = vapply(jobs, `[[`, "", "type"), size = vapply(jobs, `[[`, "", "size"),
  subset = vapply(jobs, `[[`, "", "subset"), outcome = outcome, oracle = oracle,
  df_decimals = vapply(jobs, function(j) n_decimals(j$paper$df), 0L),
  level = vapply(jobs, function(j) attr(j$paper, "level"), 0),
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
