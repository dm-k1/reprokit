# Range exactness: relation_range() returns the true range of a relation over the
# box of rounding intervals of its inputs.
# Claim: no value of the relation, sampled inside a (domain-clamped) box, falls
# outside the range relation_range() computed for that box. Samples are uniform
# draws in the box plus draws on the slices sd[i] = r * sd[3-i] (t_paired), where
# t_from_descriptives and the CI relations have interior extremes that the box
# corners miss, and on the slice where the Welch df peaks (t_independent). Handlers:
# t_paired, cor_pearson, t_independent (Student and Welch variants, per-group n),
# t_one_sample. Four known non-monotone boxes are run as named cases and print
# the corner-only range next to the true range (the third is the Welch df, whose
# peak in the SD ratio lies between the corners; the fourth is the paired d_rm).
# PASS criterion: no sampled value escapes any range (1e-9 relative slack for
# floating point), and in all named cases the corner-only range is strictly
# narrower than the true range (so the check is capable of failing).
# Size: 300 random boxes per handler x every relation x every variant, 400 uniform
# + 400 slice draws each. Boxes where the range is not finite are skipped and counted
# (check_reported() treats those as "not evaluable").
# One-way ANOVA F, a second section with its own PASS line: (a) the F range of
# 300 random boxes (k = 2..5 groups, per-group n exact or a range, M and SD rounded) against
# 20000 uniform draws of the box each (none may escape); (b) the exact smallest SSb
# (oneway_ssb_min) against stats::optimize() over the shared point, on 1000 random boxes
# (agreement within 1e-6 relative); (c) named case: three overlapping mean intervals, where
# the corner-only minimum F is above 0 and the exact minimum is 0 (printed side by side).
# Factorial ANOVA F and p (ADR 0017), a fourth section with its own PASS line: the two
# range cases the ADR decides, (a) df1 = 1 (a contrast: 2 x 2 to 2 x 4 designs, effect A, and AB
# of a 2 x 2; balanced and unbalanced, every SS type) and (b) a balanced main effect with
# df1 > 1 (marginal means). 60 random boxes per case against 2000 uniform draws each (none may
# escape, for F and for p); the F upper bound is attained: the largest F over the 2^cells
# corners of the means at the smallest SD equals the range upper bound (F is convex in the
# means, so the maximum is a vertex; uniform draws never get near it, which is why it is
# checked at the vertex). A third check is that the lower bound is not vacuous: the sampled
# minimum is within 50% of the range width of the lower bound (a range that is far too wide
# would fail; the printed upper gap is information only). The cases that have no
# exact range (the interaction with df1 > 1, an unbalanced effect with df1 > 1) must return a
# non-finite range (skipped), never a guess.
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-range-exactness.R")'

source("inst/validation/record.R")
record_start("check-range-exactness", seed = c(20261123, 20261125))
source("inst/validation/parallel-helpers.R")
started <- Sys.time()

n_boxes <- 300L
uniform_draws <- 400L
slice_draws <- 400L
# Parallel and reproducible: every section is cut into tasks that do not depend on the number of
# cores (a handler x 25 boxes, a relation x variant, ...), each task runs on its own L'Ecuyer-CMRG
# stream (par_run() in parallel-helpers.R), so the numbers are the same on 1 or 8 cores.
# REPROKIT_VALIDATION_CORES sets the core count.

fmt <- function(x, d) formatC(x, format = "f", digits = d)

random_paper <- function(handler) {
  n_pool <- c(3, 4, 5, 8, 12, 20, 40, 100)
  if (handler == "t_independent") {
    ns <- sample(n_pool, 2L, replace = TRUE)
    dec <- sample(0:3, 1L)
    scale <- 10^runif(1L, -0.5, 1.5)
    return(list(n = as.character(ns), t = fmt(rnorm(1L) * 4, sample(0:2, 1L)),
                df = as.character(sum(ns) - 2),
                m = fmt(rnorm(2L) * scale, dec),
                sd = fmt(abs(rnorm(2L)) * scale + 0.02, dec)))
  }
  n <- sample(n_pool, 1L)
  dec <- sample(0:3, 1L)
  scale <- 10^runif(1L, -0.5, 1.5)
  rho_dec <- sample(1:3, 1L)
  r <- fmt(sample(c(runif(1L, -1, 1), 0.99, 0.999, -0.99, 1, 0), 1L), rho_dec)
  r <- fmt(max(-1, min(1, as.numeric(r))), rho_dec)
  t_dec <- sample(0:2, 1L)
  paper <- list(n = as.character(n), r = r, t = fmt(rnorm(1L) * 4, t_dec),
                df = as.character(n - if (handler %in% c("t_paired", "t_one_sample")) 1 else 2),
                m = c(fmt(rnorm(1L) * scale, dec), fmt(rnorm(1L) * scale, dec)),
                sd = fmt(abs(rnorm(2L)) * scale + 0.02, dec))
  if (handler == "t_one_sample") {
    paper$m <- paper$m[1L]
    paper$sd <- paper$sd[1L]
  }
  paper
}

box_of <- function(paper) {
  Map(function(x, quantity) restrict_to_domain(interval_bounds(reported_value(x)), quantity, x),
      paper, names(paper))
}

draw_uniform <- function(box, inputs) {
  lapply(box[inputs], function(b) b$lower + runif(length(b$lower)) * (b$upper - b$lower))
}

# Uniform draw, then sd[i] forced to r * sd[3-i] when that lies inside its interval.
draw_slice <- function(box, inputs) {
  point <- draw_uniform(box, inputs)
  if (all(c("sd", "r") %in% inputs)) {
    i <- sample(1:2, 1L)
    target <- point$r * point$sd[3L - i]
    if (is.finite(target) && target > box$sd$lower[i] && target < box$sd$upper[i]) {
      point$sd[i] <- target
    }
  }
  # Welch df peaks where sd1^2/n1 = (sd2^2/n2) * (n1 - 1)/(n2 - 1).
  if (all(c("sd", "n") %in% inputs) && length(point$n) == 2L) {
    i <- sample(1:2, 1L)
    n <- point$n
    ratio <- (n[i] - 1) / (n[3L - i] - 1)
    target <- sqrt(point$sd[3L - i]^2 / n[3L - i] * ratio * n[i])
    if (is.finite(target) && target > box$sd$lower[i] && target < box$sd$upper[i]) {
      point$sd[i] <- target
    }
  }
  point
}

box_chunk <- 25L
tasks1 <- do.call(c, lapply(c("t_paired", "cor_pearson", "t_independent", "t_one_sample"), function(h) {
  lapply(seq_len(n_boxes %/% box_chunk), function(k) list(handler = h, n = box_chunk))
}))
chunk_rows <- par_run(tasks1, function(task) {
  handler <- task$handler
  rels <- get_handler(handler)$relations
  rows <- list()
  for (b in seq_len(task$n)) {
    box <- box_of(random_paper(handler))
    for (rel in rels) {
      for (variant in variant_combinations(rel$variants)) {
        range <- tryCatch(relation_range(rel, box, variant),
                          error = function(e) c(NA_real_, NA_real_))
        key <- paste(handler, rel$name, paste(variant, collapse = "+"))
        if (!all(is.finite(range))) {
          rows[[key]] <- rbind(rows[[key]], c(0, 0, 1))
          next
        }
        draws <- c(replicate(uniform_draws, draw_uniform(box, rel$inputs), simplify = FALSE),
                   replicate(slice_draws, draw_slice(box, rel$inputs), simplify = FALSE))
        values <- vapply(draws, function(pt) rel$fn(pt, variant), numeric(1))
        values <- values[is.finite(values)]
        slack <- 1e-9 * max(1, abs(range))
        bad <- sum(values < range[["lower"]] - slack | values > range[["upper"]] + slack)
        rows[[key]] <- rbind(rows[[key]], c(1, bad, 0))
      }
    }
  }
  rows
}, seed = 20261123)
rows <- list()
for (cr in chunk_rows) for (key in names(cr)) rows[[key]] <- rbind(rows[[key]], cr[[key]])
escaped <- sum(vapply(rows, function(m) sum(m[, 2L]), 0))
exactness <- do.call(rbind, lapply(names(rows), function(key) {
  m <- rows[[key]]
  data.frame(relation = key, boxes = sum(m[, 1L]), escaped_samples = sum(m[, 2L]),
             skipped_not_finite = sum(m[, 3L]))
}))
print(exactness, row.names = FALSE)

# Named case 1: t_from_descriptives, sd[1] in [1.95, 2.05], r = .99, sd[2] = 2:
# t peaks at sd[1] = r * sd[2] = 1.98, between the corners.
rel_t <- get_handler("t_paired")$relations[[2L]]
stopifnot(identical(rel_t$name, "t_from_descriptives"))
box_t <- list(n = list(lower = 20, upper = 20),
              m = list(lower = c(5, 4), upper = c(5, 4)),
              sd = list(lower = c(1.95, 2), upper = c(2.05, 2)),
              r = list(lower = 0.99, upper = 0.99))
corners_t <- rel_t
corners_t$stationary <- NULL
scan_t <- vapply(seq(1.95, 2.05, length.out = 100001L), function(s1) {
  rel_t$fn(list(n = 20, m = c(5, 4), sd = c(s1, 2), r = 0.99), NULL)
}, numeric(1))
# Named case 2: two-sided p from t in [-0.005, 0.005], df = 19: p reaches 1 at t = 0.
rel_p <- get_handler("t_paired")$relations[[3L]]
stopifnot(identical(rel_p$name, "p_from_t"))
box_p <- list(t = interval_bounds(reported_value("0.00")), df = list(lower = 19, upper = 19))
corners_p <- rel_p
corners_p$stationary <- NULL

# Named case 3: Welch df, n = (5, 6), sd = (2.0-2.4, 1.0-1.4): df peaks in sd1 at
# sd1 = sqrt(sd2^2 / n2 * (n1 - 1)/(n2 - 1) * n1) between the corners (sd2 = 1.2: 1.0).
rel_df <- get_handler("t_independent")$relations[[1L]]
stopifnot(identical(rel_df$name, "df_from_n"))
box_df <- list(n = list(lower = c(5, 6), upper = c(5, 6)),
               sd = list(lower = c(0.9, 1.0), upper = c(1.1, 1.4)))
corners_df <- rel_df
corners_df$stationary <- NULL
scan_df <- max(vapply(seq(0.9, 1.1, length.out = 2001L), function(s1) {
  vapply(seq(1.0, 1.4, length.out = 201L), function(s2) {
    rel_df$fn(list(n = c(5, 6), sd = c(s1, s2)), "welch")
  }, numeric(1))
}, numeric(201L)))

# Named case 4: paired d_rm (and d_z), same box as case 1: d = diff / SD of the differences,
# peaks where sd[1] = r * sd[2]; the relation lists that point as stationary. A scan of the
# convention over sd[1] is the independent truth (this is the exactness check for d_rm).
rel_d <- get_handler("t_paired")$relations[[6L]]
stopifnot(identical(rel_d$name, "d_from_descriptives"))
box_d <- list(m = list(lower = c(5, 4), upper = c(5, 4)),
              sd = list(lower = c(1.95, 2), upper = c(2.05, 2)),
              r = list(lower = 0.99, upper = 0.99))
corners_d <- rel_d
corners_d$stationary <- NULL
scan_d <- max(vapply(seq(1.95, 2.05, length.out = 100001L), function(s1) {
  rel_d$fn(list(m = c(5, 4), sd = c(s1, 2), r = 0.99), "d_rm")
}, numeric(1)))

case_names <- c("t_from_descriptives (sd1 1.95-2.05, r .99)",
                "p_from_t two.sided (t = 0.00, df 19)",
                "df_from_n welch (n 5/6, sd 0.9-1.1 / 1.0-1.4)",
                "d_from_descriptives d_rm (sd1 1.95-2.05, r .99)")
corner_upper <- c(relation_range(corners_t, box_t)[["upper"]],
                  relation_range(corners_p, box_p, "two.sided")[["upper"]],
                  relation_range(corners_df, box_df, "welch")[["upper"]],
                  relation_range(corners_d, box_d, "d_rm")[["upper"]])
true_upper <- c(relation_range(rel_t, box_t)[["upper"]],
                relation_range(rel_p, box_p, "two.sided")[["upper"]],
                relation_range(rel_df, box_df, "welch")[["upper"]],
                relation_range(rel_d, box_d, "d_rm")[["upper"]])
named <- data.frame(case = case_names, corner_only_upper = corner_upper,
                    true_upper = true_upper, scan_upper = c(max(scan_t), 1, scan_df, scan_d))
named[, -1L] <- round(named[, -1L], 3L)
cat("\n")
print(named, row.names = FALSE)

named_ok <- all(named$corner_only_upper < named$true_upper - 1e-3) &&
  isTRUE(all.equal(named$true_upper, named$scan_upper, tolerance = 1e-3))
pass <- escaped == 0L && named_ok && all(exactness$boxes > 0L)
cat(if (pass) "\nPASS" else "\nFAIL", ": escaped samples = ", escaped,
    ", named cases show corner-only < true = ", named_ok, "\n", sep = "")

# One-way ANOVA F range -----------------------------------------------------------
rel_f <- get_handler("anova_oneway")$relations[[3L]]
stopifnot(identical(rel_f$name, "f_from_descriptives"))

random_oneway_box <- function() {
  k <- sample(2:5, 1L)
  n <- sample(c(3, 5, 8, 15, 40), k, replace = TRUE)
  dec <- sample(0:2, 1L)
  scale <- 10^runif(1L, -0.5, 1)
  m <- rnorm(k) * scale * runif(1L, 0, 1)
  sd <- abs(rnorm(k)) * scale + 0.3 * scale
  half <- 0.5 * 10^(-dec)
  list(n = list(lower = n, upper = n),
       m = list(lower = round(m, dec) - half, upper = round(m, dec) + half),
       sd = list(lower = pmax(round(sd, dec) - half, 1e-3), upper = round(sd, dec) + half))
}
f_parts <- par_run(seq_len(12L), function(chunk) {
  out <- c(boxes = 0, skipped = 0, escaped = 0)
  for (b in seq_len(25L)) {
    box <- random_oneway_box()
    range <- relation_range(rel_f, box, NULL)
    if (!all(is.finite(range))) {
      out[["skipped"]] <- out[["skipped"]] + 1
      next
    }
    out[["boxes"]] <- out[["boxes"]] + 1
    vals <- vapply(seq_len(20000L), function(i) {
      pt <- lapply(box[c("n", "m", "sd")], function(x) {
        x$lower + runif(length(x$lower)) * (x$upper - x$lower)
      })
      rel_f$fn(pt, NULL)
    }, numeric(1))
    vals <- vals[is.finite(vals)]
    slack <- 1e-9 * max(1, abs(range))
    out[["escaped"]] <- out[["escaped"]] +
      sum(vals < range[["lower"]] - slack | vals > range[["upper"]] + slack)
  }
  out
}, seed = 20261123)
f_tot <- Reduce(`+`, f_parts)
f_boxes <- f_tot[["boxes"]]
f_skipped <- f_tot[["skipped"]]
f_escaped <- f_tot[["escaped"]]

# (b) exact SSb minimum against optimize() over the shared point c.
ssb_gap <- unlist(par_run(seq_len(10L), function(chunk) vapply(seq_len(100L), function(i) {
  k <- sample(2:6, 1L)
  n <- sample(3:40, k, replace = TRUE)
  centre <- rnorm(k)
  half <- runif(k, 0.001, 0.8)
  lo <- centre - half
  hi <- centre + half
  g <- function(c) sum(n * pmax(lo - c, 0, c - hi)^2)
  opt <- optimize(g, range(c(lo, hi)), tol = 1e-12)$objective
  exact <- oneway_ssb_min(n, lo, hi)
  abs(exact - opt) / max(1, opt)
}, numeric(1)), seed = 20261124))

# (c) overlapping mean intervals: the minimum F is 0, the corners alone say more.
box_o <- list(n = list(lower = rep(10, 3), upper = rep(10, 3)),
              m = list(lower = c(4.995, 5.295, 5.305), upper = c(5.005, 5.305, 5.315)),
              sd = list(lower = rep(0.995, 3), upper = rep(1.005, 3)))
box_o$m$lower[3L] <- 5.295
box_o$m$upper[1L] <- 5.305
corner_only <- rel_f
corner_only$range <- NULL
corner_min <- relation_range(corner_only, box_o, NULL)[["lower"]]
exact_min <- relation_range(rel_f, box_o, NULL)[["lower"]]
cat("\n== One-way F range ==\n")
print(data.frame(check = c("random boxes with a finite range", "boxes skipped (not evaluable)",
                           "samples outside the range", "max |exact - optimize()| SSb min",
                           "corner-only minimum F, overlapping means",
                           "exact minimum F, overlapping means"),
                 value = signif(c(f_boxes, f_skipped, f_escaped, max(ssb_gap), corner_min,
                                  exact_min), 4L)), row.names = FALSE)
one_pass <- f_escaped == 0L && max(ssb_gap) < 1e-6 && corner_min > 0 && exact_min == 0
cat(if (one_pass) "\nPASS" else "\nFAIL", ": one-way F, escaped samples = ", f_escaped,
    ", SSb min agrees with optimize() = ", max(ssb_gap) < 1e-6,
    ", corner-only min > 0 and exact min = 0: ", corner_min > 0 && exact_min == 0, "\n", sep = "")

# Cramer's V and Cohen's w (ADR 0016), a third section with its own PASS line ------------
# (a) v_from_table: counts are exact, so the range of V over the box is a point and equals V from
# stats::chisq.test() (200 random tables, 2 x 2 to 4 x 5; and w for 200 goodness-of-fit
# vectors). (b) v_from_chisq (chi-squared rounded to 2 dp, N exact or a range): 300 random boxes
# per handler against 5000 uniform draws each; none may escape the range, and V stays in [0, 1]
# for independence after clamping to the domain (it may exceed 1 for w).
indep_rels <- relations_at(get_handler("chisq_independence"), list(dims = c(3L, 4L)))
rel_v_table <- Filter(function(r) r$name == "v_from_table", indep_rels)[[1L]]
rel_v_chisq <- Filter(function(r) r$name == "v_from_chisq", indep_rels)[[1L]]
gof_rels <- relations_at(get_handler("chisq_gof"), list(dims = 5L))
rel_w_chisq <- Filter(function(r) r$name == "w_from_chisq", gof_rels)[[1L]]
point <- function(x) list(lower = x, upper = x)

table_gap <- unlist(par_run(seq_len(4L), function(chunk) vapply(seq_len(50L), function(i) {
  r <- sample(2:4, 1L)
  c_ <- sample(2:5, 1L)
  tab <- matrix(rpois(r * c_, 15) + 1L, r, c_)
  rels <- relations_at(get_handler("chisq_independence"), list(dims = c(r, c_)))
  rel <- Filter(function(x) x$name == "v_from_table", rels)[[1L]]
  box <- list(n = point(as.vector(tab)))
  rng <- relation_range(rel, box, NULL)
  truth <- sqrt(unname(chisq.test(tab, correct = FALSE)$statistic) / (sum(tab) * (min(r, c_) - 1)))
  max(abs(rng - truth)) / max(1, truth)
}, numeric(1)), seed = 20261126))
w_gap <- unlist(par_run(seq_len(4L), function(chunk) vapply(seq_len(50L), function(i) {
  k <- sample(2:8, 1L)
  n <- rpois(k, 12) + 1L
  rel <- Filter(function(x) x$name == "w_from_counts",
                relations_at(get_handler("chisq_gof"), list(dims = k)))[[1L]]
  rng <- relation_range(rel, list(n = point(n)), NULL)
  truth <- sqrt(unname(chisq.test(n)$statistic) / sum(n))
  max(abs(rng - truth)) / max(1, truth)
}, numeric(1)), seed = 20261127))

v_box <- function() {
  chisq <- round(10^runif(1L, -1, 2.5), 2)
  total <- sample(c(20, 60, 200, 1000), 1L)
  list(chisq = list(lower = max(chisq - 0.005, 0), upper = chisq + 0.005),
       n_total = if (runif(1L) < 0.5) point(total) else list(lower = total, upper = total + 5))
}
v_tasks <- do.call(c, lapply(1:2, function(r) lapply(seq_len(6L), function(k) list(rel = r, n = 50L))))
v_parts <- par_run(v_tasks, function(task) {
  rel <- list(rel_v_chisq, rel_w_chisq)[[task$rel]]
  out <- c(boxes = 0, escaped = 0)
  for (b in seq_len(task$n)) {
    box <- v_box()
    range <- relation_range(rel, box, NULL)
    if (!all(is.finite(range))) next
    out[["boxes"]] <- out[["boxes"]] + 1
    vals <- vapply(seq_len(5000L), function(i) {
      pt <- lapply(box, function(x) x$lower + runif(1L) * (x$upper - x$lower))
      rel$fn(pt, NULL)
    }, numeric(1))
    slack <- 1e-9 * max(1, abs(range))
    out[["escaped"]] <- out[["escaped"]] + sum(vals < range[["lower"]] - slack | vals > range[["upper"]] + slack)
  }
  out
}, seed = 20261128)
v_tot <- Reduce(`+`, v_parts)
v_boxes <- v_tot[["boxes"]]
v_escaped <- v_tot[["escaped"]]
cat("\n== Cramer's V and Cohen's w ==\n")
print(data.frame(check = c("max |range - chisq.test() V|, 200 tables",
                           "max |range - chisq.test() w|, 200 vectors",
                           "random chi-squared boxes", "samples outside the range"),
                 value = signif(c(max(table_gap), max(w_gap), v_boxes, v_escaped), 4L)),
      row.names = FALSE)
v_pass <- max(table_gap) < 1e-9 && max(w_gap) < 1e-9 && v_escaped == 0L && v_boxes > 0L
cat(if (v_pass) "\nPASS" else "\nFAIL", ": V and w ranges, point from counts equals chisq.test() = ",
    max(c(table_gap, w_gap)) < 1e-9, ", escaped samples = ", v_escaped, "\n", sep = "")

# Factorial ANOVA F and p (ADR 0017), a fourth section with its own PASS line ---------------
factorial_box <- function(dims, balanced) {
  cells <- prod(dims)
  n <- if (balanced) rep(sample(c(3, 5, 8, 15), 1L), cells) else sample(c(3, 4, 6, 10, 20), cells, replace = TRUE)
  dec <- sample(0:2, 1L)
  scale <- 10^runif(1L, -0.5, 1)
  m <- rnorm(cells) * scale * runif(1L, 0, 1)
  sd <- abs(rnorm(cells)) * scale + 0.3 * scale
  half <- 0.5 * 10^(-dec)
  list(n = list(lower = n, upper = n),
       m = list(lower = round(m, dec) - half, upper = round(m, dec) + half),
       sd = list(lower = pmax(round(sd, dec) - half, 1e-3), upper = round(sd, dec) + half))
}
fact_cases <- list(
  list(dims = c(2L, 2L), effect = "A"), list(dims = c(2L, 2L), effect = "B"),
  list(dims = c(2L, 2L), effect = "AB"), list(dims = c(2L, 3L), effect = "A"),
  list(dims = c(2L, 4L), effect = "A"), list(dims = c(3L, 2L), effect = "B"),
  list(dims = c(2L, 3L), effect = "B"), list(dims = c(3L, 3L), effect = "A"),
  list(dims = c(3L, 4L), effect = "B"))
fact_tasks <- list()
for (case in fact_cases) for (balanced in c(TRUE, FALSE)) for (type in c("III", "II", "I")) {
  if (balanced && type != "III") next
  fact_tasks[[length(fact_tasks) + 1L]] <- list(case = case, balanced = balanced, type = type)
}
fact_parts <- par_run(fact_tasks, function(task) {
  case <- task$case
  balanced <- task$balanced
  type <- task$type
  fact_unbounded_ok <- TRUE
  {
    {
      rels <- relations_at(get_handler("anova_factorial"),
                           list(dims = case$dims, effect = case$effect, balanced = balanced))
      rel_f <- Filter(function(r) r$name == "f_from_descriptives", rels)[[1L]]
      rel_p <- Filter(function(r) r$name == "p_from_descriptives", rels)[[1L]]
      df1 <- c(A = case$dims[1L] - 1, B = case$dims[2L] - 1,
               AB = prod(case$dims - 1))[[case$effect]]
      exact_case <- df1 == 1 || (balanced && case$effect != "AB")
      variant <- if (is.null(rel_f$variants)) NULL else type
      escaped <- 0L; boxes <- 0L; skipped <- 0L; attained <- TRUE; lo_gap <- 0; hi_gap <- 0
      for (b in seq_len(60L)) {
        box <- factorial_box(case$dims, balanced)
        rng <- relation_range(rel_f, box, variant)
        rng_p <- relation_range(rel_p, box, variant)
        if (!all(is.finite(rng))) {
          skipped <- skipped + 1L
          if (exact_case) fact_unbounded_ok <- FALSE
          next
        }
        if (!exact_case) fact_unbounded_ok <- FALSE
        boxes <- boxes + 1L
        draws <- vapply(seq_len(2000L), function(i) {
          pt <- lapply(box[c("n", "m", "sd")], function(x) x$lower + runif(length(x$lower)) * (x$upper - x$lower))
          c(rel_f$fn(pt, variant), rel_p$fn(pt, variant))
        }, numeric(2))
        fv <- draws[1L, ]; pv <- draws[2L, ]
        slack <- 1e-9 * max(1, abs(rng))
        slack_p <- 1e-9
        escaped <- escaped + sum(fv < rng[["lower"]] - slack | fv > rng[["upper"]] + slack) +
          sum(pv < rng_p[["lower"]] - slack_p | pv > rng_p[["upper"]] + slack_p)
        # The upper bound is a vertex: the means at their corners, the SDs at their minimum.
        if (exact_case) {
          corners <- as.matrix(expand.grid(Map(function(lo, hi) c(lo, hi), box$m$lower, box$m$upper)))
          top <- max(apply(corners, 1L, function(mm) {
            rel_f$fn(list(n = box$n$lower, m = mm, sd = pmax(box$sd$lower, 0)), variant)
          }))
          if (abs(top - rng[["upper"]]) > 1e-8 * max(1, top)) attained <- FALSE
        }
        width <- rng[["upper"]] - rng[["lower"]]
        lo_gap <- max(lo_gap, (min(fv) - rng[["lower"]]) / width)
        hi_gap <- max(hi_gap, (rng[["upper"]] - max(fv)) / width)
      }
      row <- data.frame(
        design = paste(case$dims, collapse = "x"), effect = case$effect, df1 = df1,
        balanced = balanced, type = if (is.null(variant)) "-" else type, boxes = boxes,
        skipped = skipped, escaped = escaped, upper_attained = attained,
        worst_lower_gap = round(lo_gap, 3L), worst_upper_gap = round(hi_gap, 3L))
    }
  }
  list(row = row, ok = fact_unbounded_ok)
}, seed = 20261129)
fact_table <- do.call(rbind, lapply(fact_parts, `[[`, "row"))
fact_unbounded_ok <- all(vapply(fact_parts, `[[`, logical(1), "ok"))
# no-exact-range cases must be unbounded
unbounded <- vapply(list(list(c(3L, 3L), "AB", TRUE), list(c(2L, 3L), "B", FALSE),
                         list(c(3L, 4L), "AB", FALSE)), function(z) {
  rels <- relations_at(get_handler("anova_factorial"), list(dims = z[[1L]], effect = z[[2L]], balanced = z[[3L]]))
  rel_f <- Filter(function(r) r$name == "f_from_descriptives", rels)[[1L]]
  !any(is.finite(relation_range(rel_f, factorial_box(z[[1L]], z[[3L]]), if (z[[3L]]) NULL else "III")))
}, logical(1))
cat("\n== Factorial ANOVA F and p ranges ==\n")
print(fact_table[fact_table$boxes > 0L | fact_table$skipped > 0L, ], row.names = FALSE)
cat("no-exact-range cases (interaction with df1 > 1, unbalanced df1 > 1) unbounded: ",
    all(unbounded), "\n", sep = "")
fact_pass <- all(fact_table$escaped == 0L) && all(fact_table$upper_attained) &&
  sum(fact_table$boxes) > 0L && all(fact_table$worst_lower_gap < 0.5) && fact_unbounded_ok && all(unbounded)
cat(if (fact_pass) "\nPASS" else "\nFAIL", ": factorial ranges, escaped samples = ",
    sum(fact_table$escaped), ", F upper bound attained at a vertex = ", all(fact_table$upper_attained),
    ", sampled minimum within 50% of the width of the lower bound = ",
    all(fact_table$worst_lower_gap < 0.5),
    ", others unbounded = ", all(unbounded), "\n", sep = "")

# Regression family (ADR 0020), a fifth section with its own PASS line. Relations: t = b / SE,
# the CI b -/+ t_crit * SE (at 90, 95, 99%, df exact), F from R2 (R2 and adjusted R2 variants),
# the two-sided p from t (a stationary point at t = 0), and the simple regression's r = b SD_x / SD_y.
# The ADR claims the first three are monotone in each rounded input, so their corners are exact.
# That claim is checked at the vertices, which uniform sampling never reaches: the range's lower
# and upper bounds must EQUAL the smallest and largest value over the 2^m vertices (computed here
# from rel$fn, not from relation_range), and 2000 uniform draws of the box must stay inside the
# range. p from t has an interior maximum (p = 1 at t = 0) when the box spans 0: there the upper
# bound must be 1, while the corner-only upper bound is below 1 (a named case, so the check can
# fail). Boxes where SE or SD may be zero have no finite range and are skipped and counted.
# PASS criterion: no escaped draw, bounds equal the vertex extremes (1e-12 relative) for the
# three monotone relations, p's upper bound is 1 on every box spanning t = 0, and the corner-only
# upper bound is below 1 in the named case.
reg_rels <- relations_at(get_handler("lm_simple"), list(terms = "x"))
reg_find <- function(name) Filter(function(r) r$name == name, reg_rels)[[1L]]
reg_rels_m <- relations_at(get_handler("lm_multiple"), list(terms = "x", k = 2L))
reg_find_m <- function(name) Filter(function(r) r$name == name, reg_rels_m)[[1L]]
iv <- function(center, half) list(lower = center - half, upper = center + half)
vertex_values <- function(rel, box, variant) {
  grid <- do.call(expand.grid, lapply(rel$inputs, function(i) c(box[[i]]$lower, box[[i]]$upper)))
  names(grid) <- rel$inputs
  vapply(seq_len(nrow(grid)), function(i) rel$fn(as.list(grid[i, , drop = FALSE]), variant), numeric(1))
}
reg_box <- function(rel) {
  b <- runif(1L, -5, 5); se <- 10^runif(1L, -1.5, 0.5); dec <- sample(1:3, 1L)
  h <- 0.5 * 10^-dec
  box <- list(`b:x` = iv(round(b, dec), h), `se:x` = iv(round(se, dec), h),
              `t:x` = iv(round(b / se, 2L), 0.005), df2 = point(sample(c(5, 12, 30, 120), 1L)),
              r2 = iv(round(runif(1L, 0.02, 0.95), 2L), 0.005), df1 = point(sample(1:5, 1L)),
              `sd:x` = iv(round(runif(1L, 0.3, 3), 2L), 0.005),
              `sd:(response)` = iv(round(runif(1L, 3, 9), 2L), 0.005))
  # rounding intervals are intersected with the quantity's domain (SE and SD are >= 0)
  for (nm in c("se:x", "sd:x", "sd:(response)")) box[[nm]]$lower <- max(box[[nm]]$lower, 0)
  box[c(rel$inputs)]
}
reg_tasks <- list()
for (rname in c("t_from_b_se:x", "ci_lower_from_b_se:x", "ci_upper_from_b_se:x", "f_from_r2",
                "r_from_b", "p_from_t:x")) {
  for (variant in variant_combinations(reg_find(rname)$variants)) {
    for (chunk in 1:4) reg_tasks[[length(reg_tasks) + 1L]] <- list(rname = rname, variant = variant)
  }
}
reg_parts <- par_run(reg_tasks, function(task) {
  rname <- task$rname
  variant <- task$variant
  rel <- reg_find(rname)
  monotone <- rname != "p_from_t:x"
  boxes <- 0L; esc <- 0L; miss <- 0L; skip <- 0L
  for (b in seq_len(50L)) {
    box <- reg_box(rel)
    range <- relation_range(rel, box, variant)
    if (!all(is.finite(range))) { skip <- skip + 1L; next }
    boxes <- boxes + 1L
    draws <- replicate(2000L, lapply(box, function(x) x$lower + runif(1L) * (x$upper - x$lower)),
                       simplify = FALSE)
    vals <- vapply(draws, function(pt) rel$fn(pt, variant), numeric(1))
    slack <- 1e-9 * max(1, abs(range))
    esc <- esc + sum(vals < range[["lower"]] - slack | vals > range[["upper"]] + slack)
    if (monotone) {
      v <- vertex_values(rel, box, variant)
      scale <- max(1, abs(range))
      if (abs(min(v) - range[["lower"]]) > 1e-12 * scale ||
          abs(max(v) - range[["upper"]]) > 1e-12 * scale) miss <- miss + 1L
    }
  }
  data.frame(relation = rname,
             variant = if (is.null(variant)) "-" else paste(unlist(variant), collapse = "+"),
             boxes = boxes, skipped = skip, escaped = esc,
             vertex_mismatch = if (monotone) miss else NA_integer_)
}, seed = 20261125)
reg_all <- do.call(rbind, reg_parts)
reg_rows <- lapply(split(reg_all, interaction(reg_all$relation, reg_all$variant, drop = TRUE, lex.order = FALSE)),
                   function(d) data.frame(relation = d$relation[1L], variant = d$variant[1L],
                                          boxes = sum(d$boxes), skipped = sum(d$skipped),
                                          escaped = sum(d$escaped),
                                          vertex_mismatch = if (all(is.na(d$vertex_mismatch))) NA_integer_ else sum(d$vertex_mismatch)))
reg_rows <- reg_rows[order(match(vapply(reg_rows, function(r) r$relation, ""),
                                 unique(reg_all$relation)))]
reg_escaped <- sum(reg_all$escaped)
reg_vertex_miss <- sum(reg_all$vertex_mismatch, na.rm = TRUE)
reg_skipped <- sum(reg_all$skipped)
# p from t: a box spanning 0 has maximum exactly 1 at its interior point.
rel_pt <- reg_find("p_from_t:x")
span_ok <- vapply(1:50, function(i) {
  box <- list(`t:x` = iv(runif(1L, -0.02, 0.02), 0.005 + runif(1L, 0, 0.03)), df2 = point(sample(c(5, 30), 1L)))
  box$`t:x`$lower <- -abs(box$`t:x`$lower) - 0.005
  box$`t:x`$upper <- abs(box$`t:x`$upper) + 0.005
  abs(relation_range(rel_pt, box, NULL)[["upper"]] - 1) < 1e-12
}, logical(1))
corner_only <- rel_pt
corner_only$stationary <- NULL
named_box <- list(`t:x` = list(lower = -0.5, upper = 0.7), df2 = point(20))
corner_upper_p <- relation_range(corner_only, named_box, NULL)[["upper"]]
true_upper_p <- relation_range(rel_pt, named_box, NULL)[["upper"]]
cat("\n== Regression family ranges ==\n")
print(do.call(rbind, reg_rows), row.names = FALSE)
cat("p from t on a box spanning t = 0: upper bound is 1 in ", sum(span_ok), " of ", length(span_ok),
    " boxes; named case t in [-0.5, 0.7], df 20: corner-only upper = ", signif(corner_upper_p, 4L),
    ", true upper = ", signif(true_upper_p, 4L), "\n", sep = "")
reg_pass <- reg_escaped == 0L && reg_vertex_miss == 0L && all(span_ok) &&
  corner_upper_p < 1 - 1e-6 && abs(true_upper_p - 1) < 1e-12 &&
  sum(vapply(reg_rows, function(r) r$boxes, 0L)) > 0L
cat(if (reg_pass) "\nPASS" else "\nFAIL", ": regression ranges, escaped draws = ", reg_escaped,
    ", vertex mismatches = ", reg_vertex_miss, ", skipped boxes = ", reg_skipped, "\n", sep = "")

all_pass <- pass && one_pass && v_pass && fact_pass && reg_pass
cat(if (all_pass) "\nPASS" else "\nFAIL", ": all range-exactness sections (cores: ", par_cores(),
    ", runtime ", round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1), " min)\n", sep = "")
finish_validation(all_pass)
