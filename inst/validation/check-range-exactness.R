# Range exactness: relation_range() returns the true range of a relation over the
# box of rounding intervals of its inputs.
# Claim: no value of the relation, sampled inside a (domain-clamped) box, falls
# outside the range relation_range() computed for that box. Samples are uniform
# draws in the box plus draws on the slices sd[i] = r * sd[3-i], where
# t_from_descriptives and the CI relations have interior extremes that the box
# corners miss. Two known non-monotone boxes are run as named cases and print
# the corner-only range next to the true range.
# PASS criterion: no sampled value escapes any range (1e-9 relative slack for
# floating point), and in both named cases the corner-only range is strictly
# narrower than the true range (so the check is capable of failing).
# Size: 300 random boxes per handler x every relation x every variant, 400 uniform
# + 400 slice draws each. Boxes where the range is not finite are skipped and counted
# (check_reported() treats those as "not evaluable").
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-range-exactness.R")'

n_boxes <- 300L
uniform_draws <- 400L
slice_draws <- 400L
set.seed(20261123)

fmt <- function(x, d) formatC(x, format = "f", digits = d)

random_paper <- function(handler) {
  n <- sample(c(3, 4, 5, 8, 12, 20, 40, 100), 1L)
  dec <- sample(0:3, 1L)
  scale <- 10^runif(1L, -0.5, 1.5)
  rho_dec <- sample(1:3, 1L)
  r <- fmt(sample(c(runif(1L, -1, 1), 0.99, 0.999, -0.99, 1, 0), 1L), rho_dec)
  r <- fmt(max(-1, min(1, as.numeric(r))), rho_dec)
  t_dec <- sample(0:2, 1L)
  paper <- list(n = as.character(n), r = r, t = fmt(rnorm(1L) * 4, t_dec),
                df = as.character(n - if (handler == "t_paired") 1 else 2),
                m = c(fmt(rnorm(1L) * scale, dec), fmt(rnorm(1L) * scale, dec)),
                sd = fmt(abs(rnorm(2L)) * scale + 0.02, dec))
  paper$sd <- c(paper$sd[1L], paper$sd[2L])
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
  point
}

rows <- list()
escaped <- 0L
for (handler in c("t_paired", "cor_pearson")) {
  rels <- get_handler(handler)$relations
  for (b in seq_len(n_boxes)) {
    box <- box_of(random_paper(handler))
    for (rel in rels) {
      for (variant in if (is.null(rel$variants)) list(NULL) else rel$variants) {
        range <- tryCatch(relation_range(rel, box, variant),
                          error = function(e) c(NA_real_, NA_real_))
        key <- paste(handler, rel$name, variant)
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
        escaped <- escaped + bad
        rows[[key]] <- rbind(rows[[key]], c(1, bad, 0))
      }
    }
  }
}
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

case_names <- c("t_from_descriptives (sd1 1.95-2.05, r .99)",
                "p_from_t two.sided (t = 0.00, df 19)")
corner_upper <- c(relation_range(corners_t, box_t)[["upper"]],
                  relation_range(corners_p, box_p, "two.sided")[["upper"]])
true_upper <- c(relation_range(rel_t, box_t)[["upper"]],
                relation_range(rel_p, box_p, "two.sided")[["upper"]])
named <- data.frame(case = case_names, corner_only_upper = corner_upper,
                    true_upper = true_upper, scan_upper = c(max(scan_t), 1))
named[, -1L] <- round(named[, -1L], 3L)
cat("\n")
print(named, row.names = FALSE)

named_ok <- all(named$corner_only_upper < named$true_upper - 1e-3) &&
  isTRUE(all.equal(named$true_upper, named$scan_upper, tolerance = 1e-3))
pass <- escaped == 0L && named_ok
cat(if (pass) "\nPASS" else "\nFAIL", ": escaped samples = ", escaped,
    ", named cases show corner-only < true = ", named_ok, "\n", sep = "")
