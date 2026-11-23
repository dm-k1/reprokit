product <- function() {
  relation("product", c("n", "r"), "t", function(vals, variant) vals$n * vals$r)
}

test_that("relation_range evaluates every corner of a known box", {
  box <- list(n = list(lower = 2, upper = 3), r = list(lower = -1, upper = 4))
  expect_equal(relation_range(product(), box), c(lower = -3, upper = 12))
})

test_that("exact inputs add no dimension and vector quantities add one per value", {
  diff_rel <- relation("diff", "m", "t", function(vals, variant) vals$m[1] - vals$m[2])
  box <- list(m = list(lower = c(1, 2), upper = c(1.5, 2.5)))
  expect_equal(relation_range(diff_rel, box), c(lower = -1.5, upper = -0.5))
  expect_equal(relation_range(product(), list(n = list(lower = 5, upper = 5),
                                              r = list(lower = 1, upper = 2))),
               c(lower = 5, upper = 10))
})

test_that("more than 2^14 corners is refused", {
  wide <- relation("wide", "m", "t", function(vals, variant) sum(vals$m))
  box <- list(m = list(lower = rep(0, 15), upper = rep(1, 15)))
  expect_error(relation_range(wide, box), "2\\^14")
})

test_that("a missing input is refused", {
  expect_error(relation_range(product(), list(n = list(lower = 1, upper = 1))), "needs r")
})

test_that("stationary points of a non-monotone relation extend the corner range", {
  square <- relation("square", "r", "t", function(vals, variant) vals$r^2,
                     stationary = function(intervals) list(list(r = 0)))
  box <- list(r = list(lower = -1, upper = 2))
  expect_equal(relation_range(square, box), c(lower = 0, upper = 4))
  without <- square
  without$stationary <- NULL
  expect_equal(relation_range(without, box), c(lower = 1, upper = 4))
})

test_that("relation() rejects unknown quantity names", {
  expect_error(relation("bad", "x", "t", function(vals, variant) 1), "Unknown quantity")
})

# Boxes of reported numbers, one entry per handler: values as a paper would
# print them. The second t_paired box is built so the corners alone fail: sd[1]
# can sit at r * sd[2] inside its interval (t peaks there), and the interval of
# t straddles 0 (the two-sided p of 1 lies inside it).
sample_boxes <- list(
  list(handler = "t_paired",
       paper = list(n = "20", m = c("5.10", "4.20"), sd = c("1.80", "2.00"), r = "0.500",
                    t = "2.85", df = "19")),
  list(handler = "t_paired",
       paper = list(n = "20", m = c("5.0", "4.0"), sd = c("2.0", "2.0"), r = "0.99",
                    t = "0.0", df = "19")),
  list(handler = "cor_pearson",
       paper = list(n = "32", r = "0.400", t = "2.50", df = "30")),
  # Small n, r near +-1 and sd near 0, where a rounding box meets the edge of a
  # function's domain (the boxes are clamped, as check_reported() does).
  list(handler = "cor_pearson",
       paper = list(n = "4", r = "0.99", t = "7.00", df = "2")),
  list(handler = "cor_pearson",
       paper = list(n = "5", r = "-0.999", t = "-39.0", df = "3")),
  list(handler = "t_paired",
       paper = list(n = "3", m = c("5.0", "4.0"), sd = c("0.01", "0.02"), r = "0.90",
                    t = "5.0", df = "2")),
  list(handler = "t_paired",
       paper = list(n = "10", m = c("5", "4"), sd = c("0.00", "2.00"), r = "-0.99",
                    t = "1.0", df = "9"))
)

box_of <- function(paper) {
  Map(function(x, quantity) {
    restrict_to_domain(interval_bounds(reported_value(x)), quantity, x)
  }, paper, names(paper))
}

# Output of `rel` at `draws` uniform random points of the box.
sampled_outputs <- function(rel, box, variant, draws) {
  points <- lapply(seq_len(draws), function(i) {
    lapply(box[rel$inputs], function(b) {
      b$lower + stats::runif(length(b$lower)) * (b$upper - b$lower)
    })
  })
  vapply(points, function(point) rel$fn(point, variant), numeric(1))
}

expect_range_contains <- function(range, sampled, info) {
  slack <- 1e-9 * max(1, abs(range))
  testthat::expect_gte(min(sampled), range[["lower"]] - slack, label = paste(info, "sampled min"))
  testthat::expect_lte(max(sampled), range[["upper"]] + slack, label = paste(info, "sampled max"))
}

test_that("relation_range contains a dense random sample for every registered relation", {
  set.seed(1)
  for (spec in sample_boxes) {
    box <- box_of(spec$paper)
    for (rel in get_handler(spec$handler)$relations) {
      for (variant in if (is.null(rel$variants)) list(NULL) else rel$variants) {
        range <- relation_range(rel, box, variant)
        sampled <- sampled_outputs(rel, box, variant, 20000L)
        # Where the relation is not finite the range is not evaluable and
        # check_reported() skips it; the containment claim is for finite ranges.
        if (!all(is.finite(range))) next
        expect_range_contains(range, sampled[is.finite(sampled)],
                              paste(spec$handler, rel$name, variant))
      }
    }
  }
})

test_that("t_from_descriptives has an interior maximum that the corners miss", {
  rel <- get_handler("t_paired")$relations[[2L]]
  expect_equal(rel$name, "t_from_descriptives")
  # Only sd[1] is uncertain, so a dense scan over it is the true range. r * sd[2]
  # = 1.98 lies inside [1.95, 2.05].
  box <- list(n = list(lower = 20, upper = 20),
              m = list(lower = c(5, 4), upper = c(5, 4)),
              sd = list(lower = c(1.95, 2), upper = c(2.05, 2)),
              r = list(lower = 0.99, upper = 0.99))
  scan <- vapply(seq(1.95, 2.05, length.out = 100001L), function(sd1) {
    rel$fn(list(n = 20, m = c(5, 4), sd = c(sd1, 2), r = 0.99), NULL)
  }, numeric(1))
  corners_only <- rel
  corners_only$stationary <- NULL
  corner_range <- relation_range(corners_only, box)
  full_range <- relation_range(rel, box)
  expect_lt(corner_range[["upper"]], max(scan) - 0.05)
  expect_equal(full_range[["upper"]], max(scan), tolerance = 1e-8)
  expect_equal(full_range[["lower"]], min(scan), tolerance = 1e-12)
})

test_that("two-sided p reaches 1 when the interval of t straddles 0", {
  rel <- get_handler("t_paired")$relations[[3L]]
  expect_equal(rel$name, "p_from_t")
  box <- list(t = interval_bounds(reported_value("0.00")),
              df = list(lower = 19, upper = 19))
  corners_only <- rel
  corners_only$stationary <- NULL
  corner_range <- relation_range(corners_only, box, "two.sided")
  full_range <- relation_range(rel, box, "two.sided")
  expect_lt(corner_range[["upper"]], 0.997)
  expect_equal(full_range[["upper"]], 1)
  shifted <- list(t = interval_bounds(reported_value("0.50")), df = box$df)
  expect_equal(relation_range(rel, shifted, "two.sided"),
               relation_range(corners_only, shifted, "two.sided"))
})

test_that("solve_for recovers a known r", {
  rel <- get_handler("t_paired")$relations[[2L]]
  expect_equal(rel$name, "t_from_descriptives")
  n <- 30
  m <- c(5.1, 4.3)
  sd <- c(1.8, 2)
  true_r <- 0.6
  se <- sqrt(sd[1]^2 + sd[2]^2 - 2 * true_r * sd[1] * sd[2]) / sqrt(n)
  t_reported <- formatC((m[1] - m[2]) / se, format = "f", digits = 2)
  intervals <- list(n = list(lower = n, upper = n),
                    m = interval_bounds(reported_value(c("5.10", "4.30"))),
                    sd = interval_bounds(reported_value(c("1.80", "2.00"))))
  found <- solve_for(rel, intervals, "r", reported_value(t_reported), c(-1, 1))
  expect_true(found$feasible)
  expect_gte(true_r, found$lower - found$resolution)
  expect_lte(true_r, found$upper + found$resolution)
})

test_that("solve_for reports infeasible when no value reaches the reported output", {
  rel <- get_handler("t_paired")$relations[[2L]]
  intervals <- list(n = list(lower = 30, upper = 30),
                    m = interval_bounds(reported_value(c("5.10", "5.09"))),
                    sd = interval_bounds(reported_value(c("1.80", "2.00"))))
  found <- solve_for(rel, intervals, "r", reported_value("50.00"), c(-1, 1))
  expect_false(found$feasible)
  expect_true(is.na(found$lower))
})

test_that("register_handler validates the new fields", {
  expect_error(register_handler("bad", "f", "a", "needs_raw_data", "n", lost = "x",
                                matches = function(x, ...) FALSE,
                                design = apa_design("tstat", "d")),
               "Unknown quantity name in `design`")
  expect_error(register_handler("bad", "f", "a", "needs_raw_data", "n", lost = "x",
                                matches = function(x, ...) FALSE, apa = c("n", "t")),
               "unused argument")
  expect_error(register_handler("bad", "f", "a", "needs_raw_data", "n", lost = "x",
                                matches = function(x, ...) FALSE, relations = list(1)))
})
