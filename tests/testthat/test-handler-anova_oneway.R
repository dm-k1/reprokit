oneway_data <- function() {
  data.frame(y = c(PlantGrowth$weight, 5.2, 4.1),
             g = c(as.character(PlantGrowth$group), "trt1", "ctrl"))
}

paper_oneway <- function(d = datasets::PlantGrowth, digits = 2L) {
  r <- anova_oneway_reported(weight ~ group, d, "oneway")
  # nolint start: object_usage_linter.
  list(f = fixed(r[["f"]], digits), df1 = as.character(r[["df1"]]), df2 = as.character(r[["df2"]]),
       p = fixed(r[["p"]], 3), eta2 = fixed(r[["eta2"]], 2))
  # nolint end
}

test_that("the one-way handler reproduces oneway.test() and aov()", {
  for (d in list(datasets::PlantGrowth, oneway_data())) {
    f <- if ("weight" %in% names(d)) weight ~ group else y ~ g
    s <- sufficient_stats(f, data = d, test = "oneway")
    expect_reproduces(anova_oneway_reported(f, d, "oneway"), reproduce(s))
  }
})

test_that("the summary stores per-group n, mean and SD at full precision", {
  s <- sufficient_stats(weight ~ group, data = PlantGrowth, test = "oneway")
  expect_named(s$stats, c("n", "mean", "sd"))
  expect_equal(unname(s$stats$mean), unname(tapply(PlantGrowth$weight, PlantGrowth$group, mean)),
               tolerance = 0)
  expect_identical(unname(s$stats$n), c(10L, 10L, 10L))
})

test_that("a group of one observation or a single group is refused", {
  d <- data.frame(y = c(1, 2, 3, 4, 5), g = c("a", "a", "a", "a", "b"))
  s <- sufficient_stats(y ~ g, data = d, test = "oneway")
  expect_identical(s$verdict, "needs_raw_data")
  expect_error(reproduce(s), class = "reprokit_impossible")
  d1 <- data.frame(y = 1:6 + 0.5, g = "a")
  s1 <- sufficient_stats(y ~ g, data = d1, test = "oneway")
  expect_identical(s1$verdict, "needs_raw_data")
  expect_error(reproduce(s1), class = "reprokit_impossible")
})

test_that("check_reported(\"oneway\") accepts a correct report and flags a wrong df", {
  paper <- paper_oneway()
  ok <- do.call(check_reported, c(list(test = "oneway"), paper))
  expect_true(all(ok$numbers$status != "inconsistent"))
  bad <- do.call(check_reported, c(list(test = "oneway"), replace(paper, "df2", "20")))
  expect_true(any(bad$numbers$status == "inconsistent"))
  bad_f <- do.call(check_reported, c(list(test = "oneway"), replace(paper, "f", "6.85")))
  expect_true(any(bad_f$numbers$status == "inconsistent"))
})

test_that("check_reported(\"oneway\") with group descriptives reaches F and eta2 from the data", {
  d <- datasets::PlantGrowth
  paper <- c(paper_oneway(),
             list(n = rep("10", 3), m = fixed(tapply(d$weight, d$group, mean), 2),
                  sd = fixed(tapply(d$weight, d$group, sd), 2)))
  ok <- do.call(check_reported, c(list(test = "oneway"), paper))
  expect_true(all(ok$numbers$status != "inconsistent"))
  paper$m <- c("5.03", "4.66", "6.53")
  bad <- do.call(check_reported, c(list(test = "oneway"), paper))
  expect_true(any(bad$numbers$status == "inconsistent"))
})

test_that("per-group vectors of different length or length 1 are refused", {
  expect_error(check_reported("oneway", m = c("5.0", "5.1", "5.2"), sd = c("1.0", "1.1"),
                              f = "1.00"),
               "same number of values")
  expect_error(check_reported("oneway", m = "5.0", f = "1.00"), "at least 2")
})

test_that("the one-way F range is exact: overlap gives 0, corners alone do not", {
  intervals <- list(n = reported_value(c("12", "15", "10")),
                    m = reported_value(c("5", "5.3", "5.31"), as_count = FALSE),
                    sd = reported_value(c("1.0", "1.2", "1.1"), as_count = FALSE))
  rel <- anova_oneway_relations()[[3L]]
  range <- relation_range(rel, intervals)
  expect_equal(unname(range[["lower"]]), 0)
  expect_gt(unname(range[["upper"]]), 0)
})

test_that("SSb minimum matches optimize() over random boxes", {
  set.seed(1)
  for (i in 1:50) {
    k <- sample(2:5, 1)
    n <- sample(3:30, k, replace = TRUE)
    lo <- stats::runif(k, 0, 5)
    hi <- lo + stats::runif(k, 0, 1)
    g <- function(c) sum(n * pmax(lo - c, 0, c - hi)^2)
    opt <- stats::optimize(g, c(min(lo), max(hi)), tol = 1e-12)$objective
    expect_lte(oneway_ssb_min(n, lo, hi), opt + 1e-9)
    expect_gte(oneway_ssb_min(n, lo, hi), opt - 1e-6)
  }
})

test_that("eta2 relation offers eta2 and omega2, never falsifies", {
  rel <- anova_oneway_relations()[[5L]]
  expect_false(rel$falsifies)
  expect_equal(rel$variants$convention, c("eta2", "omega2"))
  vals <- list(f = 4.846, df1 = 2, df2 = 27)
  expect_equal(rel$fn(vals, "eta2"), 4.846 * 2 / (4.846 * 2 + 27))
  expect_equal(rel$fn(vals, "omega2"), 2 * 3.846 / (2 * 4.846 + 28))
})

test_that("relation_p_from_statistic covers t, F and chisq", {
  expect_equal(relation_p_from_statistic("t")$name, "p_from_t")
  f <- relation_p_from_statistic("F")
  expect_equal(f$fn(list(f = 4, df1 = 2, df2 = 20), NULL),
               stats::pf(4, 2, 20, lower.tail = FALSE))
  x <- relation_p_from_statistic("chisq")
  expect_equal(x$fn(list(chisq = 5, df = 2), NULL), stats::pchisq(5, 2, lower.tail = FALSE))
  expect_error(relation_p_from_statistic("z"))
})

test_that("a relation with a range skips corners", {
  rel <- relation("wide", "m", "t", function(vals, variant) sum(vals$m),
                  range = function(intervals, variant) c(lower = -1, upper = 7))
  big <- list(m = reported_value(rep("1.0", 20), as_count = FALSE))
  expect_equal(unname(relation_range(rel, big)), c(-1, 7))
  expect_error(relation("bad", "m", "t", function(vals, variant) 1,
                        range = function(x) 1))
})

test_that("apa_design drops the CI quantities with ci = FALSE and takes NA cells", {
  d <- apa_design("f", "eta2", cells = NA_integer_, ci = FALSE)
  expect_true(is_apa_design(d))
  expect_equal(apa_quantities(d), c("n", "m", "sd", "f", "df1", "df2", "p", "eta2"))
  expect_true("ci_lower" %in% apa_quantities(apa_design("t", "d", cells = 2L)))
})

test_that("the one-way appendix prints F(df1, df2) and no CI row", {
  s <- sufficient_stats(weight ~ group, data = PlantGrowth, test = "oneway")
  text <- paste(appendix(s, sharing_reason = "Consent did not cover public sharing.",
                         choices = list(convention = "eta2")), collapse = "\n")
  expect_match(text, "F(2, 27)", fixed = TRUE)
  expect_false(grepl("CI", text, fixed = TRUE))
  expect_match(text, "| ctrl | trt1 | trt2 |", fixed = TRUE)
})

test_that("a negative omega squared (F < 1) in a correct one-way paper is not flagged", {
  m <- c("50.10", "50.00", "49.90")
  sd <- c("5.00", "5.20", "4.80")
  checked <- check_reported("oneway", n = rep("10", 3), m = m, sd = sd, f = "0.00",
                            df1 = "2", df2 = "27", p = ".996", eta2 = "-.07")
  expect_true(checked$consistent)
  expect_false("inconsistent" %in% checked$numbers$status)
  # F > 1: a negative eta2 is still not a value either convention gives; never an error,
  # never "inconsistent" (the eta2 relation is a convention).
  big <- check_reported("oneway", f = "15.00", df1 = "2", df2 = "27", eta2 = "-.02")
  expect_false("inconsistent" %in% big$numbers$status)
})

test_that("quantity domains cover df1, df2 and eta2", {
  expect_equal(quantity_domain("eta2"), c(0, 1))
  expect_equal(quantity_domain("df1"), c(0, Inf))
  expect_error(check_reported("oneway", f = "4.00", df1 = "2", df2 = "27", eta2 = "1.50"),
               "between -1 and 1")
})
