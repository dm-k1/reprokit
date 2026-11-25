# New engine features (variant kinds, falsifies, mu, per-group n) and the
# t_independent / t_one_sample relations, on rounded real t.test() output.
tfix <- function(x, digits) formatC(unname(x), format = "f", digits = digits)
fits <- function(checked, test) {
  checked$variants$test$fits[checked$variants$test$variant == test]
}
tstatus <- function(checked, quantity) {
  checked$numbers$status[checked$numbers$quantity == quantity]
}

paper_independent <- function(var_equal, digits = 2) {
  x <- datasets::sleep$extra[datasets::sleep$group == 1]
  y <- datasets::sleep$extra[datasets::sleep$group == 2]
  result <- stats::t.test(x, y, var.equal = var_equal)
  list(n = as.character(c(length(x), length(y))),
       m = tfix(c(mean(x), mean(y)), digits), sd = tfix(c(sd(x), sd(y)), digits),
       t = tfix(result$statistic, digits), df = tfix(result$parameter, if (var_equal) 0 else 2),
       p = tfix(result$p.value, 3),
       ci_lower = tfix(result$conf.int[1], digits), ci_upper = tfix(result$conf.int[2], digits))
}

paper_one_sample <- function(mu) {
  x <- datasets::sleep$extra
  result <- stats::t.test(x, mu = mu)
  list(n = as.character(length(x)), m = tfix(mean(x), 2), sd = tfix(sd(x), 2),
       t = tfix(result$statistic, 2), df = as.character(result$parameter),
       p = tfix(result$p.value, 3),
       ci_lower = tfix(result$conf.int[1], 2), ci_upper = tfix(result$conf.int[2], 2))
}

check_with <- function(test, paper, ...) {
  do.call(check_reported, c(list(test = test), paper, list(...)))
}
check_with_independent <- function(paper, ...) {
  do.call(check_independent, c(paper, list(...)))
}

test_that("relation() accepts the test and convention kinds and rejects others", {
  f <- function(vals, variant) vals$n
  expect_silent(relation("r1", "n", "df", f, variants = "a", variant_kind = "test"))
  expect_silent(relation("r2", "n", "df", f, variants = "a", variant_kind = "convention",
                         falsifies = FALSE))
  expect_error(relation("r3", "n", "df", f, variants = "a", variant_kind = "nope"))
  expect_false(relation("r4", "n", "df", f, falsifies = FALSE)$falsifies)
  expect_true(relation("r5", "n", "df", f)$falsifies)
})

test_that("Student output from t.test(var.equal = TRUE) is consistent and fits Student", {
  checked <- check_with_independent(paper_independent(TRUE))
  expect_true(checked$consistent)
  expect_true(fits(checked, "student"))
  expect_false(any(checked$numbers$status == "inconsistent"))
})

test_that("Welch output with a fractional df is consistent and fits Welch", {
  paper <- paper_independent(FALSE)
  expect_match(paper$df, "\\.")
  checked <- check_with_independent(paper)
  expect_true(checked$consistent)
  expect_true(fits(checked, "welch"))
  expect_false(fits(checked, "student"))
})

test_that("planted errors in t_independent are flagged", {
  paper <- paper_independent(TRUE)
  bad_t <- modifyList(paper, list(t = "4.50"))
  expect_false(check_with_independent(bad_t)$consistent)
  bad_p <- modifyList(paper, list(p = ".001"))
  expect_false(check_with_independent(bad_p)$consistent)
  bad_df <- modifyList(paper, list(df = "25"))
  expect_false(check_with_independent(bad_df)$consistent)
  bad_m <- modifyList(paper, list(m = c("0.75", "3.33")))
  expect_false(check_with_independent(bad_m)$consistent)
})

test_that("a d that matches no convention is not reproducible, never inconsistent", {
  paper <- paper_independent(TRUE)
  checked <- check_with_independent(c(paper, list(d = "0.10")))
  expect_true(checked$consistent)
  expect_equal(tstatus(checked, "d"), "not reproducible")
  expect_match(checked$numbers$checked_against[checked$numbers$quantity == "d"],
               "no convention tried reproduces it")
  # A d that does match a convention is not flagged either.
  x <- datasets::sleep$extra[datasets::sleep$group == 1]
  y <- datasets::sleep$extra[datasets::sleep$group == 2]
  pooled <- sqrt((9 * var(x) + 9 * var(y)) / 18)
  ok <- check_with_independent(c(paper, list(d = tfix((mean(x) - mean(y)) / pooled, 2))))
  expect_true(ok$consistent)
  expect_false(tstatus(ok, "d") %in% c("inconsistent", "not reproducible"))
})

test_that("Table 7.8 fits Student, not Welch, and its d is not reproducible", {
  checked <- check_independent(n = c("24", "18"), m = c("759", "694"),
                            sd = c("87", "42"), t = "2.877", df = "40", p = ".006", d = "0.840")
  expect_true(checked$consistent)
  expect_true(fits(checked, "student"))
  expect_false(fits(checked, "welch"))
  expect_equal(tstatus(checked, "d"), "not reproducible")
})

test_that("t_independent needs two values for n and m, and one for t", {
  expect_error(check_independent(n = "24", m = c("1", "2")), "needs 2 values")
  expect_error(check_reported(test = "paired", n = c("24", "20")), "needs 1 value")
})

test_that("t_one_sample is consistent for mu = 0 and mu != 0, and flags a wrong mu", {
  expect_true(check_with("one_sample", paper_one_sample(0))$consistent)
  expect_true(check_with("one_sample", paper_one_sample(1), mu = 1)$consistent)
  # t was computed against mu = 1, so checking against mu = 0 must flag it.
  expect_false(check_with("one_sample", paper_one_sample(1))$consistent)
})

test_that("mu must be a finite number and handlers without mu-relations refuse mu != 0", {
  paper <- paper_one_sample(0)
  expect_error(check_with("one_sample", paper, mu = NA), "mu")
  expect_error(check_with("one_sample", paper, mu = "a"), "mu")
  expect_error(check_reported(test = "pearson", n = "32", r = ".5", mu = 1))
})

test_that("check_reported passes mu through", {
  paper <- paper_one_sample(1)
  expect_true(do.call(check_reported, c(list(test = "one_sample"), paper, list(mu = 1)))$consistent)
})

test_that("a non-falsifying relation never makes a number inconsistent", {
  checked <- check_with("one_sample", c(paper_one_sample(0), list(d = "9.99")))
  expect_true(checked$consistent)
  expect_equal(tstatus(checked, "d"), "not reproducible")
})

test_that("a variant that cannot be evaluated makes the relation not evaluable, not narrower", {
  # SDs reported as "0" include 0, where Welch df is 0/0. With Student alone df would
  # be 24 and p = .545 for t = 0.63 would look impossible: a false positive.
  res <- check_independent(n = c("6", "20"), m = c("0", "0"), sd = c("0", "0"),
                        t = "0.63", p = "0.545")
  expect_true(res$consistent)
  expect_false(any(res$numbers$status == "inconsistent"))
})
