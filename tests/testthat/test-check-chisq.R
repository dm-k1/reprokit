tab22 <- function() matrix(c("30", "10", "20", "40"), 2)

test_that("a matrix n is read as a table and a correct Yates report is accepted", {
  r <- check_chisq(n = tab22(), chisq = "15.04", df = "1", p = ".0001", v = ".41")
  expect_true(r$consistent)
  expect_identical(r$variants$test$fits[r$variants$test$variant == "yates"], TRUE)
  expect_identical(r$variants$test$fits[r$variants$test$variant == "pearson"], FALSE)
})

test_that("Pearson chi-squared fits Pearson only", {
  r <- check_chisq(n = tab22(), chisq = "16.67", df = "1", p = "<.001")
  expect_true(r$consistent)
  expect_identical(r$variants$test$fits, c(TRUE, FALSE))
})

test_that("a Yates chi-squared with a Pearson p is inconsistent", {
  r <- check_chisq(n = tab22(), chisq = "15.04", p = "<.001")
  expect_true(r$consistent) # both large; p < .001 holds for either
  r2 <- check_chisq(n = tab22(), chisq = "15.04", p = ".00005")
  expect_false(r2$consistent)
})

test_that("larger tables have no Yates variant", {
  m <- matrix(c(10, 20, 30, 15, 25, 5), 2)
  r <- check_chisq(n = m, chisq = format(round(chisq.test(m)$statistic, 2), nsmall = 2))
  expect_null(r$variants$test)
  expect_true(r$consistent)
})

test_that("df is checked only when the shape is known", {
  v <- c("30", "10", "20", "40")
  expect_false(check_chisq(n = tab22(), chisq = "16.67", df = "3")$consistent)
  r <- check_chisq(n = v, chisq = "16.67", df = "3", handler = "chisq_independence")
  expect_true(r$consistent)
  expect_false(check_chisq(n = v, chisq = "16.67", df = "3", dims = c(2, 2),
                           handler = "chisq_independence")$consistent)
})

test_that("df from the wrong shape is caught", {
  expect_false(check_chisq(n = tab22(), chisq = "16.67", df = "2")$consistent)
})

test_that("V is a convention, never a falsifier, and phi is compared by absolute value", {
  r <- check_chisq(n = tab22(), chisq = "16.67", v = ".99")
  expect_true(r$consistent)
  expect_identical(r$numbers$status[r$numbers$quantity == "v"], "not reproducible")
  neg <- check_chisq(n = tab22(), chisq = "16.67", v = "-.41")
  expect_identical(neg$numbers$status[neg$numbers$quantity == "v"], "verified")
  expect_match(paste(neg$notes, collapse = " "), "absolute value")
})

test_that("goodness of fit takes a vector, df needs dims, p0 is validated", {
  n <- c("14", "7", "11")
  r <- check_chisq(n = n, chisq = "2.31", df = "2", p = ".32")
  expect_identical(r$handler, "chisq_gof")
  expect_true(r$consistent)
  expect_error(check_chisq(n = n, chisq = "2.31", p0 = c(0.5, 0.5)), "one proportion")
  expect_error(check_chisq(n = n, chisq = "2.31", p0 = c(0.5, 0.5, 0.5)), "sum to 1")
  g <- check_chisq(n = c("32", "11", "7"), p0 = c(0.5, 0.25, 0.25),
                   chisq = format(round(chisq.test(c(32, 11, 7), p = c(0.5, 0.25, 0.25))$statistic, 2),
                                  nsmall = 2))
  expect_true(g$consistent)
  expect_equal(g$level, 3)
})

test_that("a correct w above 1 is not flagged", {
  n <- c(97, 1, 1, 1)
  chisq <- unname(chisq.test(n)$statistic)
  w <- sqrt(chisq / sum(n))
  expect_gt(w, 1)
  r <- check_chisq(n = as.character(n), chisq = sprintf("%.2f", chisq), df = "3",
                   v = sprintf("%.2f", w))
  expect_true(r$consistent)
})

test_that("dims and a matrix must agree; undeclared arguments are errors", {
  expect_error(check_chisq(n = tab22(), chisq = "16.67", dims = c(3, 3)), "does not match")
  expect_error(check_reported("t_paired", n = "20", t = "2.85", dims = c(2, 2)),
               "no test argument")
  expect_error(check_reported("chisq_independence", n = "5", mu = 1), "no test argument")
})

test_that("a changed count or a changed digit is caught", {
  expect_false(check_chisq(n = matrix(c("31", "10", "20", "40"), 2),
                           chisq = "15.04", p = ".0001")$consistent)
  expect_false(check_chisq(n = tab22(), chisq = "15.84", df = "1")$consistent)
})

test_that("empty cells are fine and an empty margin is not evaluable", {
  m <- matrix(c("12", "0", "8", "5"), 2)
  expect_s3_class(check_chisq(n = m, chisq = "3.00"), "reprokit_check")
  r <- check_chisq(n = matrix(c("5", "0", "7", "0"), 2), chisq = "9.99")
  expect_true(r$consistent)
})

test_that("chisq without a table does not partial-match n_total (regression)", {
  expect_no_error(check_chisq(chisq = "15.04", v = ".41", n_total = "100", dims = c(2, 2)))
})
