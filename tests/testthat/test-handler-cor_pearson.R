test_that("Pearson reproduces cor.test for every alternative", {
  for (alt in c("two.sided", "less", "greater")) {
    s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson", alternative = alt,
                          conf_level = 0.9)
    expect_reproduces(
      cor_pearson_reported(~ mpg + wt, mtcars, "pearson", alternative = alt, conf_level = 0.9),
      reproduce(s)
    )
  }
})

test_that("the summary stores n, M and SD of both variables, and r", {
  s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson")
  expect_named(s$stats, c("n", "mean", "sd", "r"))
  expect_named(s$stats$mean, c("mpg", "wt"))
  expect_identical(s$stats$n, 32L)
})

test_that("a formula other than `~ x + y` is refused", {
  expect_identical(sufficient_stats(mpg ~ wt, data = mtcars, test = "pearson")$verdict,
                   "needs_raw_data")
  s <- sufficient_stats(~ mpg + wt + hp, data = mtcars, test = "pearson")
  expect_identical(s$verdict, "needs_raw_data")
  expect_error(reproduce(s), class = "reprokit_impossible")
})

test_that("check_reported never errors or warns for |r| = 1", {
  for (r in c("1.00", "-1.00", "0.999")) {
    for (n in c("3", "4", "5", "10")) {
      expect_no_error(checked <- check_reported("pearson", r = r, n = n))
      expect_no_warning(check_reported("pearson", r = r, n = n))
      expect_s3_class(checked, "reprokit_check")
      expect_false(any(checked$numbers$status == "inconsistent"), info = paste(r, n))
      expect_output(print(checked))
    }
  }
  expect_no_warning(checked <- check_reported("pearson", r = "1.00", n = "10", t = "5.2",
                                              p = "< .001", df = "8"))
  expect_true(checked$consistent)
})
