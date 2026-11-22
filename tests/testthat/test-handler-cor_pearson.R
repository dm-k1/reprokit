test_that("Pearson reproduces cor.test for every alternative", {
  for (alt in c("two.sided", "less", "greater")) {
    s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson", alternative = alt,
                          conf_level = 0.9, sharing_reason = "test")
    expect_reproduces(
      cor_pearson_reported(~ mpg + wt, mtcars, "pearson", alternative = alt, conf_level = 0.9),
      reproduce(s)
    )
  }
})

test_that("the summary stores n, M and SD of both variables, and r", {
  s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson", sharing_reason = "test")
  expect_named(s$stats, c("n", "mean", "sd", "r"))
  expect_named(s$stats$mean, c("mpg", "wt"))
  expect_identical(s$stats$n, 32L)
})

test_that("a formula other than `~ x + y` is refused", {
  expect_identical(triage(mpg ~ wt, data = mtcars, test = "pearson")$verdict, "needs_raw_data")
  expect_error(sufficient_stats(~ mpg + wt + hp, data = mtcars, test = "pearson",
                                sharing_reason = "test"),
               class = "reprokit_impossible")
})
