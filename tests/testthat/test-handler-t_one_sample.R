test_that("the one-sample t-test reproduces t.test for every alternative", {
  for (alt in c("two.sided", "less", "greater")) {
    s <- sufficient_stats(extra ~ 1, data = sleep, test = "one_sample", mu = 0.3,
                          alternative = alt, conf_level = 0.9, sharing_reason = "test")
    expect_reproduces(
      t_one_sample_reported(extra ~ 1, sleep, "one_sample", mu = 0.3, alternative = alt,
                            conf_level = 0.9),
      reproduce(s)
    )
  }
})

test_that("the summary stores n, mean and SD only", {
  s <- sufficient_stats(extra ~ 1, data = sleep, test = "one_sample", sharing_reason = "test")
  expect_named(s$stats, c("n", "mean", "sd"))
  expect_identical(s$meta$mu, 0)
})

test_that("a formula other than `y ~ 1` is refused", {
  expect_identical(triage(extra ~ group, data = sleep, test = "one_sample")$verdict,
                   "needs_raw_data")
  expect_error(sufficient_stats(extra ~ group, data = sleep, test = "one_sample",
                                sharing_reason = "test"),
               class = "reprokit_impossible")
})
