test_that("Student and Welch reproduce t.test for every alternative", {
  d <- data.frame(y = c(sleep$extra, 3.1), g = c(as.character(sleep$group), "2"))
  for (test in c("student", "welch")) {
    for (alt in c("two.sided", "less", "greater")) {
      k <- kit(y ~ g, data = d, test = test, alternative = alt, conf_level = 0.9,
               sharing_reason = "test")
      expect_reproduces(
        t_independent_reported(y ~ g, d, test, alternative = alt, conf_level = 0.9),
        refit(k)
      )
    }
  }
})

test_that("the kit stores per-group n, mean and SD only", {
  k <- kit(extra ~ group, data = sleep, test = "welch", sharing_reason = "test")
  expect_named(k$stats, c("n", "mean", "sd"))
  expect_identical(unname(k$stats$n), c(10L, 10L))
})

test_that("more or fewer than two groups is refused", {
  d <- data.frame(y = 1:15 + 0.5, g = rep(c("a", "b", "c"), 5))
  expect_identical(triage(y ~ g, data = d, test = "welch")$verdict, "needs_raw_data")
  expect_error(kit(y ~ g, data = d, test = "welch", sharing_reason = "test"),
               class = "reprokit_impossible")
})
