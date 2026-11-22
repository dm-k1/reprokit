wide_sleep <- function() {
  data.frame(drug1 = sleep$extra[sleep$group == 1], drug2 = sleep$extra[sleep$group == 2])
}

test_that("the paired t-test reproduces t.test for every alternative", {
  d <- wide_sleep()
  for (alt in c("two.sided", "less", "greater")) {
    s <- sufficient_stats(Pair(drug1, drug2) ~ 1, data = d, test = "paired", mu = -1,
                          alternative = alt, conf_level = 0.9, sharing_reason = "test")
    expect_reproduces(
      t_paired_reported(Pair(drug1, drug2) ~ 1, d, "paired", mu = -1, alternative = alt,
                        conf_level = 0.9),
      reproduce(s)
    )
  }
})

test_that("the summary stores M and SD per condition and r, as a paper reports them", {
  d <- wide_sleep()
  s <- sufficient_stats(Pair(drug1, drug2) ~ 1, data = d, test = "paired",
                        sharing_reason = "test")
  expect_named(s$stats, c("n", "mean", "sd", "r"))
  expect_named(s$stats$mean, c("drug1", "drug2"))
  expect_equal(unname(s$stats$sd), c(sd(d$drug1), sd(d$drug2)))
  expect_equal(s$stats$r, cor(d$drug1, d$drug2))
})

test_that("a formula without Pair() is refused", {
  expect_identical(triage(drug1 ~ 1, data = wide_sleep(), test = "paired")$verdict,
                   "needs_raw_data")
  expect_error(sufficient_stats(drug1 ~ 1, data = wide_sleep(), test = "paired",
                                sharing_reason = "test"),
               class = "reprokit_impossible")
})

test_that("SD_diff is stored too when r is so close to 1 that rebuilding it loses precision", {
  set.seed(3)
  pre <- rnorm(40, 50, 10)
  d <- data.frame(pre = pre, post = pre + rnorm(40, 0.001, 0.0005))
  s <- sufficient_stats(Pair(pre, post) ~ 1, data = d, test = "paired",
                        sharing_reason = "test")
  expect_true("sd_diff" %in% names(s$stats))
  expect_reproduces(t_paired_reported(Pair(pre, post) ~ 1, data = d, test = "paired"),
                    reproduce(s))
  ordinary <- sufficient_stats(Pair(drug1, drug2) ~ 1, data = wide_sleep(), test = "paired",
                               sharing_reason = "test")
  expect_false("sd_diff" %in% names(ordinary$stats))
})
