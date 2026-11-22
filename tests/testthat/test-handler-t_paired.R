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

test_that("the summary stores the differences only, not the per-condition statistics", {
  s <- sufficient_stats(Pair(drug1, drug2) ~ 1, data = wide_sleep(), test = "paired",
                        sharing_reason = "test")
  expect_named(s$stats, c("n", "mean_diff", "sd_diff"))
  expect_equal(s$stats$mean_diff, mean(sleep$extra[1:10] - sleep$extra[11:20]))
})

test_that("a formula without Pair() is refused", {
  expect_identical(triage(drug1 ~ 1, data = wide_sleep(), test = "paired")$verdict,
                   "needs_raw_data")
  expect_error(sufficient_stats(drug1 ~ 1, data = wide_sleep(), test = "paired",
                                sharing_reason = "test"),
               class = "reprokit_impossible")
})
