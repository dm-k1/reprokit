test_that("Student and Welch reproduce t.test for every alternative", {
  d <- data.frame(y = c(sleep$extra, 3.1), g = c(as.character(sleep$group), "2"))
  for (test in c("student", "welch")) {
    for (alt in c("two.sided", "less", "greater")) {
      k <- sufficient_stats(y ~ g, data = d, test = test, alternative = alt, conf_level = 0.9)
      expect_reproduces(
        t_independent_reported(y ~ g, d, test, alternative = alt, conf_level = 0.9),
        reproduce(k)
      )
    }
  }
})

test_that("the summary stores per-group n, mean and SD only", {
  k <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  expect_named(k$stats, c("n", "mean", "sd"))
  expect_identical(unname(k$stats$n), c(10L, 10L))
})

test_that("more or fewer than two groups is refused", {
  d <- data.frame(y = 1:15 + 0.5, g = rep(c("a", "b", "c"), 5))
  s <- sufficient_stats(y ~ g, data = d, test = "welch")
  expect_identical(s$verdict, "needs_raw_data")
  expect_error(reproduce(s), class = "reprokit_impossible")
})

test_that("Student and Welch agree with t.test() on unequal groups, one- and two-sided", {
  x <- c(5.1, 4.8, 6.2, 5.9, 5.5, 6.8, 4.9)
  y <- c(4.2, 3.9, 5.1, 4.4, 6.0, 3.8, 4.7, 4.1, 5.2, 4.6)
  d <- data.frame(v = c(x, y), g = rep(c("a", "b"), c(length(x), length(y))))
  for (test in c("student", "welch")) {
    for (alt in c("two.sided", "less", "greater")) {
      expected <- stats::t.test(x, y, var.equal = test == "student", alternative = alt)
      got <- reproduce(sufficient_stats(v ~ g, data = d, test = test, alternative = alt))
      expect_equal(unname(got[c("t", "df", "p")]),
                   unname(c(expected$statistic, expected$parameter, expected$p.value)),
                   info = paste(test, alt))
      expect_equal(unname(got[c("ci_lower", "ci_upper")]), expected$conf.int[1:2],
                   info = paste(test, alt))
    }
  }
})

test_that("an unknown alternative is refused and the formula must be outcome ~ group", {
  d <- data.frame(v = 1:8 + 0.5, g = rep(c("a", "b"), 4), h = rep(c("u", "v"), each = 4))
  expect_error(t_independent_summarise(v ~ g, d, "welch", alternative = "bigger"))
  expect_error(t_independent_summarise(v ~ g + h, d, "welch"), class = "reprokit_error_input")
})

test_that("the disclosure check flags only groups below the threshold", {
  disclosure <- get_handler("t_independent")$disclosure
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  expect_true(disclosure(s, 10)$passed)
  flagged <- disclosure(s, 11)
  expect_false(flagged$passed)
  expect_length(flagged$problems, 2L)
  expect_match(flagged$problems[1], "n = 10 < 11", fixed = TRUE)
})
