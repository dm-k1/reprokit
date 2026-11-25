test_that("the removed functions are not exported (ADR 0027, ADR 0029)", {
  exported <- getNamespaceExports("reprokit")
  for (f in c("triage", "reproduce", "check_t", "check_r", "check_F", "check_chisq", "check_lm",
              "check_mediation")) {
    expect_false(f %in% exported, info = f)
  }
  for (f in c("sufficient_stats", "appendix", "check_reported", "coverage_table", "conventions",
              "verdicts", "handlers", "quantity_names", "new_sufficient_stats",
              "is_sufficient_stats")) {
    expect_true(f %in% exported, info = f)
  }
})

test_that("sufficient_stats() returns, and never errors, for each refusal", {
  gl <- glm(am ~ wt, data = mtcars, family = binomial)
  for (s in list(sufficient_stats(gl), sufficient_stats(list()),
                 sufficient_stats(structure(list(), class = "locked")))) {
    expect_s3_class(s, "reprokit_sufficient_stats")
    expect_false(identical(s$verdict, "summaries_suffice"))
    expect_null(s$stats)
    expect_true(nzchar(s$verdict_reason))
    expect_output(print(s), "Verdict")
    expect_error(appendix(s, sharing_reason = "x"), class = "reprokit_impossible")
    expect_error(reproduce(s), class = "reprokit_impossible")
  }
})

test_that("sufficient_stats() returns for each disclosure flag and the print names where", {
  d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g = c(rep("a", 2), rep("b", 9)))
  s <- sufficient_stats(y ~ g, data = d, test = "welch")
  expect_s3_class(s, "reprokit_sufficient_stats")
  expect_false(identical(s$verdict, "summaries_suffice"))
  expect_output(print(s), "a")
  flagged <- data.frame(a = rep(c("x", "y"), c(3, 40)), b = rep(c("u", "v"), 21)[seq_len(43)])
  s2 <- sufficient_stats(~ a + b, data = flagged, test = "chisq")
  expect_s3_class(s2, "reprokit_sufficient_stats")
  expect_output(print(s2), "cell")
  s3 <- sufficient_stats(~ a, data = flagged, test = "gof")
  expect_output(print(s3), "category")
})

test_that("invalid input is an abort_input() error", {
  expect_error(sufficient_stats(extra ~ group, test = "welch"), class = "reprokit_error_input")
  expect_error(sufficient_stats(extra ~ group, data = sleep, test = "nonsense"),
               class = "reprokit_error_input")
})

test_that("check_reported() names the valid values for a handler-style test", {
  expect_error(check_reported("t_independent", t = "2.00", df = "10"), "welch")
  expect_error(check_reported(), class = "reprokit_error_input")
})

test_that("check_reported() does not partial-match `t` onto `test`", {
  a <- check_reported("paired", n = "20", t = "2.85", df = "19", p = ".010")
  expect_identical(check_reported(n = "20", t = "2.85", df = "19", p = ".010", test = "paired"), a)
  expect_identical(check_reported(test = "paired", n = "20", t = "2.85", df = "19", p = ".010"), a)
  b <- check_reported(t = "2.85", "paired", n = "20", df = "19", p = ".010")
  by_q <- function(x) x$numbers[order(x$numbers$quantity), ]
  expect_equal(by_q(b), by_q(a), ignore_attr = TRUE)
  expect_identical(b$level, a$level)
})

test_that("the other exported functions have no partial-matching hazard on their dots", {
  # appendix() and sufficient_stats() take `...` before their named arguments, so
  # abbreviations of later arguments never match.
  for (f in list(appendix, sufficient_stats)) {
    fm <- names(formals(f))
    expect_identical(fm[1], "x")
    expect_identical(fm[2], "...")
  }
})
