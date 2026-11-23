# Domain clamping, arity, headline, verified/coherent, apa design, level 2, boundaries.
status_of_domain <- function(checked, quantity) {
  checked$numbers$status[checked$numbers$quantity == quantity]
}

no_false_alarm <- function(handler, ...) {
  checked <- check_reported(handler, ...)
  testthat::expect_false("inconsistent" %in% checked$numbers$status)
  testthat::expect_true(checked$consistent)
  checked
}

test_that("r reported as 1.00, -1.00 or 1.000 is not flagged when |r| is that close to 1", {
  for (r in c("1.00", "-1.00", "1.000", "-1.000")) {
    sign <- if (startsWith(r, "-")) -1 else 1
    x <- seq_len(40)
    y <- sign * x + 1e-3 * sin(x)
    true_r <- stats::cor(x, y)
    expect_gt(abs(true_r), 0.9995)
    result <- stats::cor.test(x, y)
    checked <- no_false_alarm("cor_pearson", n = "40", r = r, df = "38", p = "< .001",
                              ci_lower = formatC(result$conf.int[1], format = "f", digits = 2),
                              ci_upper = formatC(result$conf.int[2], format = "f", digits = 2))
    expect_false(any(is.na(checked$numbers$status)))
  }
})

test_that("Pearson with n = 3 and n = 4 neither errors nor flags", {
  for (n in c(3, 4)) {
    x <- c(1, 2, 4, 7)[seq_len(n)]
    y <- c(2, 1, 5, 6)[seq_len(n)]
    result <- stats::cor.test(x, y)
    checked <- no_false_alarm("cor_pearson", n = as.character(n),
                              r = formatC(result$estimate, format = "f", digits = 2),
                              df = as.character(n - 2),
                              p = formatC(result$p.value, format = "f", digits = 2))
    expect_s3_class(checked, "reprokit_check")
  }
})

test_that("Pearson Fisher CI is not applicable for n < 4, not inconsistent", {
  checked <- no_false_alarm("cor_pearson", n = "3", r = "0.50", ci_lower = "-0.9", ci_upper = "0.9")
  expect_false("inconsistent" %in% checked$numbers$status)
})

test_that("sd = 0 is accepted and a zero-width sd is not flagged", {
  checked <- no_false_alarm("t_paired", n = "10", m = c("5.00", "4.00"), sd = c("0.00", "0.00"),
                            t = "5.00", df = "9")
  expect_s3_class(checked, "reprokit_check")
})

test_that("a wide box from sd = 2 reported to zero decimals is not flagged", {
  d <- datasets::sleep
  first <- d$extra[d$group == 1]
  second <- d$extra[d$group == 2]
  result <- stats::t.test(first, second, paired = TRUE)
  fx <- function(x, digits) formatC(unname(x), format = "f", digits = digits)
  no_false_alarm("t_paired", n = "10", m = fx(c(mean(first), mean(second)), 0),
                 sd = fx(c(sd(first), sd(second)), 0), t = fx(result$statistic, 2),
                 df = "9", p = fx(result$p.value, 3))
})

test_that("p given as '< .001' is handled", {
  checked <- no_false_alarm("cor_pearson", n = "32", r = "-0.868", df = "30", p = "< .001")
  expect_false(status_of_domain(checked, "p") == "inconsistent")
})

test_that("arity errors name the expected length and are input errors", {
  expect_error(check_reported("t_paired", n = "10", m = "5.1", sd = c("1", "1")),
               "`m` needs 2 values.*got 1")
  expect_error(check_reported("t_paired", n = "10", m = c("5.1", "4", "3"), sd = c("1", "1")),
               "`m` needs 2 values.*got 3")
  expect_error(check_reported("t_paired", n = c("20", "20"), m = c("5", "4")),
               "`n` needs 1 value.*got 2")
  expect_error(check_reported("cor_pearson", n = "10", r = c("0.5", "0.6")),
               "`r` needs 1 value")
})

test_that("values outside a quantity's domain are input errors", {
  expect_error(check_reported("cor_pearson", n = "10", r = "1.50"), "between -1 and 1")
  expect_error(check_reported("t_paired", n = "10", p = "1.50"), "between 0 and 1")
})

test_that("a non-finite relation range is never reported as inconsistent", {
  bad <- relation("always_infinite", "n", "t", function(vals, variant) Inf)
  nan <- relation("always_nan", "n", "t", function(vals, variant) NaN)
  for (rel in list(bad, nan)) {
    register_handler("scratch_nonfinite", "test", "scratch", "needs_raw_data", "n",
                     lost = "x", matches = function(x, ...) FALSE, relations = list(rel))
    withr::defer(unregister_handler("scratch_nonfinite"))
    checked <- check_reported("scratch_nonfinite", n = "10", t = "2.00")
    expect_false("inconsistent" %in% checked$numbers$status)
    expect_false(checked$checked)
    unregister_handler("scratch_nonfinite")
  }
})

test_that("the headline follows whether anything was evaluated, not the level", {
  checked <- check_reported("cor_pearson", n = "32", r = "1.00")
  expect_equal(checked$level, 3L)
  expect_false(checked$checked)
  out <- paste(utils::capture.output(print(checked)), collapse = "\n")
  expect_match(out, "No check was possible with the reported statistics")
  expect_match(out, "the sufficient set is reported")
})

test_that("verified is only for the output of a holding relation from descriptives", {
  checked <- check_reported("cor_pearson", n = "32", r = "-0.868", df = "30", p = "< .001",
                            ci_lower = "-0.934", ci_upper = "-0.744")
  expect_equal(status_of_domain(checked, "n"), "coherent")
  expect_equal(status_of_domain(checked, "r"), "coherent")
  for (q in c("p", "df", "ci_lower", "ci_upper")) {
    expect_equal(status_of_domain(checked, q), "verified")
  }
})

test_that("apa is built from the design and cannot be hand-written", {
  paired <- get_handler("t_paired")
  expect_setequal(paired$apa, c("n", "m", "sd", "t", "df", "p", "d", "ci_lower", "ci_upper"))
  pearson <- get_handler("cor_pearson")
  expect_setequal(pearson$apa, c("n", "m", "sd", "r", "df", "p", "ci_lower", "ci_upper"))
  expect_error(register_handler("bad", "f", "a", "needs_raw_data", "n", lost = "x",
                                matches = function(x, ...) FALSE, apa = c("n")),
               "unused argument")
})

test_that("level 2 is unreachable for t_paired and cor_pearson", {
  names <- quantity_names()
  bits <- 2^(seq_along(names) - 1)
  subsets <- lapply(0:(2^length(names) - 1), function(i) names[bitwAnd(i, bits) > 0])
  expect_length(subsets, 2^length(names))
  for (handler in c("t_paired", "cor_pearson")) {
    entry <- get_handler(handler)
    levels <- vapply(subsets, function(s) coverage_level(entry, s), integer(1))
    expect_false(2L %in% levels, info = handler)
  }
})

test_that("solve_for reports when the feasible set reaches the edge of the domain", {
  rel_t <- Filter(function(r) r$output == "t", get_handler("cor_pearson")$relations)[[1L]]
  n_box <- list(n = list(lower = 32, upper = 32))
  # Any t of at least 5 is reached by r near 1, so the set runs to the upper limit.
  found <- solve_for(rel_t, n_box, "r", list(lower = 5, upper = 1e9), c(-1, 1))
  expect_true(found$feasible)
  expect_true(found$reaches_upper)
  expect_false(found$reaches_lower)
  expect_equal(found$upper, 1)
  # A narrow target is interior and reaches neither edge.
  interior <- solve_for(rel_t, n_box, "r", reported_value("2.50"), c(-1, 1))
  expect_false(interior$reaches_upper)
})

test_that("a mean or SD reported without decimals is rounded to a whole unit, not exact", {
  # Found by inst/validation/check-false-positives.R: M = "0", SD = "1" was read
  # as exactly 0 and 1, so a real t of 0.64 was flagged inconsistent.
  expect_equal(reported_value("1", as_count = FALSE)[c("lower", "upper")],
               list(lower = 0.5, upper = 1.5))
  expect_equal(reported_value("15")[c("lower", "upper")], list(lower = 15, upper = 15))
  res <- check_reported("t_paired", n = "15", m = c("0", "0"), sd = c("1", "1"),
                        r = "-0.823", t = "0.64")
  expect_true(res$consistent)
  expect_true(res$checked)
  # Counts stay exact.
  res <- check_reported("t_paired", n = "15", df = "13")
  expect_false(res$consistent)
})
