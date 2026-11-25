ucb <- function() {
  d <- as.data.frame(margin.table(UCBAdmissions, c(1, 2)))
  d[rep(seq_len(nrow(d)), d$Freq), c("Admit", "Gender")]
}
ucb_table <- function() unclass(margin.table(UCBAdmissions, c(1, 2)))
cyl_data <- function() data.frame(cyl = mtcars$cyl)

chisq_stats <- function(...) {
  sufficient_stats(..., threshold = 1)
}

test_that("the independence handler reproduces chisq.test(), with and without Yates", {
  for (correct in c(TRUE, FALSE)) {
    s <- chisq_stats(~ Admit + Gender, data = ucb(), test = "chisq", correct = correct)
    expect_reproduces(chisq_independence_reported(~ Admit + Gender, ucb(), "chisq", correct),
                      reproduce(s))
  }
  hec <- as.data.frame(margin.table(HairEyeColor, c(1, 2)))
  d <- hec[rep(seq_len(nrow(hec)), hec$Freq), c("Hair", "Eye")]
  s <- chisq_stats(~ Hair + Eye, data = d, test = "chisq")
  expect_reproduces(chisq_independence_reported(~ Hair + Eye, d, "chisq"), reproduce(s))
  expect_equal(unname(reproduce(s)["df"]), 9)
})

test_that("the goodness-of-fit handler reproduces chisq.test() for uniform and given p0", {
  s <- chisq_stats(~ cyl, data = cyl_data(), test = "gof")
  expect_reproduces(chisq_gof_reported(~ cyl, cyl_data(), "gof"), reproduce(s))
  p0 <- c(0.3, 0.2, 0.5)
  s2 <- chisq_stats(~ cyl, data = cyl_data(), test = "gof", p0 = p0)
  expect_reproduces(chisq_gof_reported(~ cyl, cyl_data(), "gof", p0 = p0), reproduce(s2))
  expect_equal(unname(reproduce(s2)["chisq"]),
               unname(chisq.test(table(mtcars$cyl), p = p0)$statistic))
})

test_that("the summary stores the flat table, the shape and the H0 proportions", {
  s <- chisq_stats(~ Admit + Gender, data = ucb(), test = "chisq")
  expect_named(s$stats, "n")
  expect_equal(s$stats$n, as.vector(ucb_table()))
  expect_equal(s$meta$dims, c(2L, 2L))
  g <- chisq_stats(~ cyl, data = cyl_data(), test = "gof")
  expect_equal(g$stats$p0, rep(1 / 3, 3))
})

test_that("a bad p0 or a one-category variable is refused", {
  bad <- list(
    list(chisq_stats(~ cyl, data = cyl_data(), test = "gof", p0 = c(0.5, 0.5)),
         "one proportion per category"),
    list(chisq_stats(~ cyl, data = cyl_data(), test = "gof", p0 = c(0.5, 0.5, 0.5)),
         "sum to 1"),
    list(chisq_stats(~ cyl, data = data.frame(cyl = rep(4, 5)), test = "gof"), "at least 2"))
  for (b in bad) {
    expect_identical(b[[1L]]$verdict, "needs_raw_data")
    expect_match(b[[1L]]$verdict_reason, b[[2L]])
    expect_error(reproduce(b[[1L]]), class = "reprokit_impossible")
  }
})

test_that("an empty row gives chi-squared that is not evaluable, not a failure", {
  expect_true(is.nan(chisq_table_stat(c(5, 0, 7, 0), c(2, 2))))
  expect_true(is.nan(chisq_gof_stat(c(5, 0), c(1, 0))))
  r <- check_reported("chisq", n = matrix(c(5, 0, 7, 0), 2), chisq = "1.00", df = "1",
                      p = ".32")
  expect_true(r$consistent)
})

test_that("low expected counts are listed as lost, not checked", {
  expect_match(paste(get_handler("chisq_independence")$lost, collapse = " "),
               "expected counts below 5")
})

test_that("a cell below the threshold refuses the verdict", {
  d <- data.frame(a = c(rep("x", 3), rep("y", 40)), b = c("u", "v", "v", rep(c("u", "v"), 20)))
  s <- sufficient_stats(~ a + b, data = d, test = "chisq", threshold = 5)
  expect_identical(s$verdict, "needs_raw_data")
  expect_match(s$verdict_reason, "n = 1 < 5")
  expect_error(reproduce(s), class = "reprokit_impossible")
})
