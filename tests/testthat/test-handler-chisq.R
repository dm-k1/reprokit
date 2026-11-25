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
              list(chisq_stats(~ cyl, data = data.frame(cyl = rep(4, 5)), test = "gof"),
                   "at least 2"))
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

test_that("chisq_table_stat matches chisq.test() for Pearson and Yates, 2 x 2 and larger", {
  tab22 <- matrix(c(30, 10, 20, 40), 2)
  tab23 <- matrix(c(10, 20, 30, 15, 25, 5), 2)
  expect_equal(chisq_table_stat(tab22, c(2, 2), yates = TRUE),
               unname(chisq.test(tab22, correct = TRUE)$statistic))
  expect_equal(chisq_table_stat(tab22, c(2, 2)),
               unname(chisq.test(tab22, correct = FALSE)$statistic))
  expect_equal(chisq_table_stat(tab23, c(2, 3)), unname(chisq.test(tab23)$statistic))
  # An empty row gives a zero expected count: not evaluable.
  expect_true(is.nan(chisq_table_stat(matrix(c(5, 0, 7, 0), 2), c(2, 2))))
})

test_that("chisq_gof_stat matches chisq.test() for uniform and given proportions", {
  n <- c(11, 18, 3, 8)
  expect_equal(chisq_gof_stat(n), unname(suppressWarnings(chisq.test(n))$statistic))
  p0 <- c(0.1, 0.4, 0.2, 0.3)
  expect_equal(chisq_gof_stat(n, p0), unname(suppressWarnings(chisq.test(n, p = p0))$statistic))
  expect_true(is.nan(chisq_gof_stat(c(4, 5, 6), c(0.5, 0.5, 0))))
})

test_that("shape and proportion arguments are refused as input errors", {
  err <- "reprokit_error_input"
  expect_error(check_table_dims(c(1, 3), "independence"), class = err)
  expect_error(check_table_dims(3, "independence"), class = err)
  expect_error(check_table_dims(c(2, 2.5), "independence"), class = err)
  expect_error(check_table_dims(c(2, 2), "gof"), class = err)
  expect_error(check_table_dims(1, "gof"), class = err)
  expect_identical(check_table_dims(c(2, 3), "independence"), c(2, 3))
  expect_error(check_p0(c(0.5, -0.1, 0.6)), class = err)
  expect_error(check_p0(c(0.5, NA)), class = err)
  expect_error(check_p0(c(0.5, 0.5), k = 3), class = err)
  expect_error(check_p0(c(0.5, 0.4)), class = err)
  expect_identical(check_p0(c(0.25, 0.75), k = 2), c(0.25, 0.75))
  expect_error(table_matrix(1:5, c(2, 3)), class = err)
  expect_error(chisq_gof_stat(c(1, 2, 3), k = 4), class = err)
  expect_error(chisq_table_stat(1:5, c(2, 3)), class = err)
})

test_that("gof refuses a zero p0, a single category and a two-column formula", {
  err <- "reprokit_error_input"
  d <- cyl_data()
  expect_error(chisq_gof_summarise(~ cyl, d, "gof", p0 = c(0.5, 0.5, 0)), class = err)
  expect_error(chisq_gof_summarise(~ cyl, d, "gof", table = c(a = 5)), class = err)
  expect_error(chisq_gof_summarise(~ cyl + gear, mtcars, "gof"), class = err)
  expect_error(chisq_independence_summarise(~ cyl + gear + am, mtcars, "chisq"), class = err)
  expect_error(chisq_independence_summarise(~ x + y, data.frame(x = 1:6, y = "a"), "chisq"),
               class = err)
})

test_that("goodness of fit from a counts vector works on the result route and with table=", {
  chisq <- function(...) suppressWarnings(chisq.test(...))
  cases <- list(
    list(r = chisq(c(5, 7, 9)), counts = c(5, 7, 9), p = NULL),
    list(r = chisq(c(a = 5, b = 7, c = 9), p = c(.2, .3, .5)),
         counts = c(a = 5, b = 7, c = 9), p = c(.2, .3, .5)),
    list(r = chisq(table(rep(c("x", "y", "z"), c(6, 8, 10)))),
         counts = table(rep(c("x", "y", "z"), c(6, 8, 10))), p = NULL)
  )
  placeholder <- data.frame(category = 1)
  for (case in cases) {
    s <- sufficient_stats(case$r)
    expect_identical(s$verdict, "summaries_suffice")
    got <- reproduce(s)
    expect_equal(unname(got[c("chisq", "df", "p")]),
                 unname(c(case$r$statistic, case$r$parameter, case$r$p.value)),
                 tolerance = 1e-8)
    k <- if (is.null(case$p)) {
      sufficient_stats(~ category, data = placeholder, test = "gof", table = case$counts)
    } else {
      sufficient_stats(~ category, data = placeholder, test = "gof", table = case$counts,
                       p0 = case$p)
    }
    expect_identical(k$verdict, "summaries_suffice")
    expect_equal(reproduce(k), got)
    expect_identical(s$stats, k$stats)
    expect_identical(s$meta[setdiff(names(s$meta), "route")],
                     k$meta[setdiff(names(k$meta), "route")])
  }
})

test_that("an unnamed counts vector gets the labels the formula route gives a factor", {
  from_vector <- chisq_table_from_observed(c(5, 7, 9), "category")
  from_factor <- chisq_gof_table(~ category, data.frame(category = factor(rep(1:3, c(5, 7, 9)))))
  expect_identical(from_vector, from_factor)
})

test_that("a single-category counts vector is an input error, and 2-D tables are unaffected", {
  expect_error(chisq_gof_summarise(~ category, NULL, "gof", table = c(a = 5)),
               class = "reprokit_error_input")
  expect_error(chisq_gof_summarise(~ category, NULL, "gof", table = 5),
               class = "reprokit_error_input")
  m <- matrix(c(30, 10, 20, 40), 2)
  expect_identical(dim(chisq_table_from_observed(m, c("row", "column"))), c(2L, 2L))
})
