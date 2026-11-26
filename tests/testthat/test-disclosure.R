# Disclosure hooks of ADR 0023: chi-squared counts, lm factor levels, and the
# sample-size-only hook of the handlers without groups or cells.

from_counts <- function(counts, dims = NULL) {
  if (is.null(dims)) {
    f <- factor(rep(LETTERS[seq_along(counts)], counts), levels = LETTERS[seq_along(counts)])
    return(data.frame(a = f))
  }
  grid <- expand.grid(a = LETTERS[1:dims[1]], b = letters[1:dims[2]])
  grid[rep(seq_len(nrow(grid)), counts), ]
}

appendix_text <- function(s) paste(appendix(s, sharing_reason = "test"), collapse = "\n")

test_that("chi-squared independence flags a cell with 0 < count < threshold", {
  flagged <- from_counts(c(30, 3, 20, 40), c(2, 2))
  tr <- sufficient_stats(~ a + b, data = flagged, test = "chisq")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "cell \\(B, a\\) has n = 3 < 5")
  expect_no_error(sufficient_stats(~ a + b, data = flagged, test = "chisq"))
  expect_error(appendix(tr, sharing_reason = "x"), class = "reprokit_impossible")
  # The same table is fine at a lower threshold.
  expect_identical(sufficient_stats(~ a + b, data = flagged, test = "chisq", threshold = 3)$verdict,
                   "apa_suffices")
})

test_that("chi-squared independence is unaffected by a clean table and ignores zeros", {
  clean <- from_counts(c(30, 10, 20, 40), c(2, 2))
  expect_identical(sufficient_stats(~ a + b, data = clean, test = "chisq")$verdict,
                   "apa_suffices")
  # A zero count is not flagged (ADR 0023 point 3); the other cells are large.
  zero <- from_counts(c(30, 0, 20, 40), c(2, 2))
  expect_identical(sufficient_stats(~ a + b, data = zero, test = "chisq")$verdict,
                   "apa_suffices")
  h <- get_handler("chisq_independence")
  summary <- h$summarise(~ a + b, data = zero, test = "chisq")
  expect_true(h$disclosure(summary, 5)$passed)
})

test_that("chi-squared independence names the cell check in the appendix", {
  s <- sufficient_stats(~ a + b, data = from_counts(c(30, 10, 20, 40), c(2, 2)), test = "chisq")
  expect_match(appendix_text(s),
               "Checked: cell counts (a count above 0 and below 5 is flagged; zeros are not)",
               fixed = TRUE)
})

test_that("chi-squared goodness of fit flags a category with 0 < count < threshold", {
  flagged <- from_counts(c(30, 3, 20))
  tr <- sufficient_stats(~ a, data = flagged, test = "gof")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "category 'B' has n = 3 < 5")
  expect_identical(sufficient_stats(~ a, data = from_counts(c(30, 10, 20)), test = "gof")$verdict,
                   "apa_suffices")
})

test_that("chi-squared goodness of fit does not flag a zero category", {
  zero <- from_counts(c(30, 0, 20))
  expect_identical(table(zero$a)[["B"]], 0L)
  expect_identical(sufficient_stats(~ a, data = zero, test = "gof")$verdict, "apa_suffices")
  s <- sufficient_stats(~ a, data = zero, test = "gof")
  expect_match(appendix_text(s), "Checked: category counts", fixed = TRUE)
})

lm_small_level <- function() {
  # 3 of the 7 six-cylinder cars: the level is small, the sample (n = 28) is not.
  six <- which(mtcars$cyl == 6)
  mtcars[-six[4:7], ]
}

test_that("lm_multiple flags a factor level with fewer than threshold rows", {
  fit <- lm(mpg ~ wt + factor(cyl), data = lm_small_level())
  tr <- sufficient_stats(fit)
  expect_identical(tr$handler, "lm_multiple")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "factor factor\\(cyl\\), level '6' has n = 3 < 5")
  expect_identical(sufficient_stats(fit, threshold = 3)$verdict, "apa_plus_suffices")
  clean <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  expect_identical(sufficient_stats(clean)$verdict, "apa_plus_suffices")
})

test_that("lm_moderation flags a factor level with fewer than threshold rows", {
  d <- mtcars[mtcars$am == 0 | seq_len(nrow(mtcars)) %in% which(mtcars$am == 1)[1:4], ]
  fit <- lm(mpg ~ wt * factor(am), data = d)
  tr <- sufficient_stats(fit)
  expect_identical(tr$handler, "lm_moderation")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "factor factor\\(am\\), level '1' has n = 4 < 5")
  clean <- lm(mpg ~ wt * factor(am), data = mtcars)
  expect_identical(sufficient_stats(clean)$handler, "lm_moderation")
  expect_identical(sufficient_stats(clean)$verdict, "apa_plus_suffices")
})

test_that("the general lm flags a factor level with fewer than threshold rows", {
  fit <- lm(mpg ~ poly(wt, 2) + factor(cyl), data = lm_small_level())
  tr <- sufficient_stats(fit)
  expect_identical(tr$handler, "lm")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "level '6' has n = 3 < 5")
  clean <- lm(mpg ~ poly(wt, 2) + factor(cyl), data = mtcars)
  expect_identical(sufficient_stats(clean)$handler, "lm")
  expect_identical(sufficient_stats(clean)$verdict, "apa_plus_suffices")
})

test_that("factor levels are counted unweighted, over the rows in the fit", {
  d <- lm_small_level()
  d$w <- ifelse(d$cyl == 6, 1000, 1)
  expect_match(sufficient_stats(lm(mpg ~ wt + factor(cyl), data = d, weights = w))$verdict_reason,
               "level '6' has n = 3 < 5")
  # Rows with weight 0 are not in the fit, so they do not count towards a level.
  d$w <- 1
  d$w[which(d$cyl == 4)[1:7]] <- 0
  expect_match(sufficient_stats(lm(mpg ~ wt + factor(cyl), data = d, weights = w))$verdict_reason,
               "level '4' has n = 4 < 5")
})

test_that("a factor level with no rows is not flagged", {
  summary <- list(check = list(
    binary_counts = list(n = 20, ones = numeric()),
    factor_counts = list(f = table(factor(c(rep("a", 10), rep("b", 10)),
                                          levels = c("a", "b", "unused"))))
  ))
  res <- lm_disclosure(summary, 5)
  expect_true(res$passed)
  expect_length(res$problems, 0L)
  summary$check$factor_counts$f[["b"]] <- 2L
  expect_match(lm_disclosure(summary, 5)$problems, "level 'b' has n = 2 < 5")
})

test_that("the lm appendix names the factor-level check", {
  s <- sufficient_stats(lm(mpg ~ wt + factor(cyl), data = mtcars))
  expect_match(appendix_text(s), "Checked: factor levels (unweighted rows in the fit)",
               fixed = TRUE)
})

sample_size_cases <- function(n) {
  set.seed(11)
  d <- data.frame(x = rnorm(n), y = rnorm(n), m = rnorm(n))
  list(
    one_sample = list(x = x ~ 1, args = list(data = d, test = "one_sample"),
                      handler = "t_one_sample"),
    paired = list(x = Pair(x, y) ~ 1, args = list(data = d, test = "paired"),
                  handler = "t_paired"),
    pearson = list(x = ~ x + y, args = list(data = d, test = "pearson"), handler = "cor_pearson"),
    mediation = list(x = y ~ x, args = list(data = d, test = "mediation", mediator = "m"),
                     handler = "mediation_simple")
  )
}

test_that("handlers without groups state that the check is the sample size only", {
  for (case in sample_size_cases(30)) {
    tr <- do.call(sufficient_stats, c(list(case$x), case$args))
    expect_identical(tr$handler, case$handler)
    plus <- case$handler %in% c("t_paired", "mediation_simple")
    expect_identical(tr$verdict, if (plus) "apa_plus_suffices" else "apa_suffices",
                     info = case$handler)
    s <- do.call(sufficient_stats, c(list(case$x), case$args))
    expect_match(s$disclosure$checked, "sample size only", info = case$handler)
    expect_true(s$disclosure$passed, info = case$handler)
    expect_length(s$disclosure$problems, 0L)
    expect_match(appendix_text(s), "Checked: sample size only", fixed = TRUE, info = case$handler)
  }
})

test_that("the sample-size-only handlers still drop to needs_raw_data below 2 x threshold", {
  for (case in sample_size_cases(8)) {
    tr <- do.call(sufficient_stats, c(list(case$x), case$args))
    expect_identical(tr$handler, case$handler)
    expect_identical(tr$verdict, "needs_raw_data", info = case$handler)
    expect_match(tr$verdict_reason, "total n = 8 is below the minimum of 10", info = case$handler)
  }
})
