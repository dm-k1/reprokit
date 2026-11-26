test_that("lm with numeric and factor predictors reproduces summary(lm)", {
  fits <- list(
    lm(mpg ~ wt + factor(cyl), data = mtcars),
    lm(mpg ~ wt * hp, data = mtcars),
    lm(mpg ~ 1, data = mtcars),
    lm(mpg ~ 0 + wt + hp, data = mtcars),
    lm(mpg ~ 0 + factor(am), data = mtcars)
  )
  for (fit in fits) {
    expect_reproduces(lm_reported(fit), reproduce(sufficient_stats(fit)))
  }
})

test_that("weighted lm and offsets reproduce summary(lm) exactly", {
  m <- mtcars
  m$w0 <- m$hp
  m$w0[c(3, 7, 11)] <- 0  # zero weights: summary.lm treats these rows as absent
  fits <- list(
    lm(mpg ~ wt, data = mtcars, weights = hp),
    lm(mpg ~ wt + factor(cyl), data = m, weights = w0),
    lm(mpg ~ wt + offset(hp / 100), data = mtcars),
    lm(mpg ~ wt, data = mtcars, offset = qsec),
    lm(mpg ~ 0 + wt + offset(hp / 100), data = mtcars),
    lm(mpg ~ 0 + wt + hp, data = mtcars, weights = qsec),
    lm(mpg ~ 1, data = mtcars, weights = hp),
    lm(mpg ~ wt + offset(qsec), data = mtcars, weights = hp)
  )
  for (fit in fits) {
    expect_true(verdict_suffices(sufficient_stats(fit)$verdict))
    expect_reproduces(lm_reported(fit), reproduce(sufficient_stats(fit)))
  }
})

test_that("rank deficiency is downgraded with a reason", {
  m <- mtcars
  m$wt2 <- 2 * m$wt
  fit <- lm(mpg ~ wt + wt2, data = m)
  expect_no_error(tr <- sufficient_stats(fit))
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "rank-deficient")
  expect_error(reproduce(tr), class = "reprokit_impossible")
  expect_error(appendix(tr, sharing_reason = "t"), class = "reprokit_impossible")
})

test_that("weights cannot hide a small cell from the disclosure check", {
  m <- mtcars
  m$w <- ifelse(m$cyl == 8 & m$am == 1, 1000, 1)
  tr <- sufficient_stats(lm(mpg ~ factor(cyl) + am, data = m, weights = w))
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "factor\\(cyl\\)8 = 1 & am = 1: n = 2")
})

test_that("glm gets no verdict of sufficiency, and aov does not match the lm handler", {
  expect_identical(sufficient_stats(glm(am ~ wt, data = mtcars, family = binomial))$verdict,
                   "needs_raw_data")
  # aov is accepted through its own route (ADR 0027): one factor is a one-way ANOVA ...
  one <- sufficient_stats(aov(mpg ~ factor(cyl), data = mtcars))
  expect_identical(one$handler, "anova_oneway")
  expect_identical(one$verdict, "apa_suffices")
  # ... and a covariate (ANCOVA) has no handler
  ancova <- sufficient_stats(aov(mpg ~ factor(cyl) + wt, data = mtcars))
  expect_identical(ancova$verdict, "needs_raw_data")
  expect_true(is.na(ancova$handler))
})

test_that("small factor cells are flagged by the disclosure check", {
  # cyl x am: 8 cylinders with manual transmission has n = 2.
  fit <- lm(mpg ~ factor(cyl) + am, data = mtcars)
  tr <- sufficient_stats(fit)
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "factor\\(cyl\\)8 = 1 & am = 1: n = 2")
  expect_identical(sufficient_stats(fit, threshold = 2)$verdict, "apa_plus_suffices")
})
