test_that("impossible requests signal a classed error with alternatives", {
  cnd <- expect_error(
    abort_impossible("refit a logistic multilevel model", "the likelihood needs the raw data"),
    class = "reprokit_impossible"
  )
  expect_true(length(cnd$alternatives) > 0)
  expect_match(conditionMessage(cnd), "DataSHIELD")
})

test_that("a usage error is not an impossible request", {
  cnd <- expect_error(abort_input("`x` must be a number."), class = "reprokit_error_input")
  expect_s3_class(cnd, "reprokit_error")
  expect_false(inherits(cnd, "reprokit_impossible"))
  expect_false(grepl("Cannot .* from summary statistics|DataSHIELD|Instead you could",
                     conditionMessage(cnd)))
  # A real usage error from the API carries the same class and no data-sharing advice.
  cnd <- expect_error(sufficient_stats(extra ~ group, data = sleep, test = "nonsense"),
                      class = "reprokit_error_input")
  expect_false(grepl("DataSHIELD|Instead you could", conditionMessage(cnd)))
  # An impossible request is still reprokit_impossible, not an input error.
  expect_false(inherits(expect_error(abort_impossible("x", "y"), class = "reprokit_impossible"),
                        "reprokit_error_input"))
})

test_that("messages shown to users do not mention handlers", {
  msgs <- character()
  grab <- function(expr) {
    cnd <- tryCatch(expr, error = function(e) e)
    msgs <<- c(msgs, if (inherits(cnd, "condition")) conditionMessage(cnd) else
      paste(capture.output(print(cnd)), cnd$verdict_reason))
  }
  grab(check_reported(test = "factorial", f = 3, df1 = 1, df2 = 20, p = 0.1))
  grab(sufficient_stats(extra ~ group, data = sleep, test = "nonsense"))
  grab(sufficient_stats(structure(list(), class = "no_such_class")))
  grab(check_reported(test = "chisq_gof", x = 1))
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  msgs <- c(msgs, paste(capture.output(print(s)), collapse = "\n"))
  t <- sufficient_stats(y ~ g, data = data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8.5),
                                                 g = rep(c("a", "b"), c(5, 3))), test = "welch")
  msgs <- c(msgs, paste(capture.output(print(t)), t$verdict_reason))
  expect_gt(length(msgs), 5L)
  expect_false(any(grepl("handler", msgs, ignore.case = TRUE)), info = paste(msgs, collapse = "\n"))
})

test_that("the package overview matches behaviour", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  # `lost` is not an argument anywhere a user reaches.
  expect_false("lost" %in% names(formals(sufficient_stats)))
  expect_false("lost" %in% names(formals(appendix)))
  # sharing_reason is optional: missing gives a warning, not an error.
  expect_warning(out <- appendix(s), "sharing_reason")
  expect_match(paste(out, collapse = "\n"), "not stated by the author")
  # Mediation works with the two fits in either order.
  d <- mtcars
  m <- lm(mpg ~ wt, d)
  y <- lm(mpg ~ wt + hp, d)
  m2 <- lm(hp ~ wt, d)
  a <- sufficient_stats(m2, y)
  b <- sufficient_stats(y, m2)
  expect_identical(a$handler, "mediation_simple")
  expect_identical(b$handler, "mediation_simple")
})
