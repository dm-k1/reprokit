factorial_stats <- function(d, f = y ~ a * b, ...) {
  sufficient_stats(f, data = d, test = "factorial", ...)
}

test_that("the factorial handler reproduces lm() for every SS type, balanced and not", {
  tg <- datasets::ToothGrowth
  tg$dose <- factor(tg$dose)
  for (type in ss_type_codes()) {
    s <- factorial_stats(tg, len ~ supp * dose, ss_type = type)
    expect_reproduces(anova_factorial_reported(len ~ supp * dose, tg, "factorial", ss_type = type),
                      reproduce(s))
    d <- factorial_unbalanced()
    expect_reproduces(anova_factorial_reported(y ~ a * b, d, "factorial", ss_type = type),
                      reproduce(factorial_stats(d, ss_type = type)))
  }
})

test_that("the SS types agree for a balanced design and differ for an unbalanced one", {
  tg <- datasets::ToothGrowth
  tg$dose <- factor(tg$dose)
  types <- lapply(ss_type_codes(), function(type) {
    reproduce(factorial_stats(tg, len ~ supp * dose, ss_type = type))
  })
  expect_equal(types[[1L]], types[[2L]])
  expect_equal(types[[1L]], types[[3L]])
  d <- factorial_unbalanced()
  un <- lapply(ss_type_codes(), function(type) reproduce(factorial_stats(d, ss_type = type)))
  expect_gt(abs(un[[1L]][["f_A"]] - un[[2L]][["f_A"]]), 0.01)
  expect_gt(abs(un[[1L]][["f_A"]] - un[[3L]][["f_A"]]), 0.01)
  # The interaction is the same in every type.
  expect_equal(un[[1L]][["f_AB"]], un[[2L]][["f_AB"]])
  expect_equal(un[[1L]][["f_AB"]], un[[3L]][["f_AB"]])
})

test_that("the summary stores per-cell n, mean and SD at full precision, column-major", {
  tg <- datasets::ToothGrowth
  tg$dose <- factor(tg$dose)
  s <- factorial_stats(tg, len ~ supp * dose)
  expect_named(s$stats, c("n", "mean", "sd"))
  expect_equal(s$stats$mean, as.vector(tapply(tg$len, list(tg$supp, tg$dose), mean)), tolerance = 0)
  expect_identical(s$stats$n, rep(10L, 6L))
  expect_identical(s$meta$dims, c(2L, 3L))
  expect_true(s$meta$balanced)
  expect_false(factorial_stats(factorial_unbalanced())$meta$balanced)
})

test_that("the ADR 0019 limitation is listed and the sharing record is complete", {
  s <- factorial_stats(factorial_unbalanced())
  expect_true("equal variances assumed (pooled error term)" %in% s$lost)
  # the sharing reason is stated where something is published: appendix()
  expect_match(paste(appendix(s, sharing_reason = "test reason XYZ"), collapse = "\n"),
               "test reason XYZ", fixed = TRUE)
})

test_that("an empty cell, a cell of one, a missing interaction and a bad SS type are refused", {
  d <- factorial_unbalanced()
  empty <- d[!(d$a == "a1" & d$b == "1"), ]
  refused <- function(s, reason = NULL) {
    expect_identical(s$verdict, "needs_raw_data")
    if (!is.null(reason)) expect_match(s$verdict_reason, reason)
    expect_error(reproduce(s), class = "reprokit_impossible")
    expect_error(appendix(s, sharing_reason = "t"), class = "reprokit_impossible")
  }
  expect_no_error(s_empty <- factorial_stats(empty))
  refused(s_empty, "empty")
  one <- d[!(d$a == "a1" & d$b == "1") | seq_len(nrow(d)) == 1L, ]
  expect_no_error(s_one <- factorial_stats(one))
  refused(s_one, "at least 2")
  expect_no_error(s_add <- factorial_stats(d, y ~ a + b))
  refused(s_add)
  expect_no_error(s_ss <- factorial_stats(d, ss_type = "IV"))
  refused(s_ss, "ss_type")
})

test_that("the registry row reads like the plan's, with the SS type as the choice", {
  h <- get_handler("anova_factorial")
  choices <- paste(handler_choices(h), collapse = "; ")
  expect_match(choices, "SS type for unbalanced designs")
  expect_match(choices, "partial omega squared")
})

test_that("the registered example round-trips", {
  h <- get_handler("anova_factorial")
  ex <- h$example()
  s <- do.call(sufficient_stats, c(list(ex$x), ex$args))
  expect_reproduces(do.call(h$reported, c(list(ex$x), ex$args)), reproduce(s))
})
