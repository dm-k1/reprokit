test_that("sufficient_stats() dispatches to the matching handler and reproduce() reproduces", {
  k <- sufficient_stats(extra ~ group, data = sleep, test = "student", sharing_reason = "test")
  expect_s3_class(k, "reprokit_sufficient_stats")
  expect_identical(k$handler, "t_independent")
  expect_reproduces(t_independent_reported(extra ~ group, sleep, "student"), reproduce(k))
})

test_that("sufficient_stats() refuses unknown input with reprokit_impossible", {
  expect_error(sufficient_stats(list(), sharing_reason = "test"), class = "reprokit_impossible")
  # A formula without a declared test matches no handler.
  expect_error(sufficient_stats(extra ~ group, data = sleep, sharing_reason = "test"),
               class = "reprokit_impossible")
})

test_that("sufficient_stats() refuses when the handler says summaries do not suffice", {
  local_handler(
    "test_locked", family = "Test", analysis = "Locked", verdict = "needs_raw_data",
    needs = "nothing", lost = "everything", matches = function(x, ...) inherits(x, "locked")
  )
  expect_error(sufficient_stats(structure(list(), class = "locked"), sharing_reason = "test"),
               class = "reprokit_impossible")
})

test_that("reproduce() refuses statistics that is not summaries_suffice", {
  args <- valid_stats_args()
  args$verdict <- "needs_raw_data"
  k <- do.call(new_sufficient_stats, args)
  expect_error(reproduce(k), class = "reprokit_impossible")
  expect_error(reproduce(list()), "must be made by sufficient_stats")
})

test_that("the handler's lost list is a floor: users can add, not remove", {
  k <- sufficient_stats(extra ~ group, data = sleep, test = "welch", sharing_reason = "test",
                        lost = "my extra point")
  expect_true(all(get_handler("t_independent")$lost %in% k$lost))
  expect_true("my extra point" %in% k$lost)
})

test_that("a group below the threshold downgrades the verdict and blocks sufficient_stats()", {
  d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8.5), g = rep(c("a", "b"), c(5, 3)))
  tr <- triage(y ~ g, data = d, test = "welch")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$reason, "condition 3")
  expect_error(sufficient_stats(y ~ g, data = d, test = "welch", sharing_reason = "test"),
               class = "reprokit_impossible")
  # The same data pass with a lower threshold, which the object records.
  k <- sufficient_stats(y ~ g, data = d, test = "welch", sharing_reason = "test", threshold = 3)
  expect_identical(k$disclosure$threshold, 3)
})
