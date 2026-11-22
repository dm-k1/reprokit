test_that("registered handlers appear in handlers() and coverage_table()", {
  local_handler(
    "test_dummy", family = "Test", analysis = "Dummy", verdict = "needs_raw_data",
    needs = "nothing", lost = "everything", matches = function(x) FALSE
  )
  expect_true("test_dummy" %in% handlers())
  tab <- coverage_table()
  expect_true("test_dummy" %in% tab$handler)
  expect_identical(tab$verdict[tab$handler == "test_dummy"], "needs_raw_data")
})

test_that("coverage_table() has a fixed shape even when empty", {
  expect_named(coverage_table(), c("family", "analysis", "verdict", "needs", "catch", "handler"))
})

test_that("a handler claiming exact reproduction must supply kit and refit", {
  expect_error(
    register_handler(
      "test_bad", family = "Test", analysis = "Bad", verdict = "summaries_suffice",
      needs = "x", lost = "y", matches = function(x) FALSE
    ),
    "no kit/refit/example"
  )
})

test_that("every handler that claims exact reproduction round-trips at 1e-8", {
  # Grows automatically as handlers are added. No handlers exist yet.
  exact <- Filter(function(n) get_handler(n)$verdict == "summaries_suffice", handlers())
  skip_if(length(exact) == 0L, "no summaries_suffice handlers registered yet")
  for (name in exact) {
    h <- get_handler(name)
    k <- h$kit(h$example, sharing_reason = "test", lost = "test")
    expect_reproduces(stats::coef(h$example), stats::coef(h$refit(k)))
  }
})
