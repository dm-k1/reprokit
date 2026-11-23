test_that("registered handlers appear in handlers() and coverage_table()", {
  local_handler(
    "test_dummy", family = "Test", analysis = "Dummy", verdict = "needs_raw_data",
    needs = "nothing", lost = "everything", matches = function(x, ...) FALSE
  )
  expect_true("test_dummy" %in% handlers())
  tab <- coverage_table()
  expect_true("test_dummy" %in% tab$handler)
  expect_identical(tab$verdict[tab$handler == "test_dummy"], "needs_raw_data")
})

test_that("coverage_table() has a fixed shape even when empty", {
  expect_named(coverage_table(), c("family", "analysis", "verdict", "needs", "catch", "handler"))
})

test_that("a handler claiming exact reproduction must supply summarise and reproduce", {
  expect_error(
    register_handler(
      "test_bad", family = "Test", analysis = "Bad", verdict = "summaries_suffice",
      needs = "x", lost = "y", matches = function(x, ...) FALSE
    ),
    "claims exact reproduction"
  )
})

test_that("every handler that claims exact reproduction round-trips (scaled difference < 1e-8)", {
  # Grows automatically as handlers are added.
  exact <- Filter(function(n) get_handler(n)$verdict == "summaries_suffice", handlers())
  expect_gt(length(exact), 0L)
  for (name in exact) {
    h <- get_handler(name)
    ex <- h$example()
    k <- do.call(sufficient_stats, c(list(ex$x), ex$args, list(sharing_reason = "test")))
    expect_reproduces(do.call(h$reported, c(list(ex$x), ex$args)), reproduce(k))
  }
})

test_that("what a handler says is reproducible is a subset of what its summarise stores", {
  # `m` is stored as `mean`; every other quantity is stored under its own name.
  stored_name <- function(quantity) if (quantity == "m") "mean" else quantity
  with_reproducible <- Filter(function(n) !is.null(get_handler(n)$reproducible), handlers())
  expect_gt(length(with_reproducible), 0L)
  for (name in with_reproducible) {
    h <- get_handler(name)
    ex <- h$example()
    stored <- names(do.call(h$summarise, c(list(ex$x), ex$args))$stats)
    expect_true(all(vapply(h$reproducible, stored_name, character(1)) %in% stored),
                info = name)
  }
})
