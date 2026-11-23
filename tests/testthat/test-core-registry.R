test_that("registered handlers appear in handlers() and coverage_table()", {
  local_handler(
    "test_dummy", family = "Test", analysis = "Dummy", verdict = "needs_raw_data",
    needs = "nothing", lost = "everything", matches = function(x, ...) FALSE
  )
  expect_true("test_dummy" %in% handlers())
  # A handler without a design has no row: the table is about what can be checked.
  expect_false("Dummy" %in% coverage_table()$test)
})

test_that("coverage_table() has a fixed shape even when empty", {
  cols <- c("test", "apa", "sufficient", "missing", "checkable", "not_checkable", "choices")
  expect_named(coverage_table(), cols)
  saved <- as.list(.registry)
  rm(list = ls(.registry), envir = .registry)
  withr::defer(list2env(saved, envir = .registry))
  empty <- coverage_table()
  expect_named(empty, cols)
  expect_equal(nrow(empty), 0L)
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

checking_handlers <- function() {
  Filter(function(n) !is.null(get_handler(n)$design), handlers())
}

test_that("what a handler says is sufficient is a subset of what its summarise stores", {
  # `m` is stored as `mean`; every other quantity is stored under its own name.
  stored_name <- function(quantity) if (quantity == "m") "mean" else quantity
  with_sufficient <- Filter(function(n) !is.null(get_handler(n)$sufficient), handlers())
  expect_gt(length(with_sufficient), 0L)
  for (name in with_sufficient) {
    h <- get_handler(name)
    ex <- h$example()
    stored <- names(do.call(h$summarise, c(list(ex$x), ex$args))$stats)
    expect_true(all(vapply(h$sufficient, stored_name, character(1)) %in% stored),
                info = name)
  }
})

test_that("every APA number no relation reaches has a declared reason, and only those", {
  expect_gt(length(checking_handlers()), 0L)
  for (name in checking_handlers()) {
    h <- get_handler(name)
    unreached <- unreached_quantities(h)
    expect_length(setdiff(unreached, names(h$not_checkable)), 0L)
    expect_length(setdiff(names(h$not_checkable), unreached), 0L)
  }
})

test_that("every missing quantity has a shared reason", {
  for (name in checking_handlers()) {
    h <- get_handler(name)
    expect_true(all(missing_quantities(h) %in% names(missing_reasons())), info = name)
  }
})

test_that("the derived sets follow from apa, sufficient and relations", {
  paired <- get_handler("t_paired")
  expect_identical(missing_quantities(paired), "r")
  expect_setequal(checkable_quantities(paired), c("df", "t", "p", "ci_lower", "ci_upper"))
  expect_identical(unreached_quantities(paired), "d")
  pearson <- get_handler("cor_pearson")
  expect_identical(missing_quantities(pearson), character())
  expect_setequal(checkable_quantities(pearson), c("df", "p", "ci_lower", "ci_upper"))
  expect_setequal(unreached_quantities(pearson), c("m", "sd"))
  expect_true("tails (two.sided, less, greater)" %in% handler_choices(pearson))
  expect_true("test (student, welch)" %in% handler_choices(get_handler("t_independent")))
})

test_that("not_checkable names must be known quantities", {
  expect_error(
    register_handler("test_bad", family = "Test", analysis = "Bad", verdict = "needs_raw_data",
                     needs = "x", lost = "y", matches = function(x, ...) FALSE,
                     not_checkable = c(nonsense = "no")),
    "Unknown quantity name in `not_checkable`"
  )
})

test_that("coverage_table() has one row per checking handler, with em dashes for empty", {
  tab <- coverage_table()
  expect_equal(nrow(tab), length(checking_handlers()))
  expect_true(all(vapply(tab, is.character, logical(1))))
  expect_false(anyNA(tab))
  paired <- tab[tab$test == get_handler("t_paired")$analysis, ]
  expect_match(paired$missing, "^r \\(")
  expect_identical(paired$sufficient, "n, M, SD per condition, r")
})
