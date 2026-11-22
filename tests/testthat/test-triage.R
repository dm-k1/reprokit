test_that("triage never errors on unsupported input", {
  for (x in list(NULL, 1:3, list(), "a", function() 1, data.frame())) {
    res <- expect_no_error(triage(x))
    expect_s3_class(res, "reprokit_triage")
    expect_identical(res$verdict, "needs_raw_data")
  }
})

test_that("triage uses the matching handler", {
  local_handler(
    "test_dummy", family = "Test", analysis = "Dummy", verdict = "synthetic_possible",
    needs = "nothing", lost = "exactness", matches = function(x) inherits(x, "dummy")
  )
  res <- triage(structure(list(), class = "dummy"))
  expect_identical(res$verdict, "synthetic_possible")
  expect_identical(res$handler, "test_dummy")
})

test_that("a failing handler becomes a verdict, not a crash", {
  local_handler(
    "test_broken", family = "Test", analysis = "Broken", verdict = "synthetic_possible",
    needs = "nothing", lost = "exactness", matches = function(x) inherits(x, "broken"),
    triage = function(x, ...) stop("boom")
  )
  res <- expect_no_error(triage(structure(list(), class = "broken")))
  expect_identical(res$verdict, "needs_raw_data")
  expect_match(res$reason, "boom")
})
