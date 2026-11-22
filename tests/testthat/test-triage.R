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
    needs = "nothing", lost = "exactness", matches = function(x, ...) inherits(x, "dummy")
  )
  res <- triage(structure(list(), class = "dummy"))
  expect_identical(res$verdict, "synthetic_possible")
  expect_identical(res$handler, "test_dummy")
})

test_that("a failing handler becomes a verdict, not a crash", {
  local_handler(
    "test_broken", family = "Test", analysis = "Broken", verdict = "synthetic_possible",
    needs = "nothing", lost = "exactness", matches = function(x, ...) inherits(x, "broken"),
    triage = function(x, ...) stop("boom")
  )
  res <- expect_no_error(triage(structure(list(), class = "broken")))
  expect_identical(res$verdict, "needs_raw_data")
  expect_match(res$reason, "boom")
})

test_that("the general minimum n applies to every handler, with or without its own check", {
  # A handler with no disclosure function of its own: only the general rule protects it.
  local_handler(
    "test_mean", family = "Test", analysis = "Mean", verdict = "summaries_suffice",
    needs = "n and mean", lost = "the raw values",
    matches = function(x, ...) inherits(x, "test_sample"),
    kit = function(x, ...) {
      list(stats = list(mean = mean(unclass(x))), meta = list(n = length(x)))
    },
    reproduce = function(kit) c(mean = kit$stats$mean),
    reported = function(x, ...) c(mean = mean(unclass(x))),
    example = function() list(x = structure(1:20, class = "test_sample"), args = list())
  )
  sample_of <- function(n) structure(seq_len(n) + 0.5, class = "test_sample")
  expect_identical(minimum_n(5), 10)
  tr <- triage(sample_of(9))
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$reason, "total n = 9 is below the minimum of 10")
  expect_identical(triage(sample_of(10))$verdict, "summaries_suffice")
  # The minimum follows the threshold.
  expect_identical(triage(sample_of(9), threshold = 4)$verdict, "summaries_suffice")
  expect_error(kit(sample_of(9), sharing_reason = "test"), class = "reprokit_impossible")
})

test_that("a kit without meta$n is downgraded rather than trusted", {
  local_handler(
    "test_no_n", family = "Test", analysis = "No n", verdict = "summaries_suffice",
    needs = "mean", lost = "the raw values",
    matches = function(x, ...) inherits(x, "test_no_n"),
    kit = function(x, ...) list(stats = list(mean = 1), meta = list()),
    reproduce = function(kit) c(mean = 1), reported = function(x, ...) c(mean = 1),
    example = function() list(x = structure(1, class = "test_no_n"), args = list())
  )
  tr <- triage(structure(1, class = "test_no_n"))
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$reason, "did not report the sample size")
})

test_that("both shipped handlers hit the general minimum on tiny data", {
  d <- data.frame(y = c(1.1, 2.3, 3.2, 4.8, 5.1, 6.2, 7.4, 8.5, 9.9),
                  g = rep(c("a", "b"), c(5, 4)))
  # The general check runs first, so it gives the reason even though group b (n = 4) also fails.
  expect_match(triage(y ~ g, data = d, test = "welch")$reason,
               "total n = 9 is below the minimum of 10")
  fit <- lm(mpg ~ wt, data = mtcars[1:9, ])
  expect_match(triage(fit)$reason, "total n = 9 is below the minimum of 10")
})
