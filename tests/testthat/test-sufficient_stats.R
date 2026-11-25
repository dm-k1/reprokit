test_that("sufficient_stats() dispatches to the matching handler and reproduce() reproduces", {
  k <- sufficient_stats(extra ~ group, data = sleep, test = "student")
  expect_s3_class(k, "reprokit_sufficient_stats")
  expect_identical(k$handler, "t_independent")
  expect_reproduces(t_independent_reported(extra ~ group, sleep, "student"), reproduce(k))
})

test_that("sufficient_stats() refuses unknown input with reprokit_impossible", {
  # A formula without a declared test matches no handler.
  expect_error(sufficient_stats(extra ~ group, data = sleep), class = "reprokit_error_input")
  # Unsupported (non-formula) input is a verdict, not an error: nothing to publish.
  s <- expect_no_error(sufficient_stats(list()))
  expect_identical(s$verdict, "needs_raw_data")
  expect_error(appendix(list(), sharing_reason = "test"), class = "reprokit_impossible")
  expect_error(reproduce(list()), class = "reprokit_impossible")
})

test_that("sufficient_stats() returns a verdict when the handler says summaries do not suffice", {
  local_handler(
    "test_locked", family = "Test", analysis = "Locked", verdict = "needs_raw_data",
    needs = "nothing", lost = "everything", matches = function(x, ...) inherits(x, "locked")
  )
  x <- structure(list(), class = "locked")
  s <- expect_no_error(sufficient_stats(x))
  expect_identical(s$verdict, "needs_raw_data")
  expect_identical(s$handler, "test_locked")
  expect_null(s$stats)
  expect_error(appendix(s, sharing_reason = "test"), class = "reprokit_impossible")
  expect_error(appendix(x, sharing_reason = "test"), class = "reprokit_impossible")
  expect_error(reproduce(s), class = "reprokit_impossible")
})

test_that("reproduce() refuses statistics that is not summaries_suffice", {
  args <- valid_stats_args()
  args$verdict <- "needs_raw_data"
  k <- do.call(new_sufficient_stats, args)
  expect_error(reproduce(k), class = "reprokit_impossible")
  expect_error(reproduce(list()), class = "reprokit_impossible")
})

test_that("sufficient_stats() takes no lost", {
  expect_error(sufficient_stats(extra ~ group, data = sleep, test = "welch", lost = "x"),
               "not an argument")
})

test_that("lost is stated by reprokit from the handler: appendix() takes no `lost`", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  expect_true(all(get_handler("t_independent")$lost %in% s$lost))
  expect_error(appendix(s, sharing_reason = "test", lost = "my extra point"), "lost")
  md <- appendix(s, sharing_reason = "test", choices = list(convention = "pooled"))
  text <- paste(md, collapse = "\n")
  for (item in get_handler("t_independent")$lost) expect_match(text, item, fixed = TRUE)
})

test_that("a group below the threshold downgrades the verdict and blocks appendix()", {
  d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8.5), g = rep(c("a", "b"), c(5, 3)))
  tr <- sufficient_stats(y ~ g, data = d, test = "welch")
  expect_identical(tr$verdict, "needs_raw_data")
  expect_match(tr$verdict_reason, "total n = 8 is below the minimum of 10")
  expect_error(appendix(y ~ g, data = d, test = "welch", sharing_reason = "test"),
               class = "reprokit_impossible")
  # The same data pass with a lower threshold, which the object records.
  k <- sufficient_stats(y ~ g, data = d, test = "welch", threshold = 3)
  expect_identical(k$verdict, "summaries_suffice")
  expect_identical(k$disclosure$threshold, 3)
})

test_that("a missing or empty sharing_reason warns and is stated as not stated", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  line <- "Reason for not sharing the raw data: not stated by the author."
  for (r in list(NULL, "", "   ", NA_character_)) {
    out <- NULL
    expect_warning(out <- if (is.null(r)) appendix(s) else appendix(s, sharing_reason = r),
                   "sharing_reason")
    expect_true(line %in% out)
  }
})

test_that("a given sharing_reason appears verbatim, without a warning", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  expect_no_warning(out <- appendix(s, sharing_reason = "Consent did not cover it."))
  expect_true("Reason for not sharing the raw data: Consent did not cover it." %in% out)
  expect_false(any(grepl("not stated by the author", out)))
})

test_that("sufficient_stats() does not store or take a reason", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch")
  expect_null(s$sharing_reason)
})

test_that("register_handler() refuses an empty or blank lost", {
  args <- list("bad_lost", "f", "a", "needs_raw_data", "n", matches = function(x, ...) FALSE)
  expect_error(do.call(register_handler, c(args, list(lost = character()))))
  expect_error(do.call(register_handler, c(args, list(lost = ""))))
  expect_false("bad_lost" %in% handlers())
})
