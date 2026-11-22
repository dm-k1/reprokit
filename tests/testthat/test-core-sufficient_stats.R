test_that("a valid object is created and printed", {
  k <- do.call(new_sufficient_stats, valid_stats_args())
  expect_true(is_sufficient_stats(k))
  expect_s3_class(k, "reprokit_sufficient_stats")
  expect_output(print(k), "What readers lose")
})

test_that("the object cannot exist without a sharing reason", {
  args <- valid_stats_args()
  args$sharing_reason <- "   "
  expect_error(do.call(new_sufficient_stats, args), "sharing_reason")
})

test_that("the object cannot exist without a list of what is lost", {
  args <- valid_stats_args()
  args$lost <- character()
  expect_error(do.call(new_sufficient_stats, args), "lost")
})

test_that("only the three verdicts are accepted", {
  args <- valid_stats_args()
  args$verdict <- "probably_fine"
  expect_error(do.call(new_sufficient_stats, args), "verdict")
})

test_that("statistics are stored at full precision", {
  args <- valid_stats_args()
  args$stats <- list(mean = pi)
  expect_identical(do.call(new_sufficient_stats, args)$stats$mean, pi)
})

test_that("meta gets the documented defaults", {
  k <- do.call(new_sufficient_stats, valid_stats_args())
  expect_named(k$meta, c("n", "contrasts", "effect_size_conventions", "rounding", "ss_type"),
               ignore.order = TRUE)
})
