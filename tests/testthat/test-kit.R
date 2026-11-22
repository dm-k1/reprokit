test_that("a valid kit is created and printed", {
  k <- do.call(new_kit, valid_kit_args())
  expect_true(is_kit(k))
  expect_s3_class(k, "reprokit_kit")
  expect_output(print(k), "What readers lose")
})

test_that("a kit cannot exist without a sharing reason", {
  args <- valid_kit_args()
  args$sharing_reason <- "   "
  expect_error(do.call(new_kit, args), "sharing_reason")
})

test_that("a kit cannot exist without a list of what is lost", {
  args <- valid_kit_args()
  args$lost <- character()
  expect_error(do.call(new_kit, args), "lost")
})

test_that("only the three verdicts are accepted", {
  args <- valid_kit_args()
  args$verdict <- "probably_fine"
  expect_error(do.call(new_kit, args), "verdict")
})

test_that("statistics are stored at full precision", {
  args <- valid_kit_args()
  args$stats <- list(mean = pi)
  expect_identical(do.call(new_kit, args)$stats$mean, pi)
})

test_that("meta gets the documented defaults", {
  k <- do.call(new_kit, valid_kit_args())
  expect_named(k$meta, c("n", "contrasts", "effect_size_conventions", "rounding", "ss_type"),
               ignore.order = TRUE)
})
