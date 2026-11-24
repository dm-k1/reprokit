test_that("a decimal string gives its rounding interval", {
  v <- reported_value("2.85")
  expect_equal(v$point, 2.85)
  expect_equal(c(v$lower, v$upper), c(2.845, 2.855))
  expect_equal(v$relation, "=")
})

test_that("a leading-dot string is read, and its decimals set the precision", {
  v <- reported_value(".008")
  expect_equal(c(v$point, v$lower, v$upper), c(0.008, 0.0075, 0.0085))
  v <- reported_value("-0.50")
  expect_equal(c(v$lower, v$upper), c(-0.505, -0.495))
})

test_that("inequalities give open-ended intervals", {
  v <- reported_value("< .001")
  expect_equal(v$relation, "<")
  expect_equal(c(v$lower, v$upper), c(0, 0.001))
  v <- reported_value("> .05")
  expect_equal(v$relation, ">")
  expect_equal(c(v$lower, v$upper), c(0.05, 1))
})

test_that("a string without a decimal point is an exact count", {
  v <- reported_value("29")
  expect_equal(c(v$point, v$lower, v$upper), c(29, 29, 29))
})

test_that("a character vector gives a list with names carried", {
  v <- reported_value(c(first = "5.10", second = "4.2"))
  expect_named(v, c("first", "second"))
  expect_equal(v$first$upper, 5.105)
  expect_equal(v$second$lower, 4.15)
})

test_that("numeric input needs digits unless it is an integer", {
  expect_equal(reported_value(29)$upper, 29)
  v <- reported_value(2.853, digits = 2)
  expect_equal(c(v$point, v$lower, v$upper), c(2.85, 2.845, 2.855))
  expect_error(reported_value(2.85), "string.*digits")
})

test_that("unreadable input is refused with a useful message", {
  expect_error(reported_value("abc"), "Cannot read 'abc'")
  expect_error(reported_value("1.2.3"), "Cannot read")
  expect_error(reported_value(NA_character_), "NA")
  expect_error(reported_value(TRUE), "string")
  expect_error(reported_value("2.85", digits = 2), "numeric")
})

test_that("quantity_names lists the vocabulary and the validator uses it", {
  expect_equal(quantity_names(), c("n", "m", "sd", "r", "t", "f", "chisq", "df", "df1", "df2",
                                   "p", "eta2", "ci_lower", "ci_upper", "d"))
  expect_error(check_quantity_names("tstat"), "Unknown quantity name: 'tstat'.*n, m, sd")
  expect_identical(check_quantity_names(c("t", "p")), c("t", "p"))
})
