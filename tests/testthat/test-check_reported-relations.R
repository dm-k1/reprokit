# Upgrade hints, Student df from n, d and eta squared from t, R squared.

st <- function(checked, quantity) checked$numbers$status[checked$numbers$quantity == quantity]
how <- function(checked, quantity) {
  checked$numbers$checked_against[checked$numbers$quantity == quantity]
}

test_that("a coherent number names the relation that would verify it", {
  checked <- check_reported("student", t = "2.10", df = "38", p = "0.042")
  expect_equal(st(checked, "t"), "coherent")
  expect_match(how(checked, "t"), "t_from_descriptives would verify it \\(needs n, m, sd\\)")
  expect_false(grepl("would verify", how(checked, "p")))
})

test_that("Student df is checked against n only when the test is declared", {
  ok <- check_reported("student", n = c("20", "20"), t = "2.10", df = "38", p = "0.042")
  expect_true(ok$consistent)
  expect_equal(st(ok, "df"), "verified")
  bad <- check_reported("student", n = c("20", "20"), t = "2.10", df = "37", p = "0.042")
  expect_false(bad$consistent)
  expect_equal(st(bad, "df"), "inconsistent")
})

test_that("an honest Welch result is not held to Student df from n", {
  g1 <- c(5.1, 6.3, 4.8, 7.2, 5.9, 6.6, 5.0, 6.1)
  g2 <- c(7.9, 9.4, 6.2, 10.8, 8.1, 9.9, 7.5, 12.0, 6.8, 9.2, 8.8, 7.7)
  r <- t.test(g1, g2)
  shown <- function(x) format(x, digits = 4)
  rep <- function(test) {
    check_reported(test, n = c("8", "12"), t = shown(r$statistic), df = shown(r$parameter),
                   p = shown(r$p.value))
  }
  expect_false(isFALSE(rep("welch")$consistent))
})

test_that("d from t and n cannot falsify", {
  good <- check_reported("student", n = c("20", "20"), t = "2.10", df = "38",
                         p = "0.042", d = "0.66")
  expect_true(good$consistent)
  off <- check_reported("student", n = c("20", "20"), t = "2.10", df = "38",
                        p = "0.042", d = "0.20")
  expect_true(off$consistent)
  expect_equal(st(off, "d"), "not reproducible")
})

test_that("eta squared of a t test follows from t and df", {
  ok <- check_reported("student", t = "2.10", df = "38", p = "0.042", eta2 = "0.10")
  expect_true(ok$consistent)
  expect_equal(st(ok, "eta2"), "coherent")
  off <- check_reported("student", t = "2.10", df = "38", p = "0.042", eta2 = "0.50")
  expect_true(off$consistent)
  expect_equal(st(off, "eta2"), "not reproducible")
})

test_that("Pearson r squared follows from r", {
  ok <- check_reported("pearson", r = ".50", r2 = ".25", n = "30", p = ".005")
  expect_true(ok$consistent)
  bad <- check_reported("pearson", r = ".50", r2 = ".40", n = "30", p = ".005")
  expect_false(bad$consistent)
  expect_equal(st(bad, "r2"), "inconsistent")
})

test_that("mediation model R squared, F and p are checked per model", {
  ok <- check_reported("mediation", n = "100",
                       r2 = c(a = ".25"), f = c(a = "32.67"), df1 = c(a = "1"),
                       df2 = c(a = "98"), p_model = c(a = "< .001"))
  expect_true(ok$consistent)
  bad <- check_reported("mediation", n = "100",
                        r2 = c(a = ".25"), f = c(a = "50.00"), df1 = c(a = "1"), df2 = c(a = "98"))
  expect_false(bad$consistent)
})

test_that("requires_fixed must name a kind of the relation's variants", {
  expect_error(relation("x", "a", "b", function(vals, variant) 1, variants = "s",
                        variant_kind = "test", requires_fixed = "nope"))
})
