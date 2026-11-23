# n implied from df: the usual APA form r(28) = .41, p = .024 (no n reported).

pearson_apa <- function() {
  paper <- paper_pearson() # nolint: object_usage_linter
  paper[c("r", "df", "p", "ci_lower", "ci_upper")]
}

test_that("r, df and p from a real cor.test() is checkable and coherent, with n implied", {
  checked <- run_check("cor_pearson", pearson_apa()[c("r", "df", "p")])
  expect_true(checked$consistent)
  expect_true(checked$checked)
  # n = df + 2 is determined by df, so the sufficient set (n, r) is covered.
  expect_equal(checked$level, 3L)
  expect_false(any(checked$numbers$status == "inconsistent"))
  expect_equal(status_of(checked, "p"), "verified")
  expect_equal(status_of(checked, "df"), "coherent")
  expect_identical(names(checked$implied_inputs), "n")
  expect_equal(checked$implied_inputs$n[c("lower", "upper")], list(lower = 32, upper = 32))
  expect_false("n" %in% checked$numbers$quantity)
  expect_false("n" %in% checked$missing$quantity)
  expect_output(print(checked), "n = 32 (from df)", fixed = TRUE)
})

test_that("the form in the brief is checkable", {
  checked <- check_reported("cor_pearson", r = ".41", df = "28", p = ".024")
  expect_gt(checked$level, 0L)
  expect_true(checked$consistent)
  expect_equal(checked$implied_inputs$n$lower, 30)
})

test_that("a planted r/p mismatch is flagged when n is implied", {
  paper <- pearson_apa()[c("r", "df", "p")]
  paper$p <- "0.500"
  checked <- run_check("cor_pearson", paper)
  expect_false(checked$consistent)
  expect_equal(status_of(checked, "p"), "inconsistent")
})

test_that("no df_from_n row exists for an implied n, and df is not checked against it", {
  checked <- run_check("cor_pearson", pearson_apa()[c("r", "df", "p")])
  expect_false("df_from_n" %in% checked$relations$relation)
  expect_false(any(checked$relations$checked & checked$relations$output == "df"))
  # df is only ever an input here, never the output of a checked relation
  expect_false(any(grepl("df_from_n", checked$numbers$checked_against)))
  # nor does the skipped relation leave df "unconfirmed" when nothing else reaches it
  alone <- check_reported("cor_pearson", df = "28")
  expect_equal(status_of(alone, "df"), "not checkable")
})

test_that("the CI is checked through the implied n", {
  checked <- run_check("cor_pearson", pearson_apa())
  expect_true(checked$consistent)
  expect_true(all(c("ci_lower_from_r", "ci_upper_from_r") %in%
                    checked$relations$relation[checked$relations$checked]))
  paper <- pearson_apa()
  paper$ci_upper <- "-0.644"
  expect_false(run_check("cor_pearson", paper)$consistent)
})

test_that("paired and one-sample t work from t, df, p and descriptives without n", {
  paired <- paper_paired()
  checked <- run_check("t_paired", paired[c("m", "sd", "r", "t", "df", "p")])
  expect_true(checked$consistent)
  expect_equal(checked$implied_inputs$n$lower, 10)
  expect_equal(status_of(checked, "t"), "verified")
  expect_false("df_from_n" %in% checked$relations$relation)
  bad <- paired[c("m", "sd", "r", "t", "df", "p")]
  bad$t <- "5.85"
  expect_false(run_check("t_paired", bad)$consistent)

  x <- datasets::sleep$extra[datasets::sleep$group == 1]
  tt <- stats::t.test(x, mu = 0.5)
  one <- check_reported("t_one_sample", m = fixed(mean(x), 2), sd = fixed(sd(x), 2),
                        t = fixed(tt$statistic, 2), df = as.character(tt$parameter),
                        p = fixed(tt$p.value, 3), mu = 0.5)
  expect_true(one$consistent)
  expect_equal(one$implied_inputs$n$lower, 10)
  expect_equal(status_of(one, "t"), "verified")
  expect_false("df_from_n" %in% one$relations$relation)
})

test_that("t_independent implies nothing from df", {
  checked <- check_reported("t_independent", t = "2.10", df = "28", p = ".045")
  expect_length(checked$implied_inputs, 0L)
  expect_null(relation("x", "n", "df", function(vals, variant) vals$n)$inverse)
})

test_that("with n and df both reported, df_from_n checks df as before", {
  checked <- run_check("cor_pearson", paper_pearson())
  expect_length(checked$implied_inputs, 0L)
  expect_true("df_from_n" %in% checked$relations$relation[checked$relations$checked])
  paper <- paper_pearson()
  paper$df <- "29"
  expect_false(run_check("cor_pearson", paper)$consistent)
})

test_that("an impossible df implies nothing and a relation inverse needs one input", {
  expect_length(check_reported("cor_pearson", r = ".41", df = "28.4",
                               p = ".024")$implied_inputs, 0L)
  expect_error(relation("bad", c("n", "r"), "t", function(vals, variant) 1,
                        inverse = function(x) x))
})
