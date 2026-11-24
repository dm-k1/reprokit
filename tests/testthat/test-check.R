# Round-trip tests: real analyses, numbers rounded as a paper would, then checked.

test_that("a complete paired t-test report is consistent at level 3", {
  checked <- run_check("t_paired", paper_paired())
  expect_s3_class(checked, "reprokit_check")
  expect_true(checked$consistent)
  expect_equal(checked$level, 3L)
  expect_false(any(checked$numbers$status %in% c("inconsistent", "not checkable")))
  expect_equal(status_of(checked, "t"), "verified")
  expect_equal(status_of(checked, "p"), "coherent")
  # An input to a holding relation is coherent; only an output recomputed from
  # descriptives is verified.
  expect_equal(status_of(checked, "n"), "coherent")
  expect_equal(status_of(checked, "r"), "coherent")
})

test_that("without r the paired report is level 1 and implies a range for r", {
  paper <- paper_paired()
  checked <- run_check("t_paired", paper, drop = c("r", "ci_lower", "ci_upper"))
  expect_true(checked$consistent)
  expect_equal(checked$level, 1L)
  expect_equal(status_of(checked, "m"), "unconfirmed")
  expect_equal(status_of(checked, "sd"), "unconfirmed")
  expect_equal(checked$numbers$checked_against[checked$numbers$quantity == "m"],
               "t_from_descriptives (needs r)")
  expect_equal(status_of(checked, "t"), "coherent")
  expect_equal(checked$missing$quantity, "r")
  expect_equal(checked$missing$would_reach_level, 3L)
  implied <- checked$implied$r
  expect_true(implied$feasible)
  expect_gte(paper$true_r, implied$lower - implied$resolution)
  expect_lte(paper$true_r, implied$upper + implied$resolution)
})

test_that("the CI adds a second constraint on r", {
  paper <- paper_paired()
  with_ci <- run_check("t_paired", paper, drop = "r")
  without_ci <- run_check("t_paired", paper, drop = c("r", "ci_lower", "ci_upper"))
  expect_lte(with_ci$implied$r$upper - with_ci$implied$r$lower,
             without_ci$implied$r$upper - without_ci$implied$r$lower + 1e-12)
  expect_gte(paper$true_r, with_ci$implied$r$lower - with_ci$implied$r$resolution)
})

test_that("a planted error in t is flagged as inconsistent", {
  paper <- paper_paired()
  genuine_t <- paper$t
  paper$t <- sub("4.06", "4.66", paper$t, fixed = TRUE)
  expect_false(identical(paper$t, genuine_t))
  checked <- run_check("t_paired", paper)
  expect_false(checked$consistent)
  expect_equal(status_of(checked, "t"), "inconsistent")
  expect_output(print(checked), "inconsistent with the reported statistics")
})

test_that("a planted error in p is flagged with only t, df and p", {
  checked <- check_reported("t_paired", t = "-4.06", df = "9", p = ".030")
  expect_false(checked$consistent)
  expect_true(all(checked$numbers$status == "inconsistent"))
})

test_that("a complete Pearson report is consistent at level 3 with nothing unchecked", {
  checked <- run_check("cor_pearson", paper_pearson())
  expect_true(checked$consistent)
  expect_equal(checked$level, 3L)
  expect_true(all(checked$numbers$status %in% c("verified", "coherent")))
  expect_equal(nrow(checked$missing), 0L)
})

test_that("a planted error in a Pearson CI bound is flagged", {
  paper <- paper_pearson()
  paper$ci_upper <- "-0.644"
  checked <- run_check("cor_pearson", paper)
  expect_false(checked$consistent)
  expect_equal(status_of(checked, "ci_upper"), "inconsistent")
})

test_that("variants report which tail fits", {
  # One-tailed p for t = -4.06, df = 9 is .0014; two-sided is .0028.
  checked <- check_t(t = "-4.06", df = "9", p = ".001")
  fits <- stats::setNames(checked$variants$tail$fits, checked$variants$tail$variant)
  expect_true(fits[["less"]])
  expect_false(fits[["two.sided"]])
  expect_false(fits[["greater"]])
  only_less <- check_t(t = "-4.06", df = "9", p = ".001", variants = "less")
  expect_equal(only_less$variants$tail$variant, "less")
  expect_true(only_less$consistent)
  expect_error(check_t(t = "-4.06", df = "9", p = ".001", variants = "both"), "Unknown variant")
})

test_that("nothing checkable gives level 0 and says so", {
  checked <- check_t(m = c("5.10", "4.20"))
  expect_equal(checked$level, 0L)
  expect_output(print(checked), "No check was possible")
})

test_that("print gives the verdict, the level and the fixed closing sentence", {
  out <- paste(utils::capture.output(print(run_check("t_paired", paper_paired()))),
               collapse = "\n")
  expect_match(out, "consistent with the reported statistics")
  expect_match(out, "Level 3")
  expect_match(out, "cannot all be correct as reported, not which one is wrong or why")
  expect_false(grepl("error", out, ignore.case = TRUE))
})

test_that("input errors are clear", {
  expect_error(check_reported("t_paired", "2.85"), "named by its quantity")
  expect_error(check_reported(t = "2.85"), "Name the handler first")
  expect_error(check_reported("t_paired", tstat = "2.85"), "Unknown reported quantity")
  expect_error(check_reported("nope", t = "2.85"), "No handler registered")
  expect_error(check_reported("lm", t = "2.85"),
               paste0("no relations.*Handlers that do: anova_oneway, cor_pearson, ",
                      "t_independent, t_one_sample, t_paired"))
})

test_that("a confidence level is reported apart from the tails", {
  checked <- run_check("cor_pearson", paper_pearson())
  expect_named(checked$variants, c("tail", "level"))
  fitting_levels <- checked$variants$level
  expect_equal(fitting_levels$variant, c("0.90", "0.95", "0.99"))
  expect_equal(fitting_levels$variant[fitting_levels$fits], "0.95")
  out <- paste(utils::capture.output(print(checked)), collapse = "\n")
  expect_match(out, "Variants that fit: two-sided, one-sided (less)\n", fixed = TRUE)
  expect_match(out, "Confidence levels that fit: 95%\n", fixed = TRUE)
  tails_only <- do.call(check_reported, c(list(handler = "cor_pearson", variants = "less"),
                                          paper_pearson()[c("n", "r", "df", "p",
                                                            "ci_lower", "ci_upper")]))
  expect_equal(tails_only$variants$tail$variant, "less")
  expect_equal(tails_only$variants$level$variant[tails_only$variants$level$fits], "0.95")
  expect_true(tails_only$consistent)
})

test_that("a number with no relation reaching it is not checkable", {
  checked <- check_t(n = "20", m = c("5.10", "4.20"), sd = c("1.80", "2.00"))
  expect_equal(status_of(checked, "m"), "not checkable")
})

test_that("r that reaches p only through an unreported t is coherent, not 'not checkable'", {
  ok <- check_r(r = ".41", df = "28", p = ".024")
  expect_equal(status_of(ok, "r"), "coherent")
  expect_equal(status_of(ok, "p"), "verified")
  expect_false("not checkable" %in% ok$numbers$status)
  bad <- check_r(r = ".41", df = "28", p = ".5")
  expect_equal(status_of(bad, "r"), "inconsistent")
  expect_equal(status_of(check_r(r = ".41", df = "28"), "r"), "not checkable")
  full <- check_r(r = ".41", n = "30", t = "2.39", df = "28", p = ".024")
  expect_equal(status_of(full, "r"), status_of(ok, "r"))
})
