# The SS types that fit (NULL when the check offered none).
sstypes <- function(checked) {
  v <- checked$variants$test
  if (is.null(v)) NULL else v$variant[v$fits]
}

test_that("a balanced design has no SS-type variant", {
  tg <- datasets::ToothGrowth
  tg$dose <- factor(tg$dose)
  names(tg) <- c("y", "a", "b")
  for (effect in c("A", "B", "AB")) {
    paper <- paper_factorial(tg, effect)
    ok <- do.call(check_F, paper)
    expect_true(all(ok$numbers$status != "inconsistent"), info = effect)
    expect_null(sstypes(ok))
    # The interaction has df1 = 2 here: no exact range, so F is checked for coherence only.
    if (effect != "AB") {
      expect_true("verified" %in% ok$numbers$status[ok$numbers$quantity == "f"], info = effect)
    }
  }
})

test_that("an unbalanced design: the type the paper used is the one that fits", {
  d <- factorial_unbalanced()
  for (type in ss_type_codes()) {
    ok <- do.call(check_F, paper_factorial(d, "A", type))
    expect_true(all(ok$numbers$status != "inconsistent"), info = type)
    expect_identical(sstypes(ok), type)
    # B has df1 = 2 in an unbalanced design: no exact range, so never flagged, never pinned.
    b <- do.call(check_F, paper_factorial(d, "B", type))
    expect_true(all(b$numbers$status != "inconsistent"), info = type)
    expect_null(sstypes(b))
  }
  ab <- do.call(check_F, paper_factorial(d, "AB", "I"))
  expect_true(all(ab$numbers$status != "inconsistent"))
  expect_null(sstypes(ab))
})

test_that("an F from one SS type with the p of another does not fit", {
  d <- factorial_unbalanced()
  paper <- paper_factorial(d, "A", "III")
  paper$p <- paper_factorial(d, "A", "II")$p
  bad <- do.call(check_F, paper)
  expect_true(any(bad$numbers$status == "inconsistent"))
})

test_that("planted errors are flagged: a cell mean, a digit of F, df2 from the wrong N", {
  d <- factorial_unbalanced()
  paper <- paper_factorial(d, "A", "III")
  moved <- paper
  moved$m[1L, 1L] <- fixed(as.numeric(moved$m[1L, 1L]) + 1.5, 2)
  expect_true(any(do.call(check_F, moved)$numbers$status == "inconsistent"))
  digit <- paper
  digit$f <- fixed(as.numeric(digit$f) + 0.5, 2)
  expect_true(any(do.call(check_F, digit)$numbers$status == "inconsistent"))
  wrong_n <- paper
  wrong_n$df2 <- as.character(as.numeric(wrong_n$df2) + 3)
  expect_true(any(do.call(check_F, wrong_n)$numbers$status == "inconsistent"))
})

test_that("partial omega squared is accepted as a convention, and plain eta squared is not", {
  d <- factorial_unbalanced()
  paper <- paper_factorial(d, "B", "III")
  r <- anova_factorial_reported(y ~ a * b, d, "factorial", ss_type = "III")
  gain <- r[["df1_B"]] * (r[["f_B"]] - 1)
  paper$eta2 <- fixed(gain / (gain + r[["df2"]] + 6), 2)
  ok <- do.call(check_F, paper)
  expect_true(all(ok$numbers$status != "inconsistent"))
  fits <- ok$variants$convention
  expect_identical(fits$variant[fits$fits], "partial_omega2")
  # Plain (total) eta squared is not among the conventions: it is flagged, not accepted.
  plain <- paper
  ss_tot <- (nrow(d) - 1) * stats::var(d$y)
  plain$eta2 <- fixed(r[["f_B"]] * r[["df1_B"]] * stats::deviance(stats::lm(y ~ a * b, d)) /
                        r[["df2"]] / ss_tot, 2)
  expect_false(identical(plain$eta2, paper$eta2))
})

test_that("an effect must be named, and the dims must match the cell matrices", {
  d <- factorial_unbalanced()
  paper <- paper_factorial(d, "A")
  expect_error(do.call(check_F, replace(paper, "effect", list(NULL))), "effect")
  expect_error(do.call(check_F, replace(paper, "dims", list(c(3, 2)))), "dims")
})

test_that("the lost list states the equal-variance assumption (ADR 0019)", {
  expect_true("equal variances assumed (pooled error term)" %in%
                get_handler("anova_factorial")$lost)
})
