# The regression family (ADR 0020): lm_simple, lm_multiple, lm_moderation, mediation_simple.

fmt2 <- function(x) formatC(x, format = "f", digits = 2L)
mt <- datasets::mtcars
mt$am <- factor(mt$am)

test_that("the registry matches each intended handler, and the rest fall back to lm", {
  expect_identical(find_handler(lm(mpg ~ wt, mt))$name, "lm_simple")
  expect_identical(find_handler(lm(mpg ~ wt + hp + am, mt))$name, "lm_multiple")
  expect_identical(find_handler(lm(mpg ~ wt * hp, mt))$name, "lm_moderation")
  expect_identical(find_handler(lm(mpg ~ poly(wt, 2), mt))$name, "lm")
  expect_identical(find_handler(lm(mpg ~ wt * hp * disp, mt))$name, "lm")
  expect_identical(find_handler(lm(mpg ~ wt - 1, mt))$name, "lm")
  expect_identical(find_handler(lm(mpg ~ wt * am, mt))$name, "lm_moderation")
  expect_identical(find_handler(lm(mpg ~ wt + hp, mt, weights = cyl))$name, "lm_multiple")
  # the general handler has no coverage row and no check route
  expect_null(get_handler("lm")$design)
})

test_that("the three lm rows and mediation are in the coverage table", {
  ct <- coverage_table()
  for (row in c("Simple regression", "Multiple regression", "Simple mediation")) {
    expect_true(row %in% ct$test, info = row)
  }
  expect_true(any(grepl("oderation", ct$test)))
  med <- ct[ct$test == "Simple mediation", ]
  expect_match(med$not_checkable, "bootstrap, which resamples cases")
  expect_identical(med$choices, "confidence level (90%, 95%, 99%)")
})

test_that("the mediation coverage row says what plan section 4 says", {
  med <- coverage_table()
  med <- med[med$test == "Simple mediation", ]
  expect_identical(med$sufficient, "n, M, SD and correlations of X, M, Y")
  expect_identical(med$missing, "the correlations of X, M, Y")
  expect_identical(med$checkable, "a, b, c, c\u2032 (b, SE, t, p, CI each)")
})

test_that("each handler round-trips through share and reproduce", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + hp + am, mt), lm(mpg ~ wt * hp, mt),
               lm(mpg ~ wt + hp + am, mt, weights = cyl))
  for (f in fits) {
    s <- sufficient_stats(f, sharing_reason = "test")
    expect_true(nzchar(s$lost[[1L]]))
    h <- get_handler(s$handler)
    orig <- h$reported(f)
    back <- reproduce(s)
    expect_identical(names(orig), names(back))
    expect_lt(max(abs(orig - back) / pmax(1, abs(orig))), 1e-8)
  }
})

test_that("mediation reproduces the four paths of the three regressions", {
  s <- sufficient_stats(mpg ~ wt, data = mt, test = "mediation", mediator = "hp",
                        sharing_reason = "test")
  expect_identical(s$handler, "mediation_simple")
  expect_true(length(s$lost) > 0L && any(grepl("bootstrap", s$lost)))
  r <- reproduce(s)
  a <- summary(lm(hp ~ wt, mt))$coefficients
  b <- summary(lm(mpg ~ wt + hp, mt))$coefficients
  cc <- summary(lm(mpg ~ wt, mt))$coefficients
  expect_equal(unname(r[["estimate:a"]]), a["wt", 1L], tolerance = 1e-8)
  expect_equal(unname(r[["se:b"]]), b["hp", 2L], tolerance = 1e-8)
  expect_equal(unname(r[["t:c_prime"]]), b["wt", 3L], tolerance = 1e-8)
  expect_equal(unname(r[["p:c"]]), cc["wt", 4L], tolerance = 1e-8)
  expect_false(any(grepl("indirect|sobel", names(r), ignore.case = TRUE)))
})

test_that("a mediation without a mediator errors", {
  expect_error(sufficient_stats(mpg ~ wt, data = mt, test = "mediation", sharing_reason = "t"))
})

paper_multiple <- list(
  b = c(wt = "-3.88", hp = "-0.03"), se = c(wt = "0.63", hp = "0.01"),
  t = c(wt = "-6.13", hp = "-3.52"), p = c(wt = "< .001", hp = ".001"))

test_that("t = b / SE and p from t are checked, and a planted digit is flagged", {
  ok <- do.call(check_lm, paper_multiple)
  expect_true(ok$consistent)
  bad <- paper_multiple
  bad$t["wt"] <- "-9.13"
  expect_false(do.call(check_lm, c(bad, list(df2 = "29")))$consistent)
  bad <- paper_multiple
  bad$p["hp"] <- ".450"
  expect_false(do.call(check_lm, c(bad, list(df2 = "29")))$consistent)
})

test_that("the CI is checked from b, SE, df and level", {
  f <- lm(mpg ~ wt + hp, mt)
  ci <- confint(f, level = 0.90)
  cs <- summary(f)$coefficients
  paper <- list(b = c(wt = fmt2(cs["wt", 1L])), se = c(wt = fmt2(cs["wt", 2L])),
                ci_lower = c(wt = fmt2(ci["wt", 1L])), ci_upper = c(wt = fmt2(ci["wt", 2L])),
                df2 = "29")
  ok <- do.call(check_lm, paper)
  expect_true(ok$consistent)
  paper$ci_upper["wt"] <- fmt2(ci["wt", 2L] + 0.5)
  expect_false(do.call(check_lm, paper)$consistent)
})

test_that("F is checked from R2, df1 and df2, and the model p from F", {
  f <- summary(lm(mpg ~ wt + hp, mt))
  fs <- f$fstatistic
  paper <- list(r2 = ".83", f = fmt2(fs[["value"]]), df1 = "2", df2 = "29")
  expect_true(do.call(check_lm, paper)$consistent)
  paper$f <- "40.00"
  expect_false(do.call(check_lm, paper)$consistent)
  paper$f <- fmt2(fs[["value"]])
  paper$p_model <- ".900"
  expect_false(do.call(check_lm, paper)$consistent)
})

test_that("adjusted R2 reported as R2 is not flagged, and says so", {
  f <- summary(lm(mpg ~ wt + hp, mt))
  paper <- list(r2 = sub("^0", "", fmt2(f$adj.r.squared)), f = fmt2(f$fstatistic[["value"]]),
                df1 = "2", df2 = "29")
  ck <- do.call(check_lm, paper)
  expect_true(ck$consistent)
})

test_that("n is implied by df2 only when k is declared or read from df1", {
  no_k <- check_lm(df2 = "29", n = "32")
  expect_identical(no_k$numbers$status[no_k$numbers$quantity == "n"], "not checkable")
  with_k <- check_lm(df2 = "29", n = "32", k = 2L)
  expect_false(with_k$numbers$status[with_k$numbers$quantity == "n"] == "not checkable")
  wrong_k <- check_lm(df2 = "29", n = "32", k = 5L)
  expect_false(wrong_k$consistent)
  from_df1 <- check_lm(df1 = "2", df2 = "29", n = "32")
  expect_true(from_df1$consistent)
  expect_false(check_lm(df1 = "4", df2 = "29", n = "32")$consistent)
})

test_that("k is never guessed from the number of terms", {
  # two terms reported, but the model has a third the paper left out: n = 32 fits k = 3
  ck <- check_lm(b = c(wt = "-3.88", hp = "-0.03"), df2 = "28", n = "32")
  expect_true(ck$consistent)
})

test_that("simple regression: R2 = r^2, t^2 = F and r = b SD_x / SD_y", {
  f <- lm(mpg ~ wt, mt)
  cs <- summary(f)$coefficients
  r <- cor(mt$mpg, mt$wt)
  paper <- list(b = fmt2(cs[2L, 1L]), se = fmt2(cs[2L, 2L]), t = fmt2(cs[2L, 3L]),
                r2 = fmt2(r^2), f = fmt2(cs[2L, 3L]^2), r = fmt2(r),
                sd = c(fmt2(sd(mt$wt)), fmt2(sd(mt$mpg))))
  ok <- do.call(check_lm, c(paper, list(handler = "lm_simple")))
  expect_true(ok$consistent)
  bad <- paper
  bad$r2 <- ".60"
  bad$n <- "32"
  expect_false(do.call(check_lm, c(bad, list(handler = "lm_simple")))$consistent)
  bad <- paper
  bad$f <- "99.00"
  expect_false(do.call(check_lm, c(bad, list(handler = "lm_simple")))$consistent)
})

test_that("an undeclared argument errors", {
  expect_error(check_reported(handler = "lm_multiple", df2 = "29", bogus = 3), "bogus|argument")
  expect_error(check_lm(b = c(wt = "1"), handler = "nonsense"))
  expect_error(check_lm(b = "1.0"), "name them")
})

test_that("a standardised beta that does not fit is not reproducible, never inconsistent", {
  ck <- check_lm(b = c(wt = "-3.88"), beta = c(wt = "-0.99"),
                 sd = c(wt = "0.98", mpg = "6.03"))
  expect_true(ck$consistent)
  expect_true(any(grepl("beta", ck$notes)))
  expect_identical(ck$numbers$status[ck$numbers$quantity == "beta:wt"], "not reproducible")
})

test_that("mediation: each path is checked, and the indirect effect never fits or fails", {
  ok <- check_mediation(b = c(a = "1.0", c = "0.5"), se = c(a = "0.2", c = "0.1"),
                        t = c(a = "5.00", c = "5.00"), n = "50", indirect = "0.3",
                        indirect_ci_lower = "0.1", indirect_ci_upper = "0.5")
  expect_true(ok$consistent)
  ind <- ok$numbers[grepl("indirect", ok$numbers$quantity), ]
  expect_true(all(ind$status == "not checkable"))
  expect_true(any(grepl("bootstrap", ok$notes)))
  # an absurd a*b changes nothing
  odd <- check_mediation(b = c(a = "1.0"), se = c(a = "0.2"), t = c(a = "5.00"), n = "50",
                         indirect = "99", indirect_ci_lower = "-7", indirect_ci_upper = "-6")
  expect_true(odd$consistent)
  expect_false(any(c("inconsistent", "verified", "coherent") %in%
                     odd$numbers$status[grepl("indirect", odd$numbers$quantity)]))
  # a path with a wrong t is flagged
  bad <- check_mediation(b = c(a = "1.0"), se = c(a = "0.2"), t = c(a = "9.00"))
  expect_false(bad$consistent)
})

test_that("mediation: the wrong df for a path is flagged, the right one is not", {
  f <- summary(lm(mpg ~ wt + hp, mt))$coefficients
  mk <- function(df) check_mediation(b = c(b = fmt2(f["hp", 1L])), se = c(b = fmt2(f["hp", 2L])),
                                     t = c(b = fmt2(f["hp", 3L])), p = c(b = fmt3(f["hp", 4L])),
                                     df = c(b = df))
  fmt3 <- function(x) formatC(x, format = "f", digits = 3L)
  expect_true(mk("29")$consistent)
  expect_false(mk("5")$consistent)
})

# The three mediation regressions with their coefficient CIs from confint(), per path.
mediation_paper <- function(level = 0.95, digits = 2L) {
  fits <- list(a = list(lm(hp ~ wt, mt), "wt"), b = list(lm(mpg ~ wt + hp, mt), "hp"),
               c = list(lm(mpg ~ wt, mt), "wt"), c_prime = list(lm(mpg ~ wt + hp, mt), "wt"))
  per <- function(f) vapply(fits, f, character(1))
  list(
    b = per(function(x) fmt2(coef(x[[1L]])[[x[[2L]]]])),
    se = per(function(x) fmt2(summary(x[[1L]])$coefficients[x[[2L]], 2L])),
    t = per(function(x) fmt2(summary(x[[1L]])$coefficients[x[[2L]], 3L])),
    df = per(function(x) as.character(x[[1L]]$df.residual)),
    ci_lower = per(function(x) fmt2(confint(x[[1L]], level = level)[x[[2L]], 1L])),
    ci_upper = per(function(x) fmt2(confint(x[[1L]], level = level)[x[[2L]], 2L])))
}

test_that("mediation: a correct path CI fits, at 90, 95 and 99 percent", {
  for (level in c(0.90, 0.95, 0.99)) {
    ck <- do.call(check_mediation, mediation_paper(level))
    expect_true(ck$consistent, info = as.character(level))
    lv <- ck$variants$level
    expect_true(lv$fits[lv$variant == sprintf("%.2f", level)], info = as.character(level))
    ci <- ck$numbers[grepl("^ci_", ck$numbers$quantity), ]
    expect_identical(nrow(ci), 8L)
    expect_true(all(ci$status == "coherent"))
  }
})

test_that("mediation: a wrong CI bound on any path is flagged", {
  for (path in c("a", "b", "c", "c_prime")) {
    paper <- mediation_paper()
    paper$ci_upper[[path]] <- fmt2(as.numeric(paper$ci_upper[[path]]) + 1)
    expect_false(do.call(check_mediation, paper)$consistent, info = path)
  }
  # bounds from two different levels fit no single level
  paper <- mediation_paper(0.95)
  paper$ci_upper <- mediation_paper(0.99)$ci_upper
  expect_false(do.call(check_mediation, paper)$consistent)
})

test_that("mediation: a reported CI is optional, and a*b still never fits or fails", {
  paper <- mediation_paper()
  paper$ci_lower <- NULL
  paper$ci_upper <- NULL
  expect_true(do.call(check_mediation, paper)$consistent)
  paper <- c(mediation_paper(), list(indirect = "-99", indirect_ci_lower = "-98",
                                     indirect_ci_upper = "-97"))
  ck <- do.call(check_mediation, paper)
  expect_true(ck$consistent)
  expect_true(all(ck$numbers$status[grepl("indirect", ck$numbers$quantity)] == "not checkable"))
})

test_that("the appendix works for the four handlers", {
  sr <- "Consent did not cover public sharing."
  for (s in list(sufficient_stats(lm(mpg ~ wt, mt), sharing_reason = sr),
                 sufficient_stats(lm(mpg ~ wt + hp + am, mt), sharing_reason = sr),
                 sufficient_stats(lm(mpg ~ wt * hp, mt), sharing_reason = sr),
                 sufficient_stats(mpg ~ wt, data = mt, test = "mediation", mediator = "hp",
                                  sharing_reason = sr))) {
    expect_no_error(appendix(s))
  }
})
