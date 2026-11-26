# Verdicts around the APA set (ADR 0032): the tier comes from each handler's declared
# additions, a covered addition rests on an exact identity, and typed numbers get a verdict.

test_that("verdicts() names the three tiers and no synthetic one", {
  expect_identical(verdicts(), c("apa_suffices", "apa_plus_suffices", "needs_raw_data"))
})

test_that("the tier per v1 test follows from the declared additions", {
  tiers <- stats::setNames(coverage_table()$verdict, coverage_table()$test)
  expect_identical(tiers[["Simple regression"]], "apa_suffices")
  expect_identical(tiers[["Paired-samples t-test"]], "apa_plus_suffices")
  plus <- c("Multiple regression", "Moderation", "Simple mediation", "Chi-squared goodness of fit")
  for (test in plus) {
    expect_identical(tiers[[test]], "apa_plus_suffices", info = test)
  }
  exact <- c("Independent-samples t-test (Student, Welch)", "One-sample t-test",
             "Pearson correlation test", "One-way ANOVA (between-subjects)")
  for (test in exact) {
    expect_identical(tiers[[test]], "apa_suffices", info = test)
  }
  for (name in handlers()) {
    h <- get_handler(name)
    open <- Filter(function(a) is.null(a$covered), h$additions)
    expect_identical(h$verdict, if (length(open) == 0L) "apa_suffices" else "apa_plus_suffices",
                     info = name)
  }
})

test_that("coverage_table() has a verdict column and lists the covered additions", {
  ct <- coverage_table()
  expect_true(all(ct$verdict %in% verdicts()))
  expect_true(all(c("verdict", "missing", "covered") %in% names(ct)))
  simple <- ct[ct$test == "Simple regression", ]
  expect_match(simple$covered, "^r \\(")
  expect_identical(simple$missing, "—")
})

test_that("the simple-regression identities for r hold at 1e-12", {
  fit <- lm(mpg ~ wt, data = mtcars)
  r <- cor(mtcars$wt, mtcars$mpg)
  s <- summary(fit)
  tval <- s$coefficients["wt", "t value"]
  expect_equal(unname(coef(lm(scale(mpg) ~ scale(wt), data = mtcars))[2]), r, tolerance = 1e-12)
  expect_equal(sign(coef(fit)[["wt"]]) * sqrt(s$r.squared), r, tolerance = 1e-12)
  expect_equal(tval / sqrt(tval^2 + fit$df.residual), r, tolerance = 1e-12)
})

test_that("a goodness-of-fit test needs the H0 proportions only when they are not equal", {
  d <- data.frame(cyl = mtcars$cyl)
  uniform <- sufficient_stats(~cyl, data = d, test = "gof")
  expect_identical(uniform$verdict, "apa_suffices")
  skewed <- sufficient_stats(~cyl, data = d, test = "gof", p0 = c(0.5, 0.3, 0.2))
  expect_identical(skewed$verdict, "apa_plus_suffices")
  expect_identical(names(skewed$additions), "p0")
})

test_that("a paired t from data names r and the range the APA numbers imply", {
  wide <- data.frame(a = sleep$extra[1:10], b = sleep$extra[11:20])
  s <- sufficient_stats(Pair(a, b) ~ 1, data = wide, test = "paired")
  expect_identical(s$verdict, "apa_plus_suffices")
  add <- s$additions$r
  expect_identical(add$status, "implied")
  r <- cor(wide$a, wide$b)
  expect_true(add$range[1] <= r && r <= add$range[2])
  expect_match(paste(sufficient_stats_narration(s), collapse = "\n"),
               "r is implied between .* by the APA numbers; reporting it makes the result exact")
})

test_that("every verdict ends with the raw-data-standard line", {
  last <- function(x) utils::tail(sufficient_stats_narration(x), 1L)
  fits <- list(
    sufficient_stats(lm(mpg ~ wt, mtcars)),
    sufficient_stats(lm(mpg ~ wt + hp, mtcars)),
    sufficient_stats(glm(am ~ wt, mtcars, family = binomial)),
    sufficient_stats(test = "paired", n = "30", m = c("5.1", "4.6"), sd = c("1.2", "1.3"))
  )
  for (s in fits) expect_match(last(s), "remains the standard.*1\\.14.*8\\.14")
})

test_that("synthetic data is only named, outside reprokit, for an unrecognised analysis", {
  glm_stats <- sufficient_stats(glm(am ~ wt, mtcars, family = binomial))
  glm_text <- paste(sufficient_stats_narration(glm_stats), collapse = "\n")
  expect_match(glm_text, "synthetic data.*outside reprokit")
  small <- sufficient_stats(lm(mpg ~ wt, mtcars[1:6, ]))
  expect_false(grepl("synthetic", paste(sufficient_stats_narration(small), collapse = "\n")))
})

test_that("typed numbers give a verdict, hold no summaries and refuse an appendix", {
  s <- sufficient_stats(test = "paired", n = "30", m = c("5.1", "4.6"), sd = c("1.2", "1.3"),
                        t = "2.1", df = "29")
  expect_s3_class(s, "reprokit_sufficient_stats")
  expect_identical(s$verdict, "apa_plus_suffices")
  expect_identical(s$meta$route, "typed")
  expect_null(s$stats)
  expect_identical(s$additions$r$status, "implied")
  expect_error(appendix(s, sharing_reason = "x"), class = "reprokit_error_input")
  expect_error(appendix(s, sharing_reason = "x"), "model or data")
  # The first argument can be the test; strings keep their decimals.
  expect_identical(sufficient_stats("pearson", n = "30", r = ".42")$verdict, "apa_suffices")
  # A reported addition is marked present.
  r_given <- sufficient_stats(test = "paired", n = "30", m = c("5.1", "4.6"),
                              sd = c("1.2", "1.3"), r = ".55")
  expect_identical(r_given$additions$r$status, "present")
  # Unknown and inapplicable names are input errors, as in check_reported().
  expect_error(sufficient_stats(test = "paired", n = "30", bogus = "1"),
               class = "reprokit_error_input")
  expect_error(sufficient_stats(test = "pearson", n = "30", d = ".5"),
               class = "reprokit_error_input")
  expect_error(sufficient_stats(test = "nonsense", n = "30"), class = "reprokit_error_input")
  # The formula route is unchanged.
  expect_identical(sufficient_stats(extra ~ group, data = sleep, test = "welch")$meta$route,
                   "formula")
})

test_that("check_reported() ends with one line saying what to report", {
  x <- check_reported("paired", n = "30", m = c("5.1", "4.6"), sd = c("1.2", "1.3"),
                      t = "2.1", df = "29", p = ".044")
  out <- utils::capture.output(print(x))
  expect_identical(utils::tail(out, 1L), "Also report r (to level 3) to make this fully checkable.")
  expect_false(any(grepl("would raise the level", out)))
})

test_that("the appendix extras use typeset symbols, eta squared first for a t test", {
  labels <- vapply(t_extras(), function(e) e$label, character(1))
  expect_identical(unname(labels), c("η²", "R²", "ω²"))
  out <- appendix(extra ~ group, data = sleep, test = "welch", extra = c("omega2", "eta2"),
                  sharing_reason = "x")
  expect_match(paste(out, collapse = "\n"), "η²")
  expect_false(grepl("eta2|omega2", paste(out, collapse = "\n")))
})
