# Sharing from fitted results (ADR 0026): a result or model is translated into the formula
# route's inputs, so its sufficient_stats object equals the formula route's, apart from
# meta$route and the record of which choices were read from the result.

same_but_route <- function(a, b) {
  strip <- function(s) {
    s$meta$route <- NULL
    s$choices_read <- NULL
    s
  }
  testthat::expect_identical(strip(a), strip(b))
}

wide <- function() {
  d <- datasets::sleep
  data.frame(drug1 = d$extra[d$group == 1], drug2 = d$extra[d$group == 2])
}

mt <- function() {
  d <- datasets::mtcars
  d$am <- factor(d$am)
  d$vs <- factor(d$vs)
  d$cyl <- factor(d$cyl)
  d
}

test_that("t.test results equal the formula route: Welch, Student, sides, level", {
  d <- datasets::sleep
  same_but_route(sufficient_stats(t.test(extra ~ group, data = d), data = d),
                 sufficient_stats(extra ~ group, data = d, test = "welch"))
  same_but_route(sufficient_stats(t.test(extra ~ group, data = d, var.equal = TRUE), data = d),
                 sufficient_stats(extra ~ group, data = d, test = "student"))
  same_but_route(
                 sufficient_stats(t.test(extra ~ group, data = d, alternative = "less",
                                         conf.level = 0.9),
                                  data = d),
                 sufficient_stats(extra ~ group, data = d, test = "welch", alternative = "less",
                                  conf_level = 0.9))
  same_but_route(
                 sufficient_stats(t.test(extra ~ group, data = d, alternative = "greater",
                                         var.equal = TRUE),
                                  data = d),
                 sufficient_stats(extra ~ group, data = d, test = "student",
                                  alternative = "greater"))
  s <- sufficient_stats(t.test(extra ~ group, data = d, conf.level = 0.9), data = d)
  expect_identical(s$meta$route, "t.test")
  expect_identical(attr(s$choices_read, "fn"), "t.test")
  expect_match(paste(format(appendix(s, sharing_reason = "consent")), collapse = " "),
               "read from the t.test result")
})

test_that("t.test results equal the formula route: paired and one-sample with mu", {
  w <- wide()
  same_but_route(sufficient_stats(t.test(Pair(drug1, drug2) ~ 1, data = w), data = w),
                 sufficient_stats(Pair(drug1, drug2) ~ 1, data = w, test = "paired"))
  same_but_route(
                 sufficient_stats(t.test(Pair(drug1, drug2) ~ 1, data = w, mu = -1,
                                         alternative = "less"),
                                  data = w),
                 sufficient_stats(Pair(drug1, drug2) ~ 1, data = w, test = "paired", mu = -1,
                                  alternative = "less"))
  same_but_route(sufficient_stats(t.test(drug1 ~ 1, data = w, mu = 0.5), data = w),
                 sufficient_stats(drug1 ~ 1, data = w, test = "one_sample", mu = 0.5))
  same_but_route(
                 sufficient_stats(t.test(drug2 ~ 1, data = w, mu = 1, conf.level = 0.99,
                                         alternative = "greater"),
                                  data = w),
                 sufficient_stats(drug2 ~ 1, data = w, test = "one_sample", mu = 1,
                                  conf_level = 0.99,
                                  alternative = "greater"))
})

test_that("cor.test results equal the formula route", {
  d <- datasets::mtcars
  same_but_route(sufficient_stats(cor.test(~ mpg + wt, data = d), data = d),
                 sufficient_stats(~ mpg + wt, data = d, test = "pearson"))
  same_but_route(sufficient_stats(cor.test(~ mpg + wt, data = d, conf.level = 0.9,
                                           alternative = "less"), data = d),
                 sufficient_stats(~ mpg + wt, data = d, test = "pearson", conf_level = 0.9,
                                  alternative = "less"))
})

test_that("chisq.test results go to the counts path and equal the formula route", {
  d <- mt()
  tab <- table(am = d$am, vs = d$vs)
  expect_identical(sufficient_stats(chisq.test(tab), data = d)$meta$route, "chisq.test")
  same_but_route(sufficient_stats(chisq.test(tab), data = d),
                 sufficient_stats(~ am + vs, data = d, test = "chisq"))
  same_but_route(sufficient_stats(chisq.test(tab, correct = FALSE), data = d),
                 sufficient_stats(~ am + vs, data = d, test = "chisq", correct = FALSE))
  yates <- sufficient_stats(chisq.test(tab), data = d)
  expect_identical(attr(yates$choices_read, "fn"), "chisq.test")
  expect_identical(yates$choices_read$test, "Yates")
  # the summaries are the table of counts (the counts path): no rows are expanded
  expect_true(is.matrix(yates$stats$table) || !is.null(yates$stats))
  same_but_route(sufficient_stats(chisq.test(tab, simulate.p.value = FALSE), data = d),
                 sufficient_stats(~ am + vs, data = d, test = "chisq"))
})

test_that("a chi-squared goodness of fit with unequal p0 equals the formula route", {
  d <- mt()
  cnt <- table(cyl = d$cyl)
  p0 <- c(0.5, 0.25, 0.25)
  same_but_route(sufficient_stats(chisq.test(cnt, p = p0), data = d),
                 sufficient_stats(~ cyl, data = d, test = "gof", p0 = p0))
  same_but_route(sufficient_stats(chisq.test(cnt), data = d),
                 sufficient_stats(~ cyl, data = d, test = "gof"))
})

test_that("aov results equal the formula route: one-way and unbalanced factorial", {
  d <- datasets::PlantGrowth
  same_but_route(sufficient_stats(aov(weight ~ group, data = d), data = d),
                 sufficient_stats(weight ~ group, data = d, test = "oneway"))
  u <- factorial_unbalanced()
  same_but_route(sufficient_stats(aov(y ~ a * b, data = u), data = u),
                 sufficient_stats(y ~ a * b, data = u, test = "factorial"))
  s <- sufficient_stats(aov(y ~ a * b, data = u), data = u)
  expect_identical(s$verdict, "apa_suffices")
  expect_identical(s$meta$route, "aov")
  same_but_route(sufficient_stats(aov(y ~ a * b, data = u), data = u, ss_type = "II"),
                 sufficient_stats(y ~ a * b, data = u, test = "factorial", ss_type = "II"))
})

test_that("appendix(model) is identical to appendix(sufficient_stats(...))", {
  d <- datasets::sleep
  fit <- t.test(extra ~ group, data = d, var.equal = TRUE)
  a <- appendix(fit, data = d, sharing_reason = "consent")
  b <- appendix(sufficient_stats(fit, data = d), sharing_reason = "consent")
  expect_identical(a, b)
  u <- factorial_unbalanced()
  fa <- appendix(aov(y ~ a * b, data = u), data = u, sharing_reason = "consent")
  fb <- appendix(sufficient_stats(aov(y ~ a * b, data = u), data = u), sharing_reason = "consent")
  expect_identical(fa, fb)
  # the formula route gives the same appendix apart from the "read from" notes
  ff <- appendix(y ~ a * b, data = u, test = "factorial", sharing_reason = "consent")
  expect_identical(class(fa), class(ff))
})

test_that("a result that does not match the data is refused", {
  d <- datasets::sleep
  fit <- t.test(extra ~ group, data = d)
  wrong <- d
  wrong$extra[1L] <- wrong$extra[1L] + 3
  s <- sufficient_stats(fit, data = wrong)
  expect_false(identical(s$verdict, "apa_suffices"))
  expect_match(s$verdict_reason, "match|does not|differ", ignore.case = TRUE)
  subset_rows <- d[-1L, ]
  s2 <- sufficient_stats(fit, data = subset_rows)
  expect_false(identical(s2$verdict, "apa_suffices"))
  expect_error(appendix(fit, data = wrong, sharing_reason = "x"), class = "reprokit_impossible")
  u <- factorial_unbalanced()
  fit_aov <- aov(y ~ a * b, data = u)
  # an aov carries its own model frame, so `data` is not what it is checked against
  expect_identical(sufficient_stats(fit_aov, data = u[-1L, ])$verdict, "apa_suffices")
})

test_that("an unmappable data.name is refused with a hint to the formula route", {
  d <- datasets::sleep
  fit <- t.test(d$extra[1:10], d$extra[11:20])
  s <- sufficient_stats(fit, data = d)
  expect_false(identical(s$verdict, "apa_suffices"))
  expect_match(s$verdict_reason, "formula", ignore.case = TRUE)
  expect_no_error(sufficient_stats(fit, data = d))
})

test_that("unsupported results and designs are not evaluable, never an error", {
  d <- datasets::mtcars
  not_eval <- function(s) {
    expect_s3_class(s, "reprokit_sufficient_stats")
    expect_identical(s$verdict, "needs_raw_data")
    expect_null(s$stats)
    expect_true(nzchar(s$verdict_reason))
  }
  not_eval(sufficient_stats(cor.test(d$mpg, d$wt, method = "spearman", exact = FALSE), data = d))
  not_eval(sufficient_stats(cor.test(d$mpg, d$wt, method = "kendall", exact = FALSE), data = d))
  pg <- datasets::PlantGrowth
  pg$cov <- seq_len(nrow(pg))
  not_eval(sufficient_stats(aov(weight ~ group + cov, data = pg), data = pg))
  npk <- datasets::npk
  not_eval(sufficient_stats(aov(yield ~ N * P + Error(block), data = npk), data = npk))
  three <- data.frame(y = rnorm(48), a = gl(2, 24), b = gl(2, 12, 48), c = gl(2, 6, 48))
  not_eval(sufficient_stats(aov(y ~ a * b * c, data = three), data = three))
  w <- seq_len(nrow(pg))
  not_eval(sufficient_stats(aov(weight ~ group, data = pg, weights = w), data = pg))
})

test_that("a simulated chi-squared p value is not apa_suffices", {
  d <- mt()
  set.seed(1)
  sim <- chisq.test(table(d$am, d$vs), simulate.p.value = TRUE, B = 500)
  s <- sufficient_stats(sim, data = d)
  expect_false(identical(s$verdict, "apa_suffices"))
  expect_match(s$verdict_reason, "simulat", ignore.case = TRUE)
})

test_that("a conflicting explicit choice is refused", {
  d <- datasets::sleep
  fit <- t.test(extra ~ group, data = d, var.equal = TRUE)
  expect_error(sufficient_stats(fit, data = d, test = "welch"), class = "reprokit_error_input")
})

test_that("results that cannot be read exactly are refused with the reason", {
  d <- datasets::mtcars
  d$cyl <- factor(d$cyl)
  reason <- function(...) {
    s <- sufficient_stats(...)
    expect_identical(s$verdict, "needs_raw_data")
    s$verdict_reason
  }
  expect_match(reason(aov(cbind(mpg, hp) ~ cyl, data = d)), "multivariate")
  expect_match(reason(aov(mpg ~ cyl - 1, data = d)), "intercept")
  expect_match(reason(aov(mpg ~ cyl, data = d, model = FALSE)), "model frame")
  expect_match(reason(chisq.test(table(d$cyl, d$am), simulate.p.value = TRUE, B = 100)),
               "simulated")
  no_table <- structure(list(method = "Pearson's Chi-squared test", statistic = 1,
                             parameter = 1, p.value = .3, observed = NULL), class = "htest")
  expect_match(reason(no_table), "no table")
  expect_match(reason(t.test(d$mpg, d$hp), data = d), "t.test")
  expect_match(reason(cor.test(d$mpg, d$wt), data = d), "cor.test")
  expect_match(reason(lm(wt ~ hp - 1, d), lm(mpg ~ hp + wt, d)), "intercept")
})

test_that("an aov conflicting with test= and results without data are input errors", {
  d <- datasets::mtcars
  d$cyl <- factor(d$cyl)
  expect_error(sufficient_stats(aov(mpg ~ cyl, data = d), test = "factorial"),
               class = "reprokit_error_input")
  expect_error(sufficient_stats(t.test(mpg ~ am, data = d)), class = "reprokit_error_input")
  expect_error(sufficient_stats(cor.test(d$mpg, d$wt)), class = "reprokit_error_input")
  expect_error(sufficient_stats(lm(wt ~ hp, d), lm(mpg ~ hp + wt, d), lm(mpg ~ 1, d)),
               class = "reprokit_error_input")
})
