sharing <- "Consent did not cover public sharing."

sleep_wide <- function() {
  data.frame(drug1 = sleep$extra[sleep$group == 1], drug2 = sleep$extra[sleep$group == 2])
}

# Snapshots must not depend on the locale: a C locale prints U+2212 as "<U+2212>".
ascii_safe <- function(lines) {
  lines <- gsub("<U\\+2212>", "-", lines)
  lines <- gsub("<U\\+00A7>", "S", lines)
  lines <- gsub(intToUtf8(0x2212), "-", lines, fixed = TRUE)
  gsub(intToUtf8(0x00A7), "S", lines, fixed = TRUE)
}

paired_stats <- function() {
  sufficient_stats(Pair(drug1, drug2) ~ 1, data = sleep_wide(), test = "paired",
                   sharing_reason = sharing)
}

test_that("the paired appendix matches t.test()", {
  expect_snapshot(print(appendix(paired_stats(), choices = list(convention = "d_z"))),
                  transform = ascii_safe)
})

test_that("the one-sample appendix is stable", {
  s <- sufficient_stats(extra ~ 1, data = sleep, test = "one_sample", mu = 0.5,
                        sharing_reason = sharing)
  expect_snapshot(print(appendix(s, choices = list(convention = "cohen"))),
                  transform = ascii_safe)
})

test_that("the independent-samples appendix is stable", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch", sharing_reason = sharing)
  expect_snapshot(print(appendix(s, choices = list(convention = "pooled"))),
                  transform = ascii_safe)
})

test_that("numeric group levels print as 'group 1' and 'group 2'", {
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch", sharing_reason = sharing)
  text <- paste(appendix(s, choices = list(convention = "pooled")), collapse = "\n")
  expect_match(text, paste0("group 1 ", intToUtf8(0x2212), " group 2"), fixed = TRUE)
})

test_that("non-numeric condition labels are left unchanged", {
  text <- paste(appendix(paired_stats(), choices = list(convention = "d_z")), collapse = "\n")
  expect_match(text, "drug1", fixed = TRUE)
  expect_false(grepl("group drug", text, fixed = TRUE))
})

test_that("the Pearson appendix is stable", {
  s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson", sharing_reason = sharing)
  expect_snapshot(print(appendix(s)), transform = ascii_safe)
})

test_that("paired numbers equal t.test(paired = TRUE), rounded as printed", {
  w <- sleep_wide()
  r <- stats::t.test(w$drug1, w$drug2, paired = TRUE)
  fmt <- function(x, d = 2L) formatC(x, format = "f", digits = d)
  text <- paste(appendix(paired_stats(), choices = list(convention = "d_z")), collapse = "\n")
  text <- gsub(intToUtf8(0x2212), "-", text, fixed = TRUE)
  expect_match(text, paste0("t(", r$parameter, ") | ", fmt(r$statistic)), fixed = TRUE)
  expect_match(text, paste0("| p | ", sub("^0", "", formatC(r$p.value, format = "f", digits = 3L))),
               fixed = TRUE)
  expect_match(text, paste0("[", fmt(r$conf.int[1]), ", ", fmt(r$conf.int[2]), "]"), fixed = TRUE)
})

test_that("a paired appendix without a convention says it is not computed", {
  text <- paste(appendix(paired_stats()), collapse = "\n")
  expect_match(text, "not computed: declare the effect-size convention (block C)", fixed = TRUE)
  expect_match(text, "not declared", fixed = TRUE)
  expect_false(grepl("NA", text, fixed = TRUE))
})

test_that("an unknown convention errors and lists the valid codes", {
  expect_error(appendix(paired_stats(), choices = list(convention = "nope")),
               "d_z.*d_rm.*d_av.*g_av")
})

test_that("a handler without a declared APA set has no appendix", {
  # A polynomial falls back to the general lm, which has no APA set.
  s <- sufficient_stats(stats::lm(mpg ~ poly(wt, 2), data = mtcars), sharing_reason = sharing)
  expect_equal(s$handler, "lm")
  expect_error(appendix(s), "No appendix for")
})

test_that("a convention given to a handler without conventions errors", {
  s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson", sharing_reason = sharing)
  expect_error(appendix(s, choices = list(convention = "d_z")),
               "`cor_pearson` has no effect-size convention to declare; drop `choices$convention`.",
               fixed = TRUE)
})

# Known answers: the printed strings must equal base R's, rounded as appendix() prints.
printed <- function(x) {
  gsub(intToUtf8(0x2212), "-", paste(x, collapse = "\n"), fixed = TRUE)
}
fmt2 <- function(x, d = 2L) formatC(x, format = "f", digits = d)
fmt_p <- function(p) {
  if (p < 0.001) "< .001" else sub("^0", "", formatC(p, format = "f", digits = 3L))
}
fmt_r <- function(x) sub("^(-?)0\\.", "\\1.", fmt2(x))
fmt_df <- function(df) if (abs(df - round(df)) < 1e-8) fmt2(df, 0L) else fmt2(df)

expect_t_printed <- function(text, r) {
  testthat::expect_match(text, paste0("t(", fmt_df(r$parameter), ") | ", fmt2(r$statistic), " |"),
                         fixed = TRUE)
  testthat::expect_match(text, paste0("| p | ", fmt_p(r$p.value), " |"), fixed = TRUE)
  testthat::expect_match(text, paste0("[", fmt2(r$conf.int[1]), ", ", fmt2(r$conf.int[2]), "] |"),
                         fixed = TRUE)
}

test_that("Welch numbers equal t.test(extra ~ group), with fractional df", {
  r <- stats::t.test(extra ~ group, data = sleep)
  s <- sufficient_stats(extra ~ group, data = sleep, test = "welch", sharing_reason = sharing)
  text <- printed(appendix(s, choices = list(convention = "pooled")))
  expect_match(text, "t(17.78) |", fixed = TRUE)
  expect_t_printed(text, r)
})

test_that("Student numbers equal t.test(var.equal = TRUE), with whole df", {
  r <- stats::t.test(extra ~ group, data = sleep, var.equal = TRUE)
  s <- sufficient_stats(extra ~ group, data = sleep, test = "student", sharing_reason = sharing)
  text <- printed(appendix(s, choices = list(convention = "pooled")))
  expect_match(text, "t(18) |", fixed = TRUE)
  expect_t_printed(text, r)
})

test_that("one-sample numbers equal t.test(x, mu = 0.5)", {
  r <- stats::t.test(sleep$extra, mu = 0.5)
  s <- sufficient_stats(extra ~ 1, data = sleep, test = "one_sample", mu = 0.5,
                        sharing_reason = sharing)
  expect_t_printed(printed(appendix(s, choices = list(convention = "cohen"))), r)
})

test_that("Pearson numbers equal cor.test(mpg, wt)", {
  r <- stats::cor.test(mtcars$mpg, mtcars$wt)
  s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson", sharing_reason = sharing)
  text <- printed(appendix(s))
  expect_match(text, paste0("r(", fmt_df(r$parameter), ") | ", fmt_r(r$estimate), " |"),
               fixed = TRUE)
  expect_match(text, paste0("| p | ", fmt_p(r$p.value), " |"), fixed = TRUE)
  expect_match(text, paste0("[", fmt_r(r$conf.int[1]), ", ", fmt_r(r$conf.int[2]), "] |"),
               fixed = TRUE)
})

test_that("the one-way appendix is stable", {
  s <- sufficient_stats(weight ~ group, data = PlantGrowth, test = "oneway",
                        sharing_reason = sharing)
  expect_snapshot(print(appendix(s, choices = list(convention = "eta2"))),
                  transform = ascii_safe)
})

test_that("the chi-squared independence appendix is stable", {
  d <- as.data.frame(margin.table(UCBAdmissions, c(1, 2)))
  d <- d[rep(seq_len(nrow(d)), d$Freq), c("Admit", "Gender")]
  s <- sufficient_stats(~ Admit + Gender, data = d, test = "chisq", sharing_reason = sharing,
                        threshold = 1)
  expect_snapshot(print(appendix(s)), transform = ascii_safe)
})

test_that("the chi-squared goodness-of-fit appendix is stable", {
  s <- sufficient_stats(~ cyl, data = data.frame(cyl = mtcars$cyl), test = "gof",
                        sharing_reason = sharing, threshold = 1)
  expect_snapshot(print(appendix(s)), transform = ascii_safe)
})

test_that("the factorial appendix is stable: balanced, then unbalanced with and without the SS type", {
  tg <- datasets::ToothGrowth
  tg$dose <- factor(tg$dose)
  s <- sufficient_stats(len ~ supp * dose, data = tg, test = "factorial", sharing_reason = sharing)
  expect_snapshot(print(appendix(s, choices = list(convention = "partial_eta2"))),
                  transform = ascii_safe)
  u <- factorial_unbalanced()
  su <- sufficient_stats(y ~ a * b, data = u, test = "factorial", sharing_reason = sharing)
  expect_snapshot(print(appendix(su)), transform = ascii_safe)
  expect_snapshot(print(appendix(su, choices = list(ss_type = "II", convention = "partial_omega2"))),
                  transform = ascii_safe)
})

test_that("the factorial appendix numbers equal the handler's at the declared SS type", {
  u <- factorial_unbalanced()
  su <- sufficient_stats(y ~ a * b, data = u, test = "factorial", sharing_reason = sharing)
  out <- paste(capture.output(print(appendix(su, choices = list(ss_type = "I",
                                                               convention = "partial_eta2")))),
               collapse = "\n")
  r <- anova_factorial_reported(y ~ a * b, u, "factorial", ss_type = "I")
  expect_match(out, sprintf("F(1, %d) = %.2f", r[["df2"]], r[["f_A"]]), fixed = TRUE)
  expect_error(appendix(su, choices = list(ss_type = "IV")), "ss_type")
  expect_error(appendix(sufficient_stats(weight ~ group, data = PlantGrowth, test = "oneway",
                                        sharing_reason = sharing),
                        choices = list(ss_type = "III")), "SS type")
})

test_that("the regression-family appendices are stable", {
  mt <- datasets::mtcars
  mt$am <- factor(mt$am)
  sr <- "Consent did not cover public sharing."
  expect_snapshot(print(appendix(sufficient_stats(lm(mpg ~ wt, mt), sharing_reason = sr))),
                  transform = ascii_safe)
  expect_snapshot(print(appendix(sufficient_stats(lm(mpg ~ wt + hp + am, mt), sharing_reason = sr),
                                 choices = list(coding = "treatment"))),
                  transform = ascii_safe)
  expect_snapshot(print(appendix(sufficient_stats(lm(mpg ~ wt * hp, mt), sharing_reason = sr),
                                 choices = list(centering = "none", coding = "none"))),
                  transform = ascii_safe)
  expect_snapshot(print(appendix(sufficient_stats(mpg ~ wt, data = mt, test = "mediation",
                                                  mediator = "hp", sharing_reason = sr))),
                  transform = ascii_safe)
})
