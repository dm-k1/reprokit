status_of <- function(res, quantity) res$numbers$status[res$numbers$quantity == quantity]

test_that("every test applies the quantities its relations and APA set name", {
  expect_true(all(c("n", "r", "r2", "adj_r2") %in%
                    applicable_quantities(get_handler("cor_pearson"))))
  expect_true(all(c("eta2", "r2", "omega2") %in%
                    applicable_quantities(get_handler("anova_oneway"))))
  expect_true(all(c("eta2", "partial_eta2", "partial_omega2") %in%
                    applicable_quantities(get_handler("anova_factorial"))))
  expect_true(all(c("t", "eta2", "r2", "omega2") %in%
                    applicable_quantities(get_handler("t_independent"))))
  expect_false("d" %in% applicable_quantities(get_handler("cor_pearson")))
  expect_false("r2" %in% applicable_quantities(get_handler("chisq_independence")))
})

test_that("a known name that does not apply is an input error listing the applicable names", {
  err <- tryCatch(check_reported("pearson", n = "32", d = "1"), error = function(e) e)
  expect_s3_class(err, "reprokit_error_input")
  expect_match(conditionMessage(err), "`d` does not apply")
  expect_match(conditionMessage(err), "`adj_r2`", fixed = TRUE)
  expect_error(check_reported("chisq", r2 = "0.1"), class = "reprokit_error_input")
  expect_error(check_reported("pearson", tstat = "1"), class = "reprokit_error_input")
})

test_that("Pearson r2 and adjusted R2 are cross-checked, and a planted error is flagged", {
  fine <- check_reported("pearson", n = "32", r = "0.40", r2 = "0.16")
  expect_true(fine$consistent)
  expect_equal(status_of(fine, "r2"), "verified")
  adj <- check_reported("pearson", n = "32", r2 = "0.16", adj_r2 = "0.13")
  expect_true(adj$consistent)
  expect_equal(status_of(adj, "adj_r2"), "verified")
  bad <- check_reported("pearson", n = "32", r2 = "0.16", adj_r2 = "0.50")
  expect_false(bad$consistent)
  expect_false(check_reported("pearson", n = "32", r = "0.40", r2 = "0.30")$consistent)
})

test_that("one-way r2 equals eta2 and is checked against F", {
  fit <- summary(aov(weight ~ group, PlantGrowth))[[1L]]
  ss <- fit[["Sum Sq"]]
  eta <- sprintf("%.2f", ss[1L] / sum(ss))
  ok <- check_reported("oneway", f = sprintf("%.2f", fit[["F value"]][1L]),
                       df1 = "2", df2 = "27", r2 = eta)
  expect_true(ok$consistent)
  expect_true(status_of(ok, "r2") %in% c("verified", "coherent"))
  bad <- check_reported("oneway", f = sprintf("%.2f", fit[["F value"]][1L]),
                        df1 = "2", df2 = "27", r2 = "0.60")
  expect_false(bad$consistent)
})

test_that("t-test r2 and eta2 follow from t and df but never falsify omega2", {
  ok <- check_reported("welch", t = "2.85", df = "18", r2 = "0.31")
  expect_true(status_of(ok, "r2") %in% c("verified", "coherent"))
  expect_true(ok$consistent)
  off <- check_reported("welch", t = "2.85", df = "18", omega2 = "0.99")
  expect_true(off$consistent)
})

test_that("Student df is checked against n, and only the non-redundant relation is hinted", {
  expect_false(check_reported("student", df = "17", n = c("10", "10"))$consistent)
  expect_true(check_reported("student", df = "18", n = c("10", "10"))$consistent)
  hint <- check_reported("student", df = "18", t = "2.85")
  checked <- hint$numbers$checked_against[hint$numbers$quantity == "df"]
  expect_equal(sum(grepl("^df_from_n", checked)),
               1L)
  expect_match(hint$numbers$checked_against[hint$numbers$quantity == "df"],
               "df_from_n_student (needs n)", fixed = TRUE)
})

test_that("an honest Welch paper is not held to Student df", {
  expect_true(check_reported("welch", df = "10.98", n = c("10", "10"),
                             sd = c("1.0", "3.0"))$consistent)
})

test_that("the coverage table lists the extras from the registry", {
  tab <- coverage_table()
  expect_true("extras" %in% names(tab))
  expect_match(tab$extras[tab$test == "Pearson correlation test"], "adjusted R")
})

test_that("appendix extras: names, 'all', formatting and the invalid-name error", {
  s <- sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson")
  base <- appendix(s, sharing_reason = "r")
  expect_identical(appendix(s, sharing_reason = "r", extra = character()), base)
  txt <- paste(appendix(s, sharing_reason = "r", extra = "all"), collapse = "\n")
  expect_match(txt, "Additional numbers", fixed = TRUE)
  expect_match(txt, "R² | .75", fixed = TRUE)
  expect_false(grepl("0.75", sub(".*Additional numbers", "", txt), fixed = TRUE))
  err <- tryCatch(appendix(s, sharing_reason = "r", extra = "nope"), error = function(e) e)
  expect_s3_class(err, "reprokit_error_input")
  expect_match(conditionMessage(err), "Valid extras: `r2`, `adj_r2`", fixed = TRUE)
  expect_error(appendix(sufficient_stats(chisq.test(table(mtcars$am, mtcars$vs))),
                        sharing_reason = "r", extra = "r2"), class = "reprokit_error_input")
})

test_that("every handler with extras prints them under 'all'", {
  specs <- list(
    sufficient_stats(extra ~ group, data = sleep, test = "welch"),
    sufficient_stats(weight ~ group, data = PlantGrowth, test = "oneway"),
    sufficient_stats(lm(mpg ~ wt + hp, mtcars)),
    sufficient_stats(lm(hp ~ wt, mtcars), lm(mpg ~ wt + hp, mtcars))
  )
  for (s in specs) {
    choices <- if (s$handler == "t_independent") list(convention = "eta2") else list()
    txt <- paste(suppressWarnings(appendix(s, sharing_reason = "r", extra = "all",
                                           choices = choices)),
                 collapse = "\n")
    expect_match(txt, "Additional numbers", fixed = TRUE, info = s$handler)
    expect_false(grepl("not determined", txt, fixed = TRUE), info = s$handler)
  }
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  f <- sufficient_stats(len ~ supp * dose, data = tg, test = "factorial")
  txt <- paste(appendix(f, sharing_reason = "r", extra = "all", choices = list(ss_type = "III")),
               collapse = "\n")
  expect_match(txt, "partial eta2", fixed = TRUE)
})
