# Mediation from two fitted models (ADR 0028) and the coherence of the reported indirect effect.

med_data <- function() {
  d <- datasets::mtcars
  d$cyl <- as.numeric(d$cyl)
  d
}

strip_route <- function(s) {
  s$meta$route <- NULL
  s$choices_read <- NULL
  s
}

test_that("the two-model route equals the formula route, in either argument order", {
  d <- med_data()
  m <- lm(hp ~ wt, data = d)
  y <- lm(mpg ~ wt + hp, data = d)
  formula_route <- sufficient_stats(mpg ~ wt, data = d, test = "mediation", mediator = "hp")
  expect_identical(formula_route$verdict, "summaries_suffice")
  fwd <- sufficient_stats(m, y)
  expect_identical(fwd$meta$route, "lm")
  expect_identical(strip_route(fwd), strip_route(formula_route))
  expect_identical(strip_route(sufficient_stats(y, m)), strip_route(formula_route))
  expect_identical(appendix(m, y, sharing_reason = "consent"),
                   appendix(formula_route, sharing_reason = "consent"))
})

test_that("two models that are not the same cases are refused", {
  d <- med_data()
  m <- lm(hp ~ wt, data = d)
  y_sub <- lm(mpg ~ wt + hp, data = d[-1L, ])
  expect_error(sufficient_stats(m, y_sub), class = "reprokit_impossible")
  d2 <- d
  d2$wt <- rev(d2$wt)
  expect_error(sufficient_stats(m, lm(mpg ~ wt + hp, data = d2)), class = "reprokit_impossible")
  other <- lm(qsec ~ wt + drat, data = d)
  expect_error(sufficient_stats(m, other), class = "reprokit_impossible")
})

test_that("a covariate, an extra mediator or weights are not evaluable", {
  d <- med_data()
  m <- lm(hp ~ wt, data = d)
  with_cov <- sufficient_stats(m, lm(mpg ~ wt + hp + disp, data = d))
  expect_false(identical(with_cov$verdict, "summaries_suffice"))
  m_cov <- lm(hp ~ wt + disp, data = d)
  expect_false(identical(sufficient_stats(m_cov, lm(mpg ~ wt + hp, data = d))$verdict,
                         "summaries_suffice"))
  w <- seq_len(nrow(d))
  expect_false(identical(sufficient_stats(lm(hp ~ wt, data = d, weights = w),
                                          lm(mpg ~ wt + hp, data = d, weights = w))$verdict,
                         "summaries_suffice"))
})

test_that("the appendix gives a*b as a reproduced number, and its CI stays in block D and lost", {
  d <- med_data()
  txt <- paste(appendix(lm(hp ~ wt, d), lm(mpg ~ wt + hp, d), sharing_reason = "consent"),
               collapse = "\n")
  expect_match(txt, "a.b", ignore.case = TRUE)
  expect_match(txt, "bootstrap", ignore.case = TRUE)
  s <- sufficient_stats(lm(hp ~ wt, d), lm(mpg ~ wt + hp, d))
  expect_true(any(grepl("bootstrap|indirect", s$lost, ignore.case = TRUE)))
})

# Reported paths from the fits, at two decimals; a*b is the product of the unrounded paths.
paths <- function() {
  d <- med_data()
  a <- unname(coef(lm(hp ~ wt, d))["wt"])
  b <- unname(coef(lm(mpg ~ wt + hp, d))["hp"])
  list(a = a, b = b)
}

status_ind <- function(ck) ck$numbers$status[ck$numbers$quantity == "indirect"]

test_that("check_reported('mediation') checks the indirect against the product of a and b", {
  p <- paths()
  ok <- check_reported(test = "mediation", b = c(a = formatC(p$a, 2, format = "f"),
                                                 b = formatC(p$b, 4, format = "f")),
                       indirect = formatC(p$a * p$b, 2, format = "f"))
  expect_identical(status_ind(ok), "coherent")
  expect_true(ok$consistent)
  bad <- check_reported(test = "mediation", b = c(a = formatC(p$a, 2, format = "f"),
                                                  b = formatC(p$b, 4, format = "f")),
                        indirect = formatC(p$a * p$b + 5, 2, format = "f"))
  expect_identical(status_ind(bad), "inconsistent")
  expect_match(paste(bad$notes, collapse = " "), "not coherent")
  expect_false(bad$consistent)
  expect_match(paste(bad$notes, collapse = " "), "bootstrap")
  ci <- check_reported(test = "mediation", b = c(a = "1.0", b = "0.5"), indirect = "0.5",
                       indirect_ci_lower = "0.1", indirect_ci_upper = "0.9")
  expect_true(all(ci$numbers$status[grepl("indirect_ci", ci$numbers$quantity)] == "not checkable"))
})

test_that("a standardized indirect without a declared type is not evaluable", {
  ck <- check_reported(test = "mediation", beta = c(a = "0.5", b = "0.4"), indirect = "0.20")
  expect_false(isTRUE(status_ind(ck) %in% c("coherent", "inconsistent")))
  expect_true(any(grepl("not evaluable|standardized", ck$notes)))
  decl <- check_reported(test = "mediation", beta = c(a = "0.5", b = "0.4"), indirect = "0.20",
                         indirect_type = "standardized")
  expect_s3_class(decl, "reprokit_check")
})

test_that("SEM mediation is not evaluable, with SEM as the reason", {
  ck <- check_reported(test = "mediation", b = c(a = "1.0", b = "0.5"), indirect = "99",
                       estimator = "sem")
  expect_match(paste(ck$notes, collapse = " "), "SEM")
  expect_false(any(c("coherent", "inconsistent") %in% ck$numbers$status))
})
