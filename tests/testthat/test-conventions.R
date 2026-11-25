sharing <- "Consent did not cover public sharing."

sleep_wide <- function() {
  data.frame(drug1 = sleep$extra[sleep$group == 1], drug2 = sleep$extra[sleep$group == 2])
}

test_that("conventions() lists every handler's codes in the documented shape", {
  x <- conventions()
  expect_s3_class(x, "data.frame")
  expect_named(x, c("handler", "choice", "code", "default", "label"))
  expect_type(x$default, "logical")
  expect_false(anyNA(x$label))
  expect_true(all(x$choice %in% c("convention", "ss_type")))
  expect_setequal(unique(x$handler),
                  c("anova_factorial", "anova_oneway", "chisq_gof", "chisq_independence",
                    "lm_moderation", "lm_multiple", "lm_simple", "t_independent",
                    "t_one_sample", "t_paired"))
  expect_identical(conventions("t_paired"), x[x$handler == "t_paired", ], ignore_attr = TRUE)
})

test_that("an unknown handler is impossible", {
  expect_error(conventions("nope"), class = "reprokit_impossible")
  expect_error(conventions("cor_pearson") -> r, NA)
  expect_identical(nrow(r), 0L)
})

test_that("each handler x choice has at most one default, and exactly one where one applies", {
  x <- conventions()
  n_default <- tapply(x$default, paste(x$handler, x$choice), sum)
  expect_true(all(n_default <= 1L))
  n_codes <- tapply(x$code, paste(x$handler, x$choice), length)
  expect_true(all(n_default[n_codes == 1L] == 1L))
  # Share-side default (ADR 0025): type III.
  ss <- x[x$choice == "ss_type", ]
  expect_identical(ss$code[ss$default], "III")
})

# One design per handler, to run its codes through the real functions.
big_table <- function() {
  data.frame(a = rep(c("x", "y"), each = 100),
             b = rep(c("u", "v", "u", "v"), c(60, 40, 30, 70)))
}

convention_stats <- function(name) {
  share <- function(...) sufficient_stats(...)
  switch(name,
    t_one_sample = share(extra ~ 1, data = sleep, test = "one_sample"),
    t_independent = share(extra ~ group, data = sleep, test = "welch"),
    t_paired = share(Pair(drug1, drug2) ~ 1, data = sleep_wide(), test = "paired"),
    anova_oneway = share(weight ~ group, data = PlantGrowth, test = "oneway"),
    anova_factorial = share(len ~ supp * dose, data = unbalanced_tooth(), test = "factorial"),
    chisq_independence = share(~ a + b, data = big_table(), test = "chisq"),
    chisq_gof = share(~ a, data = big_table(), test = "gof"),
    lm_simple = share(lm(mpg ~ wt, mtcars)),
    lm_multiple = share(lm(mpg ~ wt + hp, mtcars)),
    lm_moderation = share(lm(mpg ~ wt * hp, mtcars)))
}

unbalanced_tooth <- function() {
  tg <- transform(ToothGrowth, dose = factor(dose))
  tg[-(1:2), ]
}

test_that("every code is accepted by appendix() and gives its own row", {
  x <- conventions()
  for (name in unique(x$handler)) {
    s <- convention_stats(name)
    expect_identical(s$handler, name)
    for (choice in unique(x$choice[x$handler == name])) {
      codes <- x$code[x$handler == name & x$choice == choice]
      pattern <- if (choice == "convention") "^\\| Effect-size convention \\|" else
        "^\\| SS type \\|"
      rows <- vapply(codes, function(code) {
        md <- appendix(s, sharing_reason = sharing,
                       choices = stats::setNames(list(code), choice))
        row <- grep(pattern, md, value = TRUE)
        expect_length(row, 1L)
        row
      }, "")
      if (length(codes) > 1L) {
        expect_false(anyDuplicated(rows) > 0, info = paste(name, choice))
      }
    }
  }
})

test_that("the defaults are what happens when the argument is omitted", {
  x <- conventions()
  # A single convention code is used when none is declared; several codes have no default.
  for (name in unique(x$handler[x$choice == "convention"])) {
    codes <- x$code[x$handler == name & x$choice == "convention"]
    if (length(codes) == 1L) {
      s <- convention_stats(name)
      expect_identical(x$code[x$handler == name & x$choice == "convention" & x$default], codes)
      expect_identical(appendix(s, sharing_reason = sharing),
                       appendix(s, sharing_reason = sharing,
                                choices = list(convention = codes)))
    } else {
      expect_false(any(x$default[x$handler == name & x$choice == "convention"]))
    }
  }
  # Omitting the SS type gives type III on an unbalanced design, labelled as the default.
  u <- sufficient_stats(len ~ supp * dose, data = unbalanced_tooth(), test = "factorial")
  row <- grep("^\\| SS type \\|", appendix(u, sharing_reason = sharing), value = TRUE)
  expect_match(row, "type III \\(reprokit default, not stated by the author\\)")
  row <- grep("^\\| SS type \\|",
              appendix(u, sharing_reason = sharing, choices = list(ss_type = "III")),
              value = TRUE)
  expect_match(row, "type III")
  expect_no_match(row, "reprokit default")
})

test_that("appendix() renders every undeclared choice, for every handler", {
  extra <- list(
    cor_pearson = sufficient_stats(~ mpg + wt, data = mtcars, test = "pearson"),
    mediation_simple = sufficient_stats(mpg ~ wt, data = mtcars,
                                        test = "mediation", mediator = "hp"))
  for (name in handlers()) {
    if (is.null(get_handler(name)$design)) next  # `lm`: no appendix by design
    s <- if (name %in% names(extra)) extra[[name]] else convention_stats(name)
    expect_identical(s$handler, name)
    expect_error(md <- appendix(s, sharing_reason = sharing), NA, info = name)
    expect_s3_class(md, "reprokit_appendix")
  }
})
