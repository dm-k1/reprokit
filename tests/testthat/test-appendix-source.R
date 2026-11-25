# Printed numbers come from the model, and a fit whose own numbers differ from the ones
# recomputed from its summaries is refused.

or <- function(a, b) if (is.null(a)) b else a

reason <- "Consent did not cover public sharing."

app <- function(x, ..., extra = "all", choices = list()) {
  appendix(x, ..., sharing_reason = reason, extra = extra, choices = choices)
}

routes <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  list(
    lm_simple = list(x = lm(mpg ~ wt, d), dots = list()),
    lm_multiple = list(x = lm(mpg ~ wt + hp, d), dots = list()),
    lm_factor = list(x = lm(mpg ~ wt + cyl, d), dots = list()),
    welch = list(x = t.test(extra ~ group, data = sleep), dots = list(data = sleep),
                 choices = list(convention = "pooled")),
    student = list(x = t.test(extra ~ group, data = sleep, var.equal = TRUE),
                   dots = list(data = sleep), choices = list(convention = "pooled")),
    pearson = list(x = cor.test(~ mpg + wt, data = mtcars), dots = list(data = mtcars)),
    oneway = list(x = aov(weight ~ group, PlantGrowth), dots = list()),
    chisq = list(x = chisq.test(table(mtcars$am, mtcars$vs)), dots = list(), extra = character()),
    mediation = list(x = lm(hp ~ wt, mtcars), dots = list(lm(mpg ~ wt + hp, mtcars)))
  )
}

test_that("model-sourced values equal the recomputed ones on every route", {
  for (name in names(routes())) {
    spec <- routes()[[name]]
    from_model <- do.call(app, c(list(spec$x), spec$dots,
                                 list(extra = or(spec$extra, "all"),
                                      choices = or(spec$choices, list()))))
    s <- do.call(sufficient_stats, c(list(spec$x), spec$dots))
    from_stats <- app(s, extra = or(spec$extra, "all"), choices = or(spec$choices, list()))
    expect_identical(unclass(from_model), unclass(from_stats), info = name)
  }
})

test_that("the reference values are the model's own and match reproduce() within 1e-8", {
  for (name in setdiff(names(routes()), "chisq")) {
    spec <- routes()[[name]]
    h <- get_handler(do.call(sufficient_stats, c(list(spec$x), spec$dots))$handler)
    s <- do.call(sufficient_stats, c(list(spec$x), spec$dots))
    ref <- appendix_model_reference(spec$x, spec$dots, h$name)
    expect_false(is.null(ref), info = name)
    recomputed <- appendix_recomputed(h, s)
    keys <- intersect(names(ref), names(recomputed))
    expect_gt(length(keys), 2L)
    expect_true(all(scaled_differences(ref[keys], recomputed[keys]) < 1e-8), info = name)
  }
})

test_that("the printed number is the model's, not the recomputed one", {
  fit <- lm(mpg ~ wt, mtcars)
  s <- sufficient_stats(fit)
  ref <- appendix_model_reference(fit, list(), "lm_simple")
  shifted <- ref
  shifted[["r.squared"]] <- ref[["r.squared"]] + 1e-10
  r <- with_model_source(shifted, "lm_simple", source_reproduce(s))
  expect_equal(r[["r.squared"]], shifted[["r.squared"]], tolerance = 0)
  expect_null(.appendix_source$reference)
})

test_that("a tampered stored coefficient is refused", {
  fit <- lm(mpg ~ wt + hp, mtcars)
  fit$coefficients[["wt"]] <- fit$coefficients[["wt"]] * 1.01
  expect_error(app(fit), class = "reprokit_impossible")
  med <- lm(hp ~ wt, mtcars)
  med$coefficients[["wt"]] <- med$coefficients[["wt"]] + 0.5
  expect_error(app(med, lm(mpg ~ wt + hp, mtcars)), class = "reprokit_impossible")
  outc <- lm(mpg ~ wt + hp, mtcars)
  outc$coefficients[["hp"]] <- outc$coefficients[["hp"]] * 2
  expect_error(app(lm(hp ~ wt, mtcars), outc), class = "reprokit_impossible")
})

test_that("a tampered test result or table is refused", {
  ct <- cor.test(~ mpg + wt, data = mtcars)
  ct$estimate[[1L]] <- ct$estimate[[1L]] + 0.01
  expect_error(app(ct, data = mtcars), class = "reprokit_impossible")
  tt <- t.test(extra ~ group, data = sleep)
  tt$statistic[[1L]] <- tt$statistic[[1L]] * 1.01
  expect_error(app(tt, data = sleep, choices = list(convention = "pooled")),
               class = "reprokit_impossible")
  fit <- aov(weight ~ group, PlantGrowth)
  fit$effects[2L] <- fit$effects[2L] * 1.02
  expect_error(app(fit), class = "reprokit_impossible")
  chi <- chisq.test(table(mtcars$am, mtcars$vs))
  chi$statistic[[1L]] <- chi$statistic[[1L]] + 0.1
  expect_error(app(chi, extra = character()), class = "reprokit_impossible")
})

test_that("the guard names the disagreeing value and says what to do", {
  fit <- lm(mpg ~ wt, mtcars)
  fit$coefficients[["wt"]] <- 0
  err <- tryCatch(app(fit), error = function(e) e)
  expect_s3_class(err, "reprokit_impossible")
  expect_match(conditionMessage(err), ":wt")
  expect_match(conditionMessage(err), "Refit the model")
})
