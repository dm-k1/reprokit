# Sharing from fitted results (ADR 0026, ADR 0027 point 5).
# In one line: an aov, chisq.test, t.test or cor.test result is translated into the inputs of
# the formula route (a formula, data, `test` and the choices the result records), and the
# existing summarise code runs on them. There is no second summarise path, so a result and
# the formula for the same analysis give the same summaries by construction; `meta$route`
# records which one was used. Where the result holds an answer (its statistic, df and p),
# the summaries must reproduce it (the match check) or they are refused.

# Returns the input as the handlers see it:
#   x, dots   what find_handler() and the handler's functions get
#   route     "formula", "lm", "model", "aov", "chisq.test", "t.test" or "cor.test"
#   read      choices read from the result, a named list with attribute `fn`
#   expected  the result's own numbers, for the match check (or NULL)
#   problem   a reason the result cannot be shared (a verdict, not an error), or NULL
# Invalid input (no data, a conflicting explicit choice) is an abort_input() error.
prepare_input <- function(x, dots) {
  route <- if (inherits(x, "formula")) "formula" else if (inherits(x, "lm")) "lm" else "model"
  out <- list(x = x, dots = dots, route = route, read = list(), expected = NULL, problem = NULL)
  if (inherits(x, "formula")) {
    return(check_formula_input(out))
  }
  second <- unnamed_models(dots)
  if (inherits(x, "lm") && length(second) > 0L) {
    return(prepare_mediation(out, second))
  }
  if (inherits(x, "aovlist")) {
    return(refuse_result(out, paste0(
      "An aov with Error() strata (repeated measures or nested error terms) is not a design ",
      "reprokit can evaluate: its F tests use several error terms."
    )))
  }
  if (inherits(x, "maov")) {
    return(refuse_result(out, "A multivariate aov (several outcomes) is not evaluable."))
  }
  if (inherits(x, "aov")) {
    return(prepare_aov(out))
  }
  if (inherits(x, "htest")) {
    return(prepare_htest(out))
  }
  out
}

# Unnamed arguments in `...` that are fitted models (the second model of a mediation).
unnamed_models <- function(dots) {
  nm <- names(dots)
  if (is.null(nm)) nm <- rep("", length(dots))
  dots[!nzchar(nm) & vapply(dots, function(d) inherits(d, "lm"), logical(1))]
}

refuse_result <- function(out, reason) {
  out$problem <- reason
  out
}

# A formula needs `data` and a known `test`; both are invalid input when missing.
check_formula_input <- function(out) {
  test <- out$dots[["test"]]
  if (is.null(test)) {
    abort_input(no_handler_reason(out$x))
  }
  known <- is.character(test) && length(test) == 1L && test %in% formula_tests()
  if (!known) {
    abort_input("`test` must be one of: ", paste0('"', formula_tests(), '"', collapse = ", "), ".")
  }
  if (!is.data.frame(out$dots[["data"]])) {
    abort_input("`data` must be a data frame.")
  }
  out
}

# A choice read from the result. An explicit choice of the caller that differs is refused.
read_choice <- function(out, name, value, label = name) {
  given <- out$dots[[name]]
  if (!is.null(given) && !isTRUE(all.equal(unname(given), unname(value), tolerance = 1e-12))) {
    abort_input(
      "`", name, " = ", paste(format(given), collapse = ", "), "` conflicts with this result: the ",
      attr(out$read, "fn"), " result records ", label, " = ", paste(format(value), collapse = ", "),
      ". Drop `", name, "`, or share from the formula instead."
    )
  }
  out$dots[[name]] <- value
  out
}

# `test` given explicitly must agree with what the result is (Student/Welch, paired, ...).
set_test <- function(out, value) {
  given <- out$dots[["test"]]
  if (!is.null(given) && !identical(given, value)) {
    abort_input(
      "`test = \"", given, "\"` conflicts with this ", attr(out$read, "fn"), " result: it is a \"",
      value, "\" test. Drop `test`, or share from the formula instead."
    )
  }
  out$dots[["test"]] <- value
  out
}

record_read <- function(out, ...) {
  fn <- attr(out$read, "fn")
  out$read <- structure(c(out$read, list(...)), fn = fn)
  out
}

# Formula pieces, built from names so that a column such as `log(y)` or `my var` works.
name_formula <- function(lhs = NULL, rhs) {
  f <- if (is.null(lhs)) call("~", rhs) else call("~", lhs, rhs)
  eval(f, envir = list(), enclos = asNamespace("stats"))
}

# aov ---------------------------------------------------------------------------------

prepare_aov <- function(out) {
  x <- out$x
  out$route <- "aov"
  mf <- x$model
  preds <- if (is.null(mf)) NULL else mf[-1L]
  why <- if (is.null(mf)) {
    "The aov fit holds no model frame, so the data cannot be read from it."
  } else if (!is.null(x$weights)) {
    "An aov with weights is not evaluable: the F tests are weighted."
  } else if (attr(stats::terms(x), "intercept") != 1L) {
    "An aov without an intercept is not evaluable."
  } else if (!is.numeric(mf[[1L]]) || is.matrix(mf[[1L]])) {
    "The aov outcome must be one numeric variable."
  } else if (!all(vapply(
    preds, function(p) is.factor(p) || is.character(p) || is.logical(p),
    logical(1)
  ))) {
    paste0(
      "An aov with a numeric covariate (an ANCOVA) is not evaluable; ",
      "reprokit handles factors only."
    )
  } else {
    NULL
  }
  if (!is.null(why)) {
    return(refuse_result(out, why))
  }
  labels <- attr(stats::terms(x), "term.labels")
  p <- names(preds)
  test <- if (length(p) == 1L && setequal(labels, p)) {
    "oneway"
  } else if (length(p) == 2L && setequal(labels, c(p, paste(p, collapse = ":")))) {
    "factorial"
  } else if (length(p) > 2L) {
    return(refuse_result(out, paste0(
      "An aov with ", length(p), " factors is not evaluable: reprokit handles one factor, ",
      "or two factors with their interaction."
    )))
  } else {
    return(refuse_result(out, paste0(
      "This aov (terms ", paste(labels, collapse = ", "), ") is not one factor, or two ",
      "factors with their interaction; reprokit does not evaluate other designs."
    )))
  }
  if (!is.null(out$dots[["test"]]) && !identical(out$dots[["test"]], test)) {
    abort_input(
      "`test = \"", out$dots[["test"]], "\"` conflicts with this aov: it is a ", test,
      " design."
    )
  }
  rhs <- if (test == "oneway") {
    as.name(p)
  } else {
    call("*", as.name(p[1L]), as.name(p[2L]))
  }
  out$x <- name_formula(as.name(names(mf)[1L]), rhs)
  out$dots[["data"]] <- data.frame(mf, check.names = FALSE, stringsAsFactors = FALSE)
  out$dots[["test"]] <- test
  out$expected <- list(kind = "aov", table = summary(x)[[1L]])
  out
}

# chisq.test, t.test, cor.test --------------------------------------------------------

prepare_htest <- function(out) {
  x <- out$x
  method <- trimws(paste(x$method, collapse = " "))
  if (grepl("Chi-squared", method, fixed = TRUE)) {
    attr(out$read, "fn") <- "chisq.test"
    out$route <- "chisq.test"
    return(prepare_chisq(out, method))
  }
  if (grepl("t-test", method, fixed = TRUE)) {
    attr(out$read, "fn") <- "t.test"
    out$route <- "t.test"
    return(prepare_t(out, method))
  }
  if (grepl("correlation", method, fixed = TRUE)) {
    attr(out$read, "fn") <- "cor.test"
    out$route <- "cor.test"
    return(prepare_cor(out, method))
  }
  refuse_result(out, paste0(
    "The result of '", method, "' is not a test reprokit evaluates (t.test, cor.test with ",
    "Pearson, chisq.test, aov and lm are)."
  ))
}

prepare_chisq <- function(out, method) {
  x <- out$x
  if (grepl("simulated", method, fixed = TRUE)) {
    return(refuse_result(out, paste0(
      "A chi-squared test with a simulated p value cannot be reproduced exactly: the p value ",
      "depends on random draws, so summaries do not suffice."
    )))
  }
  obs <- x$observed
  if (is.null(obs) || !is.numeric(obs)) {
    return(refuse_result(out, "The chisq.test result holds no table of observed counts."))
  }
  # The observed table goes to the handler's summarise code as it is (the counts path).
  out$dots[["data"]] <- NULL
  out$dots[["table"]] <- obs
  if (is.matrix(obs) && min(dim(obs)) >= 2L) {
    yates <- grepl("Yates", method, fixed = TRUE)
    correct <- if (all(dim(obs) == 2L)) yates else TRUE
    out$x <- name_formula(rhs = call("+", as.name("row"), as.name("column")))
    out <- set_test(out, "chisq")
    out <- read_choice(out, "correct", correct, "the continuity correction")
    read_test <- if (all(dim(obs) == 2L) && yates) "Yates" else "Pearson"
    out <- record_read(out, test = read_test)
    out$expected <- list(kind = "stat", values = c(
      chisq = unname(x$statistic),
      df = unname(x$parameter),
      p = unname(x$p.value)
    ))
  } else {
    expected <- x$expected
    out$x <- name_formula(rhs = as.name("category"))
    out <- set_test(out, "gof")
    out <- read_choice(
      out, "p0", as.numeric(expected) / sum(expected),
      "the H0 proportions"
    )
    out <- record_read(out, test = "Pearson (given probabilities)")
    out$expected <- list(kind = "stat", values = c(
      chisq = unname(x$statistic),
      df = unname(x$parameter),
      p = unname(x$p.value)
    ))
  }
  out
}

# The columns a t.test or cor.test result was computed from, read from its `data.name`:
# "y by g", "x and y", "x", "Pair(x, y)". NULL when a name is not a column of `data`.
htest_columns <- function(data_name, data) {
  data_name <- paste(data_name, collapse = " ")
  in_data <- function(v) length(v) > 0L && all(nzchar(v)) && all(v %in% names(data))
  by <- strsplit(data_name, " by ", fixed = TRUE)[[1L]]
  if (length(by) == 2L && in_data(by)) {
    return(list(kind = "by", cols = by))
  }
  pair <- regmatches(data_name, regexec("^Pair\\((.+), (.+)\\)$", data_name))[[1L]]
  if (length(pair) == 3L && in_data(pair[2:3])) {
    return(list(kind = "and", cols = pair[2:3]))
  }
  and <- strsplit(data_name, " and ", fixed = TRUE)[[1L]]
  if (length(and) == 2L && in_data(and)) {
    return(list(kind = "and", cols = and))
  }
  if (in_data(data_name)) {
    return(list(kind = "one", cols = data_name))
  }
  NULL
}

unmappable <- function(out, fn, hint) {
  refuse_result(out, paste0(
    "The data.name of the ", fn, " result ('", paste(out$x$data.name, collapse = " "),
    "') cannot be mapped to columns of `data`, so reprokit cannot rebuild the test. ", hint
  ))
}

result_data <- function(out, fn) {
  data <- out$dots[["data"]]
  if (!is.data.frame(data)) {
    abort_input(
      "A ", fn, " result does not contain the data. Pass the data frame it was ",
      "computed from as `data = `."
    )
  }
  data
}

read_alternative_and_level <- function(out, x, level = TRUE) {
  out <- read_choice(out, "alternative", x$alternative, "the alternative")
  conf <- attr(x$conf.int, "conf.level")
  if (level) {
    out <- read_choice(
      out, "conf_level", if (is.null(conf)) 0.95 else conf,
      "the confidence level"
    )
  }
  out
}

prepare_t <- function(out, method) {
  x <- out$x
  data <- result_data(out, "t.test")
  cols <- htest_columns(x$data.name, data)
  formula_hint <- paste0(
    "Use the formula route instead, e.g. sufficient_stats(y ~ group, ",
    "data = d, test = \"welch\")."
  )
  mu <- unname(x$null.value)
  if (grepl("^Paired", method)) {
    if (is.null(cols) || cols$kind != "and") {
      return(unmappable(out, "t.test", paste0(
        "Use the formula route instead: sufficient_stats(Pair(first, second) ~ 1, data = d, ",
        'test = "paired").'
      )))
    }
    out$x <- name_formula(rhs = 1, lhs = call(
      "Pair", as.name(cols$cols[1L]),
      as.name(cols$cols[2L])
    ))
    out <- set_test(out, "paired")
    out <- read_choice(out, "mu", mu, "the test value mu")
    variant <- "paired"
  } else if (grepl("^One Sample", method)) {
    if (is.null(cols) || cols$kind != "one") {
      return(unmappable(out, "t.test", paste0(
        "Use the formula route instead: sufficient_stats(y ~ 1, data = d, ",
        'test = "one_sample").'
      )))
    }
    out$x <- name_formula(rhs = 1, lhs = as.name(cols$cols))
    out <- set_test(out, "one_sample")
    out <- read_choice(out, "mu", mu, "the test value mu")
    variant <- "one-sample"
  } else {
    if (is.null(cols) || cols$kind != "by") {
      return(unmappable(out, "t.test", formula_hint))
    }
    if (!isTRUE(all.equal(mu, 0))) {
      return(refuse_result(out, paste0(
        "A two-sample t.test with a null value of ", mu, " (mu) is not evaluable: reprokit ",
        "tests a difference of 0."
      )))
    }
    out$x <- name_formula(as.name(cols$cols[1L]), as.name(cols$cols[2L]))
    welch <- grepl("^Welch", method)
    out <- set_test(out, if (welch) "welch" else "student")
    variant <- if (welch) "Welch" else "Student"
  }
  out <- read_alternative_and_level(out, x)
  out <- record_read(out, test = variant)
  out$expected <- list(kind = "stat", values = c(
    t = unname(x$statistic),
    df = unname(x$parameter),
    p = unname(x$p.value)
  ))
  out
}

prepare_cor <- function(out, method) {
  x <- out$x
  if (!grepl("Pearson", method, fixed = TRUE)) {
    return(refuse_result(out, paste0(
      "The ", sub("'s.*$", "", method), " correlation is a rank test and not evaluable: ",
      "the correlation of ranks needs the raw data, not summaries."
    )))
  }
  data <- result_data(out, "cor.test")
  cols <- htest_columns(x$data.name, data)
  if (is.null(cols) || cols$kind != "and") {
    return(unmappable(out, "cor.test", paste0(
      'Use the formula route instead: sufficient_stats(~ x + y, data = d, test = "pearson").'
    )))
  }
  out$x <- name_formula(rhs = call("+", as.name(cols$cols[1L]), as.name(cols$cols[2L])))
  out <- set_test(out, "pearson")
  out <- read_alternative_and_level(out, x)
  out <- record_read(out, test = "Pearson")
  out$expected <- list(kind = "stat", values = c(
    t = unname(x$statistic),
    df = unname(x$parameter),
    p = unname(x$p.value)
  ))
  out
}

# The match check (ADR 0026 point 4) -------------------------------------------------

# NULL when the summaries reproduce the result's own numbers, otherwise the reason.
match_problem <- function(handler, summary, prep) {
  expected <- prep$expected
  if (is.null(expected) || is.null(handler$reproduce)) {
    return(NULL)
  }
  s <- list(stats = summary$stats, meta = utils::modifyList(default_meta(), summary$meta))
  if (expected$kind == "aov") {
    return(match_aov(handler, s, expected$table, prep$route))
  }
  got <- handler$reproduce(s)
  differences <- scaled_differences(expected$values, got[names(expected$values)])
  if (all(differences < match_tolerance)) {
    return(NULL)
  }
  worst <- names(differences)[which.max(differences)]
  paste0(
    "The ", attr(prep$read, "fn"), " result does not follow from this data: ", worst, " is ",
    format(expected$values[[worst]], digits = 10), " in the result but ",
    format(got[[worst]], digits = 10), " from the data. Check that `data` is the data frame ",
    "the result was computed from, with the same rows (no subset, same missing-value ",
    "handling), or use the formula route."
  )
}

# ADR 0005's tolerance: scaled difference |a - b| / max(1, |a|) below 1e-8.
match_tolerance <- 1e-8

scaled_differences <- function(expected, got) {
  d <- ifelse(expected == got, 0, abs(expected - got) / pmax(1, abs(expected)))
  d[is.na(d)] <- Inf
  d
}

# summary(aov) is sequential (type I). The summaries must reproduce that table: for a factorial
# design, type I for the terms in the order of the fit (the first factor is type I, the second
# is the same as type II, the interaction is the same in every type).
match_aov <- function(handler, s, table, route) {
  # The aov table is sequential: the first term is type I, the second (given the first) is
  # type II, the interaction is the same in every type.
  reproduced <- if (identical(handler$name, "anova_factorial")) {
    s1 <- s
    s1$meta$ss_type <- "I"
    s2 <- s
    s2$meta$ss_type <- "II"
    r1 <- handler$reproduce(s1)
    r2 <- handler$reproduce(s2)
    r <- r1
    r[grepl("_B$", names(r))] <- r2[grepl("_B$", names(r))]
    r
  } else {
    handler$reproduce(s)
  }
  rows <- trimws(rownames(table))
  rows <- rows[rows != "Residuals"]
  f_of <- table[["F value"]]
  p_of <- table[["Pr(>F)"]]
  df_of <- table[["Df"]]
  if (identical(handler$name, "anova_factorial")) {
    effects <- c("A", "B", "AB")
    expected <- c(
      unlist(lapply(seq_along(effects), function(i) {
        stats::setNames(
          c(f_of[i], df_of[i], p_of[i]),
          paste0(c("f", "df1", "p"), "_", effects[i])
        )
      })),
      df2 = df_of[length(df_of)]
    )
  } else {
    expected <- c(f = f_of[1L], df1 = df_of[1L], df2 = df_of[2L], p = p_of[1L])
    names(expected) <- c("f", "df1", "df2", "p")
  }
  got <- reproduced[names(expected)]
  if (anyNA(got)) {
    return(paste0("The aov table could not be compared with the stored summaries, ",
                  "so reprokit does not evaluate this fit."))
  }
  differences <- scaled_differences(expected, got)
  if (all(differences < match_tolerance)) {
    return(NULL)
  }
  paste0("The aov table does not follow from its model frame (", names(differences)[
    which.max(differences)
  ], " differs); reprokit does not evaluate this fit.")
}

# Two models: mediation (ADR 0028) -----------------------------------------------------

# The mediator model (M ~ X) and the outcome model (Y ~ X + M) as two lm fits, in either
# order: the roles are read from the predictors. The fits become the data, the formula
# `Y ~ X` and `mediator = "M"` of the formula route, which then runs unchanged.
prepare_mediation <- function(out, second) {
  fits <- c(list(out$x), second)
  if (length(fits) != 2L) {
    abort_input(
      "A simple mediation is two models, M ~ X and Y ~ X + M; got ",
      length(fits), "."
    )
  }
  out$route <- "lm"
  nm <- names(out$dots)
  out$dots <- if (is.null(nm)) list() else out$dots[nzchar(nm)]
  not_simple <- function(why) refuse_result(out, paste0("A mediation from two models: ", why))
  for (f in fits) {
    if (!identical(class(f), "lm")) {
      return(not_simple("both must be plain lm fits."))
    }
    if (!is.null(f$weights)) {
      return(not_simple("weights are not evaluable."))
    }
    if (attr(stats::terms(f), "intercept") != 1L) {
      return(not_simple("a model without an intercept is not evaluable."))
    }
    if (is.null(f$model) || !is.numeric(f$model[[1L]]) || is.matrix(f$model[[1L]])) {
      return(not_simple("the outcome of each model must be one numeric variable."))
    }
  }
  info <- lapply(fits, function(f) {
    list(response = names(f$model)[1L], predictors = names(f$model)[-1L])
  })
  fits_roles <- function(med, out_) {
    length(info[[med]]$predictors) == 1L &&
      setequal(info[[out_]]$predictors, c(info[[med]]$predictors, info[[med]]$response)) &&
      length(info[[out_]]$predictors) == 2L
  }
  roles <- if (fits_roles(1L, 2L)) c(1L, 2L) else if (fits_roles(2L, 1L)) c(2L, 1L) else NULL
  if (is.null(roles)) {
    related <- function(med, out_) info[[med]]$response %in% info[[out_]]$predictors
    if (related(1L, 2L) || related(2L, 1L)) {
      return(not_simple(paste0(
        "covariates, extra mediators or other terms are outside the ",
        "simple mediation (M ~ X and Y ~ X + M), so it is not evaluable."
      )))
    }
    abort_input(
      "These two models fit neither role: the mediator model M ~ X and the outcome model ",
      "Y ~ X + M, where M is the response of the first. Got ",
      paste(vapply(
        fits, function(f) paste(deparse(stats::formula(f)), collapse = ""),
        character(1)
      ), collapse = " and "), "."
    )
  }
  med <- fits[[roles[1L]]]
  outc <- fits[[roles[2L]]]
  xn <- info[[roles[1L]]]$predictors
  mn <- info[[roles[1L]]]$response
  yn <- info[[roles[2L]]]$response
  mf_m <- med$model
  mf_o <- outc$model
  same <- nrow(mf_m) == nrow(mf_o) && identical(unname(mf_m[[xn]]), unname(mf_o[[xn]])) &&
    identical(unname(mf_m[[mn]]), unname(mf_o[[mn]]))
  if (!same) {
    abort_impossible(
      "share a mediation from these two models",
      paste0(
        "they were not fitted to the same cases: the two fits need the same n, identical ",
        "values of X (", xn, ") and the mediator (", mn, ") in the outcome model equal to ",
        "the mediator model's response. Fit both on the same rows."
      ),
      alternatives = character()
    )
  }
  if (anyDuplicated(c(xn, mn, yn))) {
    return(not_simple("X, M and Y must be three different variables."))
  }
  data <- data.frame(mf_m[[xn]], mf_m[[mn]], mf_o[[yn]], check.names = FALSE)
  names(data) <- c(xn, mn, yn)
  out$x <- name_formula(as.name(yn), as.name(xn))
  out$dots[["data"]] <- data
  out$dots[["test"]] <- "mediation"
  out$dots[["mediator"]] <- mn
  out
}
