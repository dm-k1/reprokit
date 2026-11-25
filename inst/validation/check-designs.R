# Shared generators of correctly reported papers for the check_reported() validation scripts
# (check-false-positives.R, check-planted-errors.R, check-statcheck.R). Each generator draws raw
# data, runs the real t.test() / cor.test(), and returns the numbers rounded as a paper prints
# them (2 decimals for t, 3 for p and r, "< .001" below .001). Sourced, not run: it draws nothing
# until a generator is called, so the caller owns the seed.
# One-way ANOVA: oneway_raw() draws k = 2..6 groups, balanced or not, and
# oneway_paper() prints classical oneway.test(var.equal = TRUE) output as a paper does
# (F 2 dp, p 3 dp or "< .001", M and SD 2 dp, eta2 or omega2 2 dp).

# The rounding a paper used: round half to even (what sprintf, formatC and round() do, on the
# binary value) or half up / away from zero on the decimal value (SPSS, Excel; the 1e-9 stands
# for the decimal arithmetic of those programs, so that 2.675 prints as 2.68). Chosen with
# options(reprokit.validation.rounding = "half_up"); default half even. They differ only at an
# exact tie such as a mean of 2.5 printed at 0 decimals, which integer (Likert) data produce.
fmt <- function(x, d) {
  if (identical(getOption("reprokit.validation.rounding"), "half_up")) {
    x <- sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d
  }
  formatC(x, format = "f", digits = d)
}
with_rounding <- function(mode, expr) {
  old <- options(reprokit.validation.rounding = mode)
  on.exit(options(old))
  expr
}
# The same draw printed both ways: the RNG state is restored before the second paper, so the
# two papers differ only in the rounding. Returns a list of the papers that are not NULL.
both_roundings <- function(make) {
  seed <- get(".Random.seed", globalenv())
  even <- with_rounding("half_even", make())
  assign(".Random.seed", seed, globalenv())
  up <- with_rounding("half_up", make())
  Filter(Negate(is.null), list(even = even, half_up = up))
}
fmt_p <- function(p) if (p < 0.001) "< .001" else fmt(p, 3L)
# Confidence level of the CI a paper prints: 95% mostly, 90% and 99% in a quarter of the papers
# each. The generators draw it with the global RNG.
draw_level <- function() sample(c(0.90, 0.95, 0.99), 1L, prob = c(0.25, 0.5, 0.25))
sizes <- c(3, 3, 4, 4, 5, 6, 8, 10, 15, 20, 30, 60, 150)
rhos <- c(0, 0.3, 0.7, 0.95, 0.99, 0.995, 0.999, 0.9999, -0.5, -0.9, -0.999)

paired_design <- function() {
  n <- sample(sizes, 1L)
  scale <- 10^runif(1L, -1, 2)
  rho <- if (runif(1L) < 0.5) sample(rhos, 1L) else runif(1L, -0.99, 0.99)
  x <- rnorm(n) * scale * runif(1L, 0.5, 2) + runif(1L, -1, 1) * scale
  z <- (x - mean(x)) / sd(x)
  e <- rnorm(n)
  e <- resid(lm(e ~ z))
  y <- rho * z * sd(x) + sqrt(1 - rho^2) * e / sd(e) * sd(x) * runif(1L, 0.5, 1.5) +
    runif(1L, -1, 1) * scale * 0.2
  list(x = x, y = y, dec = sample(c(0L, 2L, 3L), 1L, prob = c(0.3, 0.4, 0.3)))
}

paired_paper <- function(d, alternative = "two.sided") {
  level <- draw_level()
  tt <- t.test(d$x, d$y, paired = TRUE, conf.level = level)
  r <- cor(d$x, d$y)
  if (!is.finite(r) || !is.finite(tt$statistic)) return(NULL)
  dec <- d$dec
  n <- length(d$x)
  paper <- list(n = as.character(n), m = c(fmt(mean(d$x), dec), fmt(mean(d$y), dec)),
                sd = c(fmt(sd(d$x), dec), fmt(sd(d$y), dec)), r = fmt(r, 3L),
                t = fmt(unname(tt$statistic), 2L), df = as.character(n - 1L),
                p = fmt_p(if (alternative == "two.sided") tt$p.value else
                  t.test(d$x, d$y, paired = TRUE, alternative = alternative)$p.value),
                ci_lower = fmt(tt$conf.int[1L], dec), ci_upper = fmt(tt$conf.int[2L], dec))
  # In 30% of the papers also a d, in one of the conventions of Lakens (2013), 2 decimals.
  if (runif(1L) < 0.3) {
    diff <- d$x - d$y
    d_z <- mean(diff) / sd(diff)
    convention <- sample(c("d_z", "d_rm", "d_av"), 1L)
    value <- switch(convention, d_z = d_z, d_rm = d_z * sqrt(2 * (1 - r)),
                    d_av = mean(diff) / ((sd(d$x) + sd(d$y)) / 2))
    if (is.finite(value)) paper$d <- fmt(value, 2L)
  }
  attr(paper, "level") <- level
  attr(paper, "alternative") <- alternative
  paper
}

pearson_paper <- function(raw = NULL, alternative = "two.sided") {
  if (is.null(raw)) {
    n <- sample(sizes, 1L)
    rho <- if (runif(1L) < 0.5) sample(rhos, 1L) else runif(1L, -0.99, 0.99)
    x <- rnorm(n)
    y <- rho * x + sqrt(1 - rho^2) * rnorm(n)
  } else {
    x <- raw$x
    y <- raw$y
    n <- length(x)
  }
  r <- suppressWarnings(cor(x, y))
  if (!is.finite(r) || abs(r) >= 1) return(NULL)  # |r| = 1 has t = Inf: nothing to print
  level <- draw_level()
  ct <- cor.test(x, y, conf.level = level)
  dec <- sample(2:3, 1L)
  p_value <- if (alternative == "two.sided") ct$p.value else
    cor.test(x, y, alternative = alternative)$p.value
  paper <- list(n = as.character(n), r = fmt(unname(ct$estimate), dec),
                df = as.character(n - 2L), t = fmt(unname(ct$statistic), 2L),
                p = fmt_p(p_value))
  # R squared and its adjusted value (cross-complete quantities of the test): printed with r
  r_hat <- unname(ct$estimate)
  paper$r2 <- fmt(r_hat^2, dec)
  if (n > 3L) paper$adj_r2 <- fmt(1 - (1 - r_hat^2) * (n - 1) / (n - 2), dec)
  if (n > 3L) {
    paper <- c(paper, list(ci_lower = fmt(ct$conf.int[1L], dec),
                           ci_upper = fmt(ct$conf.int[2L], dec)))
  }
  attr(paper, "level") <- level
  attr(paper, "alternative") <- alternative
  paper
}

# `raw = list(x, y)` supplies the data (the Likert families), `dec` the decimals of M, SD and the CI,
# `df_dec` the decimals of a Welch df (default 1 or 2; 0 prints it as an integer, which a paper
# that rounds the df does), `alternative` the tail of the p printed (the CI stays two-sided).
independent_paper <- function(var_equal, raw = NULL, dec = NULL, df_dec = NULL,
                              alternative = "two.sided") {
  if (is.null(raw)) {
    n1 <- sample(sizes[sizes >= 3], 1L)
    n2 <- sample(sizes[sizes >= 3], 1L)
    scale <- 10^runif(1L, -1, 2)
    x <- rnorm(n1) * scale * runif(1L, 0.3, 3) + runif(1L, -1, 1) * scale
    y <- rnorm(n2) * scale * runif(1L, 0.3, 3) + runif(1L, -1, 1) * scale
  } else {
    x <- raw$x
    y <- raw$y
    n1 <- length(x)
    n2 <- length(y)
  }
  level <- draw_level()
  tt <- t.test(x, y, var.equal = var_equal, conf.level = level)
  if (!is.finite(tt$statistic) || sd(x) == 0 || sd(y) == 0) return(NULL)
  p_value <- if (alternative == "two.sided") tt$p.value else
    t.test(x, y, var.equal = var_equal, alternative = alternative)$p.value
  if (is.null(dec)) dec <- sample(c(0L, 2L, 3L), 1L, prob = c(0.3, 0.4, 0.3))
  if (is.null(df_dec)) df_dec <- if (var_equal) 0L else sample(1:2, 1L)
  pooled <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  paper <- list(n = as.character(c(n1, n2)), m = c(fmt(mean(x), dec), fmt(mean(y), dec)),
                sd = c(fmt(sd(x), dec), fmt(sd(y), dec)),
                t = fmt(unname(tt$statistic), 2L), df = fmt(unname(tt$parameter), df_dec),
                p = fmt_p(p_value), ci_lower = fmt(tt$conf.int[1L], dec),
                ci_upper = fmt(tt$conf.int[2L], dec))
  if (runif(1L) < 0.3) paper$d <- fmt((mean(x) - mean(y)) / pooled, 2L)
  attr(paper, "level") <- level
  attr(paper, "alternative") <- alternative
  paper
}

one_sample_paper <- function(raw = NULL, dec = NULL, mu = NULL, alternative = "two.sided") {
  if (is.null(raw)) {
    n <- sample(sizes, 1L)
    scale <- 10^runif(1L, -1, 2)
    x <- rnorm(n) * scale + runif(1L, -1, 1) * scale
    mu <- if (runif(1L) < 0.4) 0 else round(runif(1L, -1, 1) * scale, sample(0:2, 1L))
  } else {
    x <- raw$x
    n <- length(x)
  }
  level <- draw_level()
  tt <- t.test(x, mu = mu, conf.level = level)
  if (!is.finite(tt$statistic) || sd(x) == 0) return(NULL)
  if (is.null(dec)) dec <- sample(c(0L, 2L, 3L), 1L, prob = c(0.3, 0.4, 0.3))
  p_value <- if (alternative == "two.sided") tt$p.value else
    t.test(x, mu = mu, alternative = alternative)$p.value
  paper <- list(n = as.character(n), m = fmt(mean(x), dec), sd = fmt(sd(x), dec),
                t = fmt(unname(tt$statistic), 2L), df = as.character(n - 1L),
                p = fmt_p(p_value), ci_lower = fmt(tt$conf.int[1L], dec),
                ci_upper = fmt(tt$conf.int[2L], dec))
  attr(paper, "mu") <- mu
  attr(paper, "level") <- level
  attr(paper, "alternative") <- alternative
  paper
}

# Integer data (Likert items, counts) -----------------------------------------------------------
# Means of n integers are multiples of 1/n, so with n in 4, 8, 10, 16, 20, 24, 40, 50 a mean lands
# on an exact tie at 0, 1 or 2 decimals (2.5 at 0 decimals, 3.25 at 1, 3.125 at 2): the cases
# where printing with round-half-even and with round-half-up differ. likert_values() draws a
# 1-5 or 1-7 rating or a count (Poisson); the likert_*_paper() functions give the paper of a
# t.test()/cor.test() on such data (decimals 0 to 2, so ties occur), `alternative` as for the
# continuous generators. Use both_roundings() to print the same data under both conventions.
tie_sizes <- c(4, 8, 10, 16, 20, 24, 40, 50)
likert_values <- function(n, kind, shift = 0) {
  switch(kind,
         l5 = pmin(5, pmax(1, round(rnorm(n, 3 + shift, 1.1)))),
         l7 = pmin(7, pmax(1, round(rnorm(n, 4 + shift, 1.7)))),
         count = as.numeric(rpois(n, 6 + 3 * shift)))
}
likert_kind <- function() sample(c("l5", "l7", "count"), 1L)
likert_dec <- function() sample(c(0L, 1L, 2L), 1L, prob = c(0.2, 0.4, 0.4))
likert_independent_paper <- function(var_equal, alternative = "two.sided", df_dec = NULL) {
  kind <- likert_kind()
  x <- likert_values(sample(tie_sizes, 1L), kind)
  y <- likert_values(sample(tie_sizes, 1L), kind, shift = runif(1L, -0.5, 0.5))
  independent_paper(var_equal, raw = list(x = x, y = y), dec = likert_dec(), df_dec = df_dec,
                    alternative = alternative)
}
likert_one_sample_paper <- function(alternative = "two.sided") {
  kind <- likert_kind()
  x <- likert_values(sample(tie_sizes, 1L), kind)
  mu <- switch(kind, l5 = 3, l7 = 4, count = sample(c(5, 6, 7), 1L))
  one_sample_paper(raw = list(x = x), dec = likert_dec(), mu = mu, alternative = alternative)
}
likert_paired_paper <- function(alternative = "two.sided") {
  kind <- likert_kind()
  n <- sample(tie_sizes, 1L)
  x <- likert_values(n, kind)
  y <- x + sample(-1:1, n, replace = TRUE) + sample(0:1, 1L)
  y <- if (kind == "l5") pmin(5, pmax(1, y)) else if (kind == "l7") pmin(7, pmax(1, y)) else pmax(0, y)
  paired_paper(list(x = x, y = y, dec = likert_dec()), alternative = alternative)
}
likert_pearson_paper <- function(alternative = "two.sided") {
  kind <- likert_kind()
  n <- sample(tie_sizes, 1L)
  x <- likert_values(n, kind)
  y <- x + sample(-2:2, n, replace = TRUE)
  y <- if (kind == "l5") pmin(5, pmax(1, y)) else if (kind == "l7") pmin(7, pmax(1, y)) else pmax(0, y)
  pearson_paper(raw = list(x = x, y = y), alternative = alternative)
}
# TRUE when the two papers of a both_roundings() draw differ: an exact tie was met.
tie_hit <- function(papers) {
  length(papers) == 2L && !identical(unclass(papers[[1L]]), unclass(papers[[2L]]))
}

# One-way ANOVA ------------------------------------------------------------------------------
# Raw data of k groups (k drawn from 2..6 unless given), equal sizes if `balanced`, otherwise
# sizes drawn independently; group means and SDs differ. Returns list(y, g, k, n).
oneway_raw <- function(k = sample(2:6, 1L), balanced = runif(1L) < 0.5, null = FALSE) {
  pool <- sizes[sizes >= 3]
  n <- if (balanced) rep(sample(pool, 1L), k) else sample(pool, k, replace = TRUE)
  scale <- 10^runif(1L, -1, 2)
  effect <- if (null) 0 else runif(1L, 0, 1.5) * scale
  y <- unlist(lapply(seq_len(k), function(i) {
    rnorm(n[i]) * scale * runif(1L, 0.5, 2) + rnorm(1L) * effect
  }))
  list(y = y, g = factor(rep(letters[seq_len(k)], n)), k = k, n = n)
}

# The classical one-way results of a raw design: list(f, df1, df2, p, eta2, omega2).
oneway_truth <- function(d) {
  fit <- summary(stats::aov(y ~ g, data = data.frame(y = d$y, g = d$g)))[[1L]]
  ss <- fit[["Sum Sq"]]
  f <- fit[["F value"]][1L]
  df1 <- fit[["Df"]][1L]
  df2 <- fit[["Df"]][2L]
  list(f = f, df1 = df1, df2 = df2, p = fit[["Pr(>F)"]][1L], eta2 = ss[1L] / sum(ss),
       omega2 = df1 * (f - 1) / (df1 * f + df2 + 1))
}

# A correctly reported one-way ANOVA; NULL when the draw is degenerate. eta2 is printed as
# eta squared or, half of the time, as omega squared; a negative omega squared (F < 1) is
# printed with its sign (ADR 0022). `negative_omega = TRUE` forces omega squared and returns NULL
# unless it prints as a negative number.
oneway_paper <- function(d = oneway_raw(), negative_omega = FALSE) {
  truth <- oneway_truth(d)
  sds <- tapply(d$y, d$g, sd)
  if (!is.finite(truth$f) || any(!is.finite(sds)) || any(sds == 0)) return(NULL)
  if (negative_omega && !(round(truth$omega2, 2L) < 0)) return(NULL)
  effect <- if (!negative_omega && runif(1L) < 0.5) truth$eta2 else truth$omega2
  list(n = as.character(d$n), m = fmt(tapply(d$y, d$g, mean), 2L), sd = fmt(sds, 2L),
       f = fmt(truth$f, 2L), df1 = as.character(truth$df1), df2 = as.character(truth$df2),
       p = fmt_p(truth$p), eta2 = fmt(effect, 2L), r2 = fmt(truth$eta2, 2L))
}

# Factorial ANOVA (ADR 0017) ---------------------------------------------------------------
# factorial_raw() draws a x b = 2..3 x 2..4 cells of raw data, balanced or not (cell sizes at
# least 3), with unequal cell means and SDs; factorial_paper() prints one effect (A, B or AB) of
# the classical type I, II or III table as a paper does (F 2 dp, p 3 dp or "< .001", cell M and SD
# 2 dp, partial eta2 or partial omega2 2 dp, cell n), the cell matrices as a x b matrices, and
# sets the attributes `effect`, `dims` and `truth` (the SS type it was printed at, df).
# Truth is computed here from stats::lm() with anova()/drop1(), independently of reprokit's handler.
factorial_raw <- function(dims = c(sample(2:3, 1L), sample(2:4, 1L)),
                          balanced = runif(1L) < 0.4) {
  cells <- prod(dims)
  pool <- sizes[sizes >= 3 & sizes <= 60]
  n <- if (balanced) rep(sample(pool, 1L), cells) else sample(pool, cells, replace = TRUE)
  scale <- 10^runif(1L, -1, 2)
  effect <- runif(1L, 0, 1.5) * scale
  mu <- rnorm(cells) * effect
  sds <- runif(cells, 0.5, 2) * scale
  cell <- rep(seq_len(cells), n)
  list(d = data.frame(y = rnorm(length(cell), mu[cell], sds[cell]),
                      a = factor(((cell - 1L) %% dims[1L]) + 1L, levels = seq_len(dims[1L])),
                      b = factor(((cell - 1L) %/% dims[1L]) + 1L, levels = seq_len(dims[2L]))),
       dims = dims, balanced = balanced)
}

factorial_truth <- function(d, type) {
  full <- stats::lm(y ~ a * b, d)
  ss <- switch(type,
    I = c(A = anova(full)["a", "Sum Sq"], B = anova(stats::lm(y ~ b * a, d))["b", "Sum Sq"],
          AB = anova(full)["a:b", "Sum Sq"]),
    II = {
      dr <- stats::drop1(stats::lm(y ~ a + b, d), scope = ~ a + b)
      c(A = dr["a", "Sum of Sq"], B = dr["b", "Sum of Sq"], AB = anova(full)["a:b", "Sum Sq"])
    },
    III = {
      dr <- stats::drop1(stats::lm(y ~ a * b, d, contrasts = list(a = "contr.sum", b = "contr.sum")),
                         scope = ~ a + b + a:b)
      c(A = dr["a", "Sum of Sq"], B = dr["b", "Sum of Sq"], AB = dr["a:b", "Sum of Sq"])
    })
  df1 <- c(A = nlevels(d$a) - 1, B = nlevels(d$b) - 1, AB = (nlevels(d$a) - 1) * (nlevels(d$b) - 1))
  df2 <- stats::df.residual(full)
  sse <- stats::deviance(full)
  f <- (ss / df1) / (sse / df2)
  list(f = f, df1 = df1, df2 = df2, p = stats::pf(f, df1, df2, lower.tail = FALSE),
       eta2 = ss / (ss + sse), omega2 = df1 * (f - 1) / (df1 * (f - 1) + df2 + nlevels(d$a) * nlevels(d$b)))
}

factorial_paper <- function(raw = factorial_raw(), effect = sample(c("A", "B", "AB"), 1L),
                            type = sample(c("III", "II", "I"), 1L)) {
  d <- raw$d
  sds <- tapply(d$y, list(d$a, d$b), sd)
  if (any(!is.finite(sds)) || any(sds == 0)) return(NULL)
  truth <- factorial_truth(d, type)
  if (!is.finite(truth$f[[effect]]) || truth$f[[effect]] <= 0) return(NULL)
  size <- if (runif(1L) < 0.5) truth$eta2[[effect]] else truth$omega2[[effect]]
  paper <- list(n = matrix(as.character(as.vector(table(d$a, d$b))), raw$dims[1L]),
                m = matrix(fmt(as.vector(tapply(d$y, list(d$a, d$b), mean)), 2L), raw$dims[1L]),
                sd = matrix(fmt(as.vector(sds), 2L), raw$dims[1L]),
                f = fmt(truth$f[[effect]], 2L), df1 = as.character(truth$df1[[effect]]),
                df2 = as.character(truth$df2), p = fmt_p(truth$p[[effect]]),
                eta2 = fmt(size, 2L), partial_eta2 = fmt(truth$eta2[[effect]], 2L),
                partial_omega2 = fmt(truth$omega2[[effect]], 2L))
  attr(paper, "effect") <- effect
  attr(paper, "dims") <- raw$dims
  attr(paper, "balanced") <- raw$balanced
  attr(paper, "type") <- type
  paper
}

# check_reported("factorial") on the named numbers of a factorial paper; an error is returned, not thrown.
run_factorial_check <- function(paper, keys) {
  args <- c(paper[keys], list(effect = attr(paper, "effect"), dims = attr(paper, "dims")))
  tryCatch(suppressWarnings(do.call(check_reported, c(list(test = "factorial"), args))), error = function(e) e)
}

# Planted errors ----------------------------------------------------------------
# Each planter takes a correct paper (named list of printed strings) and returns
# list(paper, quantity, size) with ONE number changed, or NULL when the paper cannot take
# this error (the caller draws another). They use the global RNG, so the caller owns the seed.

n_decimals <- function(s) nchar(sub("^[^.]*\\.?", "", s))

# The paper's p as a number, or NA for "< .001".
p_value <- function(s) if (grepl("<", s, fixed = TRUE)) NA_real_ else as.numeric(s)

plant_t_digit <- function(paper) {
  t0 <- as.numeric(paper$t)
  if (t0 == 0) return(NULL)
  units <- sample(1:9, 1L)
  step <- sample(c(-1, 1), 1L) * units / 100
  if (t0 * (t0 + step) <= 0) step <- -step
  if (t0 * (t0 + step) <= 0) return(NULL)
  paper$t <- fmt(t0 + step, 2L)
  list(paper = paper, quantity = "t", size = if (units <= 2) "small" else "large")
}

# Change one decimal digit of a number printed as "0.412" (sign kept, magnitude below 1).
change_digit <- function(s) {
  digits <- n_decimals(s)
  neg <- startsWith(s, "-")
  mag <- abs(as.numeric(s))
  pos <- sample(digits, 1L)
  place <- 10^-pos
  current <- floor(mag / place + 1e-9) %% 10
  new_digit <- sample(setdiff(0:9, current), 1L)
  new <- mag + (new_digit - current) * place
  if (new <= 0 || new >= 1 || mag >= 1) return(NULL)
  list(value = fmt(if (neg) -new else new, digits), last = pos == digits)
}

plant_p_digit <- function(paper) {
  if (is.na(p_value(paper$p))) return(NULL)
  changed <- change_digit(paper$p)
  if (is.null(changed)) return(NULL)
  paper$p <- changed$value
  list(paper = paper, quantity = "p", size = if (changed$last) "small" else "large")
}

plant_r_digit <- function(paper) {
  changed <- change_digit(paper$r)
  if (is.null(changed)) return(NULL)
  paper$r <- changed$value
  list(paper = paper, quantity = "r", size = if (changed$last) "small" else "large")
}

# df from the wrong n: n (not n - 1) for paired and one-sample, n1 + n2 for independent,
# n - 1 (not n - 2) for r. Size is the n of the paper: the error is invisible when n is large.
plant_df_wrong_n <- function(paper, handler) {
  n <- as.numeric(paper$n)
  wrong <- switch(handler, t_paired = n, t_one_sample = n, t_independent = sum(n),
                  cor_pearson = n - 1)
  paper$df <- fmt(wrong, n_decimals(paper$df))
  list(paper = paper, quantity = "df", size = if (sum(n) <= 10) "n<=10" else "n>10")
}

# One M (or the mean difference, through one M) off by 5 to 20 units of its last printed digit.
plant_m_change <- function(paper) {
  which <- sample(length(paper$m), 1L)
  units <- sample(5:20, 1L) * sample(c(-1, 1), 1L)
  digits <- n_decimals(paper$m[which])
  paper$m[which] <- fmt(as.numeric(paper$m[which]) + units * 10^-digits, digits)
  list(paper = paper, quantity = "m", size = if (abs(units) < 10) "small" else "large")
}

# One CI bound off by 5 to 20 units of its last printed digit.
plant_ci_change <- function(paper) {
  if (is.null(paper$ci_lower)) return(NULL)
  which <- sample(c("ci_lower", "ci_upper"), 1L)
  units <- sample(5:20, 1L) * sample(c(-1, 1), 1L)
  digits <- n_decimals(paper[[which]])
  paper[[which]] <- fmt(as.numeric(paper[[which]]) + units * 10^-digits, digits)
  list(paper = paper, quantity = which, size = if (abs(units) < 10) "small" else "large")
}

# Two adjacent digits of the test statistic transposed ("2.34" -> "2.43", "12.50" -> "15.20": the
# sign, the decimal point and a leading position are kept in place). The statistic is t, r (Pearson)
# or F (one-way). Size is small when the change is under 0.1 of its value in absolute terms
# (the two digits are the last two), else large.
plant_transposed <- function(paper, handler) {
  key <- switch(handler, cor_pearson = "r", anova_oneway = "f", "t")
  s <- paper[[key]]
  chars <- strsplit(s, "")[[1L]]
  digit <- which(chars %in% as.character(0:9))
  pairs <- digit[-length(digit)][diff(digit) == 1L]
  # a pair of equal digits changes nothing; a leading "0" before the point stays (0.43 -> 0.34 is fine)
  pairs <- pairs[chars[pairs] != chars[pairs + 1L]]
  if (!length(pairs)) return(NULL)
  at <- if (length(pairs) == 1L) pairs else sample(pairs, 1L)
  chars[c(at, at + 1L)] <- chars[c(at + 1L, at)]
  new <- paste(chars, collapse = "")
  if (handler == "cor_pearson" && abs(as.numeric(new)) >= 1) return(NULL)
  paper[[key]] <- new
  size <- if (abs(as.numeric(new) - as.numeric(s)) < 0.1) "small" else "large"
  list(paper = paper, quantity = key, size = size)
}

# A one-tailed p reported as if it were two-tailed: the paper's p replaced by half the two-sided
# p of its t and df (the sign of t kept). A one-tailed test is a legitimate analysis, so the
# result is expected to be UNDETECTABLE, not a miss: check_reported() accepts a p from either
# tail unless the caller says two.sided. Counted in the table, outside the "flags something" and
# "large errors are found" criteria.
plant_one_tailed <- function(paper) {
  p0 <- p_value(paper$p)
  if (is.na(p0) || p0 < 0.004 || is.null(paper$t)) return(NULL)
  paper$p <- fmt_p(p0 / 2)
  list(paper = paper, quantity = "p", size = "large")
}

# p moved across .05 with t and df unchanged.
plant_decision <- function(paper) {
  p0 <- p_value(paper$p)
  significant <- is.na(p0) || p0 < 0.05
  paper$p <- fmt_p(if (significant) runif(1L, 0.055, 0.6) else runif(1L, 0.002, 0.044))
  list(paper = paper, quantity = "p", size = "large")
}

plant_f_digit <- function(paper) {
  f0 <- as.numeric(paper$f)
  units <- sample(1:9, 1L)
  step <- sample(c(-1, 1), 1L) * units / 100
  if (f0 + step <= 0) step <- abs(step)
  paper$f <- fmt(f0 + step, 2L)
  list(paper = paper, quantity = "f", size = if (units <= 2) "small" else "large")
}

# df2 from the wrong n: N (not N - k) or N - k + 1. Size is the total n.
plant_oneway_df <- function(paper) {
  n <- as.numeric(paper$n)
  paper$df2 <- as.character(sum(n) - length(n) + sample(c(1, length(n)), 1L))
  list(paper = paper, quantity = "df2", size = if (sum(n) <= 10) "n<=10" else "n>10")
}

planted_types <- function(handler) {
  if (handler == "anova_oneway") {
    return(c("f_digit", "p_digit", "df_wrong_n", "decision", "m_change", "transposed"))
  }
  c("t_digit", "p_digit", "df_wrong_n", "decision",
    if (handler == "cor_pearson") "r_digit" else "m_change", "ci_change", "transposed", "one_tailed")
}

plant_error <- function(paper, handler, type) {
  if (handler == "anova_oneway" && type %in% c("f_digit", "df_wrong_n")) {
    return(if (type == "f_digit") plant_f_digit(paper) else plant_oneway_df(paper))
  }
  switch(type, t_digit = plant_t_digit(paper), p_digit = plant_p_digit(paper),
         df_wrong_n = plant_df_wrong_n(paper, handler), decision = plant_decision(paper),
         r_digit = plant_r_digit(paper), m_change = plant_m_change(paper),
         ci_change = plant_ci_change(paper), transposed = plant_transposed(paper, handler),
         one_tailed = plant_one_tailed(paper))
}

# Groups of correct papers, one per handler (independent t split by test), and a driver
# that draws papers with one planted error each. The error type rotates through the
# handler's types so every type gets the same number of papers; a paper that cannot take
# its error (p_digit on "< .001") is replaced by another draw.
planted_groups <- list(
  t_paired = list(handler = "t_paired", test = "paired",
                  make = function() paired_paper(paired_design())),
  t_independent_student = list(handler = "t_independent", test = "student",
                               make = function() independent_paper(TRUE)),
  t_independent_welch = list(handler = "t_independent", test = "welch",
                             make = function() independent_paper(FALSE)),
  t_one_sample = list(handler = "t_one_sample", test = "one_sample", make = one_sample_paper),
  cor_pearson = list(handler = "cor_pearson", test = "pearson", make = pearson_paper),
  anova_oneway = list(handler = "anova_oneway", test = "oneway", make = function() oneway_paper())
)

draw_cases <- function(group, n_papers) {
  types <- planted_types(group$handler)
  cases <- vector("list", n_papers)
  for (i in seq_len(n_papers)) {
    type <- types[(i - 1L) %% length(types) + 1L]
    for (attempt in 1:50) {
      paper <- group$make()
      planted <- if (is.null(paper)) NULL else plant_error(paper, group$handler, type)
      if (!is.null(planted)) break
    }
    if (!is.null(planted)) cases[[i]] <- list(type = type, paper = paper, planted = planted)
  }
  Filter(Negate(is.null), cases)
}

# The reported subsets the planted-error and statcheck scripts use.
# Keys the generators print for the cross-complete quantities (ADR 0031); the planted-error
# oracles below do not model them, so those sections leave them out and the dedicated section
# plants errors in them.
new_keys <- c("r2", "adj_r2", "partial_eta2", "partial_omega2")
report_subsets <- function(handler, paper) {
  paper <- paper[setdiff(names(paper), new_keys)]
  if (handler == "anova_oneway") {
    return(list(all = names(paper), `f/df/p` = c("f", "df1", "df2", "p"),
                `desc+f+p` = c("n", "m", "sd", "f", "p")))
  }
  desc <- switch(handler, t_paired = c("n", "m", "sd", "r", "t", "p"),
                 cor_pearson = c("n", "r", "t", "p"), c("n", "m", "sd", "t", "p"))
  subsets <- list(all = names(paper), `t/df/p` = c("t", "df", "p"), `desc+t+p` = desc)
  if (!is.null(paper$ci_lower)) {
    subsets[["desc+ci"]] <- c(setdiff(desc, c("t", "p")), "ci_lower", "ci_upper")
  }
  # The APA form r(df) = r, p = p: n is not reported, it is implied by df (n = df + 2).
  if (handler == "cor_pearson") subsets[["r/df/p"]] <- c("r", "df", "p")
  subsets
}

# check_reported() on the named numbers of a paper; an error is returned, not thrown.
run_check <- function(test, paper, keys) {
  mu <- attr(paper, "mu")
  args <- c(list(test = test), paper[keys], if (!is.null(mu)) list(mu = mu))
  tryCatch(suppressWarnings(do.call(check_reported, args)), error = function(e) e)
}

# Chi-squared (ADR 0016) -------------------------------------------------------------------
# chisq_independence_paper() draws an r x c table (2..4 by 2..5) of counts, with empty cells in
# the small-N draws, and prints chisq.test() output as a paper does: chi-squared 2 dp, df, p 3 dp
# or "< .001", N, V (or phi, signed, for a 2 x 2 table) 2 dp. A 2 x 2 table is analysed with or
# without Yates' correction (half each; V is always from the Pearson chi-squared, as in the
# literature). chisq_gof_paper() draws k = 2..8 counts against uniform or given H0 proportions
# and prints w, which is not bounded by 1: half the draws put nearly all the counts in one
# category (w up to sqrt(k - 1)). Both return the paper as a list and set the attributes
# `dims` and `p0` the checker needs; `handler` names the handler. A paper never has an empty
# row or column (chisq.test() would give NaN).
chisq_paper_attrs <- function(paper, handler, dims = NULL, p0 = NULL, truth = NULL) {
  attr(paper, "handler") <- handler
  attr(paper, "dims") <- dims
  attr(paper, "p0") <- p0
  attr(paper, "truth") <- truth
  paper
}

chisq_independence_paper <- function(force_2x2 = FALSE) {
  r <- if (force_2x2) 2L else sample(2:4, 1L)
  c_ <- if (force_2x2) 2L else sample(2:5, 1L)
  total <- sample(c(12, 20, 40, 100, 400, 3000), 1L)
  p <- rexp(r * c_)
  if (runif(1L) < 0.3) p[sample(r * c_, 1L)] <- 0
  tab <- matrix(rmultinom(1L, total, p / sum(p)), r, c_)
  if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) return(NULL)
  yates <- r == 2L && c_ == 2L && runif(1L) < 0.5
  test <- suppressWarnings(chisq.test(tab, correct = yates))
  pearson <- suppressWarnings(chisq.test(tab, correct = FALSE))$statistic
  v <- sqrt(pearson / (sum(tab) * (min(r, c_) - 1)))
  if (r == 2L && c_ == 2L && runif(1L) < 0.5) {
    phi <- (tab[1, 1] * tab[2, 2] - tab[1, 2] * tab[2, 1]) / sqrt(prod(rowSums(tab), colSums(tab)))
    v <- phi
    if (runif(1L) < 0.5) v <- -abs(v)
  }
  paper <- list(n = matrix(as.character(tab), r, c_), chisq = fmt(unname(test$statistic), 2L),
                df = as.character(r * c_ - r - c_ + 1L), p = fmt_p(test$p.value),
                n_total = as.character(sum(tab)),
                v = sub("^-0\\.", "-.", sub("^0\\.", ".", fmt(unname(v), 2L))))
  chisq_paper_attrs(paper, "chisq_independence", c(r, c_), NULL,
                    list(chisq = unname(test$statistic), p = test$p.value, yates = yates))
}

chisq_gof_paper <- function(high_w = runif(1L) < 0.5, k = sample(2:8, 1L)) {
  total <- sample(c(10, 25, 60, 200, 1000), 1L)
  given <- runif(1L) < 0.5
  p0 <- if (given) { q <- rexp(k) + 0.05; q / sum(q) } else rep(1 / k, k)
  prob <- if (high_w) c(0.97, rep(0.03 / (k - 1), k - 1))[sample(k)] else { q <- rexp(k); q / sum(q) }
  n <- as.vector(rmultinom(1L, total, prob))
  expected <- sum(n) * p0
  chisq <- sum((n - expected)^2 / expected)
  df <- k - 1L
  paper <- list(n = as.character(n), chisq = fmt(chisq, 2L), df = as.character(df),
                p = fmt_p(pchisq(chisq, df, lower.tail = FALSE)), n_total = as.character(sum(n)),
                v = sub("^0\\.", ".", fmt(sqrt(chisq / sum(n)), 2L)))
  if (sqrt(chisq / sum(n)) >= 1) paper$v <- fmt(sqrt(chisq / sum(n)), 2L)
  chisq_paper_attrs(paper, "chisq_gof", k, if (given || runif(1L) < 0.3) p0,
                    list(chisq = chisq, p = pchisq(chisq, df, lower.tail = FALSE),
                         w = sqrt(chisq / sum(n)), given = given))
}

# check_reported("chisq" / "gof") on the named numbers of a chi-squared paper.
run_chisq_check <- function(paper, keys, ...) {
  test <- c(chisq_independence = "chisq", chisq_gof = "gof")[[attr(paper, "handler")]]
  args <- c(list(test = test), paper[keys], list(dims = attr(paper, "dims")),
            if (test == "gof") list(p0 = attr(paper, "p0")), list(...))
  tryCatch(suppressWarnings(do.call(check_reported, args)), error = function(e) e)
}

chisq_subsets <- function(paper) {
  keys <- names(paper)
  list(keys, c("n", "chisq", "df", "p"), c("chisq", "df", "p"), c("n", "chisq"), c("n", "p"),
       c("n", "v"), c("chisq", "n_total", "v"), c("n", "chisq", "df", "v"),
       sample(keys, sample(2:length(keys), 1L)))
}

# Regression family (ADR 0020). lm_raw() fits a real lm() for one of the three lm rows (simple,
# multiple with 2..6 predictor columns and sometimes a factor, moderation X * Z with 0..2
# covariates), unweighted or weighted; lm_paper() prints what a paper would, rounded as printed:
# b, SE, t and the CI at 2 or 3 decimals (the CI at 90, 95 or 99%), p at 3 decimals or "< .001",
# R2 at 2, F at 2, n, df1 and df2 exact. Per-term quantities are vectors named by coefficient,
# which is what check_reported("lm") takes. The paper carries attributes: handler, k (predictor columns),
# adj (the adjusted R2 as printed), sd_all (SD of every non-intercept column, then the outcome).
lm_raw <- function(handler = sample(c("lm_simple", "lm_multiple", "lm_moderation"), 1L),
                   weighted = runif(1L) < 0.3, with_factor = runif(1L) < 0.4, k = NULL, n = NULL) {
  if (is.null(n)) n <- sample(c(20, 30, 50, 100, 300), 1L)
  common <- rnorm(n)
  col <- function() rnorm(n, sd = runif(1L, 0.5, 3)) + 0.5 * common * runif(1L)
  d <- data.frame(x1 = col())
  rhs <- "x1"
  if (handler == "lm_multiple") {
    nf <- 0L
    if (with_factor) {
      d$f <- factor(sample(letters[1:sample(2:4, 1L)], n, replace = TRUE))
      nf <- nlevels(d$f) - 1L
    }
    target <- if (is.null(k)) sample(2:6, 1L) else k
    nn <- max(target - nf, if (nf == 0L) 2L else 1L)
    for (j in seq_len(nn)[-1L]) d[[paste0("x", j)]] <- col()
    rhs <- paste(setdiff(names(d), "y"), collapse = " + ")
  } else if (handler == "lm_moderation") {
    d$x2 <- col()
    cov <- sample(0:2, 1L)
    for (j in seq_len(cov)) d[[paste0("z", j)]] <- col()
    rhs <- paste(c("x1 * x2", names(d)[-(1:2)]), collapse = " + ")
  }
  num <- vapply(d, is.numeric, logical(1))
  mm <- stats::model.matrix(~ ., d)[, -1L, drop = FALSE]
  beta <- rnorm(ncol(mm)) * sample(c(0, 0.3, 1), ncol(mm), replace = TRUE, prob = c(0.2, 0.4, 0.4))
  d$y <- drop(mm %*% beta) + rnorm(n, sd = runif(1L, 0.5, 5)) * (1 + sum(abs(beta)))
  d$w <- if (weighted) runif(n, 0.2, 5) else NULL
  fit <- if (is.null(d$w)) stats::lm(stats::as.formula(paste("y ~", rhs)), data = d) else
    stats::lm(stats::as.formula(paste("y ~", rhs)), data = d, weights = w)  # nolint
  stopifnot(identical(find_handler(fit)$name, handler))
  list(fit = fit, handler = handler, data = d)
}

lm_paper <- function(raw, intercept = runif(1L) < 0.3) {
  fit <- raw$fit
  sm <- summary(fit)
  cf <- sm$coefficients
  terms <- rownames(cf)
  if (!intercept) terms <- setdiff(terms, "(Intercept)")
  dec <- sample(c(2L, 3L), 1L)
  level <- draw_level()
  ci <- stats::confint(fit, level = level)
  named <- function(x, d) stats::setNames(fmt(x[terms], d), terms)
  k <- length(stats::coef(fit)) - 1L
  fst <- sm$fstatistic
  mm <- stats::model.matrix(fit)[, -1L, drop = FALSE]
  w <- stats::weights(fit)
  wsd <- function(x) if (is.null(w)) stats::sd(x) else
    sqrt(sum(w * (x - sum(w * x) / sum(w))^2) / (sum(w) - 1))
  paper <- list(
    b = named(cf[, 1L], dec), se = named(cf[, 2L], dec), t = named(cf[, 3L], 2L),
    p = stats::setNames(vapply(cf[terms, 4L], fmt_p, ""), terms),
    ci_lower = named(ci[, 1L], dec), ci_upper = named(ci[, 2L], dec),
    r2 = fmt(sm$r.squared, 2L), f = fmt(unname(fst[["value"]]), 2L),
    df1 = as.character(fst[["numdf"]]), df2 = as.character(fst[["dendf"]]),
    p_model = fmt_p(stats::pf(fst[["value"]], fst[["numdf"]], fst[["dendf"]], lower.tail = FALSE)),
    adj_r2 = fmt(sm$adj.r.squared, 2L), n = as.character(stats::nobs(fit)))
  paper$ci_upper <- paper$ci_upper
  attr(paper, "handler") <- raw$handler
  attr(paper, "k") <- k
  attr(paper, "level") <- level
  attr(paper, "adj") <- fmt(sm$adj.r.squared, 2L)
  attr(paper, "sd_all") <- c(stats::setNames(fmt(apply(mm, 2L, wsd)[setdiff(terms, "(Intercept)")], 2L),
                                              setdiff(terms, "(Intercept)")),
                             stats::setNames(fmt(wsd(stats::model.response(stats::model.frame(fit))), 2L),
                                             "(response)"))
  if (raw$handler == "lm_simple") {
    # the (weighted) correlation: the slope of a weighted fit is cov_w / var_w, R2 is r_w^2
    yv <- stats::model.response(stats::model.frame(fit))
    ww <- if (is.null(w)) rep(1, length(yv)) else w
    cw <- function(u, v) sum(ww * (u - sum(ww * u) / sum(ww)) * (v - sum(ww * v) / sum(ww)))
    attr(paper, "r") <- fmt(cw(mm[, 1L], yv) / sqrt(cw(mm[, 1L], mm[, 1L]) * cw(yv, yv)), 2L)
  }
  paper
}

# check_reported("lm" / "lm_simple" / "lm_moderation") on the named numbers of a regression paper, with keys as in the paper's names.
# `terms_only` keeps only these terms in the per-term vectors (a subset of the table).
run_lm_check <- function(paper, keys, terms_only = NULL, k = NULL, ...) {
  per_term <- c("b", "se", "t", "p", "ci_lower", "ci_upper", "beta")
  args <- list()
  for (key in keys) {
    v <- if (key == "sd") attr(paper, "sd_all") else if (key == "r2_adj") NULL else paper[[key]]
    if (key %in% per_term && !is.null(terms_only)) v <- v[intersect(names(v), terms_only)]
    if (key == "r2_adj") { args[["r2"]] <- attr(paper, "adj"); next }
    if (key == "beta") v <- paper$b
    if (!is.null(v) && length(v) > 0L) args[[key]] <- v
  }
  if (attr(paper, "handler") == "lm_simple") {
    k <- NULL  # a simple regression fixes k = 1 and takes no k argument
    # a simple regression's single slope is the one unnamed value; the intercept is not kept
    for (key in intersect(names(args), per_term)) args[[key]] <- args[[key]][setdiff(names(args[[key]]), "(Intercept)")]
    if ("r" %in% keys) args[["r"]] <- attr(paper, "r")
  }
  test <- c(lm_simple = "lm_simple", lm_multiple = "lm", lm_moderation = "lm_moderation")[[
    attr(paper, "handler")]]
  tryCatch(suppressWarnings(do.call(check_reported, c(list(test = test), args, list(k = k), list(...)))),
           error = function(e) e)
}

# Simple mediation (X -> M -> Y): three real lm() fits on raw data, the four paths printed as a
# paper does (b, SE, t at 2 decimals, p at 3, df exact, the CI b -/+ t_crit SE at a level drawn
# from 90/95/99 per cent, 2 decimals), and the indirect effect a*b with a
# percentile bootstrap CI (200 resamples, drawn here), which the checker must never judge.
mediation_raw <- function(size = sample(c("small", "large", "mixed"), 1L)) {
  n <- sample(c(30, 50, 100, 300), 1L)
  x <- rnorm(n, sd = runif(1L, 0.5, 3))
  a <- switch(size, small = 0.1, large = 1.5, mixed = 0.6) * sample(c(-1, 1), 1L)
  b <- switch(size, small = 0.1, large = 1.5, mixed = 1.2) * sample(c(-1, 1), 1L)
  m <- a * x + rnorm(n, sd = runif(1L, 0.5, 2))
  y <- 0.3 * x + b * m + rnorm(n, sd = runif(1L, 0.5, 2))
  data.frame(x = x, m = m, y = y)
}

mediation_paper <- function(d) {
  fa <- summary(stats::lm(m ~ x, d))
  fc <- summary(stats::lm(y ~ x, d))
  fb <- summary(stats::lm(y ~ x + m, d))
  row <- function(sm, term) sm$coefficients[term, ]
  paths <- list(a = row(fa, "x"), b = row(fb, "m"), c = row(fc, "x"), c_prime = row(fb, "x"))
  n <- nrow(d)
  level <- draw_level()
  dfs <- c(a = n - 2L, b = n - 3L, c = n - 2L, c_prime = n - 3L)
  half <- vapply(names(paths), function(nm) stats::qt(1 - (1 - level) / 2, dfs[[nm]]) * paths[[nm]][[2L]], 0)
  # 200 resamples of a*b with the closed-form slopes of the two regressions (a: m on x; b: the m
  # coefficient of y on x and m), so the loop fits no models. The sample() calls are those of
  # the former lm.fit loop: same draws, same printed paper.
  boot <- vapply(seq_len(200L), function(i) {
    idx <- sample(n, replace = TRUE)
    xc <- d$x[idx] - mean(d$x[idx])
    mc <- d$m[idx] - mean(d$m[idx])
    yc <- d$y[idx] - mean(d$y[idx])
    sxx <- sum(xc * xc)
    sxm <- sum(xc * mc)
    sxy <- sum(xc * yc)
    smm <- sum(mc * mc)
    smy <- sum(mc * yc)
    (sxm / sxx) * ((sxx * smy - sxm * sxy) / (sxx * smm - sxm * sxm))
  }, numeric(1))
  q <- stats::quantile(boot, c(0.025, 0.975))
  get <- function(i, d) stats::setNames(vapply(paths, function(p) fmt(p[[i]], d), ""), names(paths))
  paper <- list(
    b = get(1L, 2L), se = get(2L, 2L), t = get(3L, 2L),
    p = stats::setNames(vapply(paths, function(p) fmt_p(p[[4L]]), ""), names(paths)),
    ci_lower = stats::setNames(fmt(vapply(paths, `[[`, 0, 1L) - half, 2L), names(paths)),
    ci_upper = stats::setNames(fmt(vapply(paths, `[[`, 0, 1L) + half, 2L), names(paths)),
    df = c(a = as.character(n - 2L), b = as.character(n - 3L), c = as.character(n - 2L),
           c_prime = as.character(n - 3L)),
    n = as.character(n), indirect = fmt(paths$a[[1L]] * paths$b[[1L]], 2L),
    indirect_ci_lower = fmt(q[[1L]], 2L), indirect_ci_upper = fmt(q[[2L]], 2L))
  # the model rows PROCESS prints: R squared and F(df1, df2) of the three regressions, by path
  models <- list(a = fa, b = fb, c = fc)
  paper$r2 <- stats::setNames(vapply(models, function(m) fmt(m$r.squared, 2L), ""), names(models))
  paper$f <- stats::setNames(vapply(models, function(m) fmt(m$fstatistic[["value"]], 2L), ""), names(models))
  paper$df1 <- stats::setNames(vapply(models, function(m) as.character(m$fstatistic[["numdf"]]), ""), names(models))
  paper$df2 <- stats::setNames(vapply(models, function(m) as.character(m$fstatistic[["dendf"]]), ""), names(models))
  attr(paper, "level") <- level
  # The other way a paper gets its indirect effect: the product of the printed paths,
  # round(round(a) * round(b)).
  attr(paper, "indirect_rounded_product") <-
    fmt(as.numeric(fmt(paths$a[[1L]], 2L)) * as.numeric(fmt(paths$b[[1L]], 2L)), 2L)
  paper
}

run_mediation_check <- function(paper, keys, paths = NULL) {
  args <- list()
  for (key in keys) {
    v <- paper[[key]]
    if (key %in% c("b", "se", "t", "p", "df", "ci_lower", "ci_upper") && !is.null(paths)) v <- v[intersect(names(v), paths)]
    if (length(v) > 0L) args[[key]] <- v
  }
  tryCatch(suppressWarnings(do.call(check_reported, c(list(test = "mediation"), args))), error = function(e) e)
}

# TRUE when no indirect-effect number came out "not coherent" and the bootstrap CI was left
# "not checkable" (ADR 0028: the indirect is checked against a*b, its CI cannot be checked).
indirect_untouched <- function(res) {
  rows <- res$numbers[grepl("^indirect", res$numbers$quantity), ]
  ci <- grepl("^indirect_ci", rows$quantity)
  all(rows$status[ci] == "not checkable") && all(rows$status[!ci] %in% c("coherent", "not checkable"))
}
