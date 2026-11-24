# Shared generators of correctly reported papers for the check_reported() validation scripts
# (check-false-positives.R, check-planted-errors.R, check-statcheck.R). Each generator draws raw
# data, runs the real t.test() / cor.test(), and returns the numbers rounded as a paper prints
# them (2 decimals for t, 3 for p and r, "< .001" below .001). Sourced, not run: it draws nothing
# until a generator is called, so the caller owns the seed.
# One-way ANOVA: oneway_raw() draws k = 2..6 groups, balanced or not, and
# oneway_paper() prints classical oneway.test(var.equal = TRUE) output as a paper does
# (F 2 dp, p 3 dp or "< .001", M and SD 2 dp, eta2 or omega2 2 dp).

fmt <- function(x, d) formatC(x, format = "f", digits = d)
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

paired_paper <- function(d) {
  level <- draw_level()
  tt <- t.test(d$x, d$y, paired = TRUE, conf.level = level)
  r <- cor(d$x, d$y)
  if (!is.finite(r) || !is.finite(tt$statistic)) return(NULL)
  dec <- d$dec
  n <- length(d$x)
  paper <- list(n = as.character(n), m = c(fmt(mean(d$x), dec), fmt(mean(d$y), dec)),
                sd = c(fmt(sd(d$x), dec), fmt(sd(d$y), dec)), r = fmt(r, 3L),
                t = fmt(unname(tt$statistic), 2L), df = as.character(n - 1L),
                p = fmt_p(tt$p.value), ci_lower = fmt(tt$conf.int[1L], dec),
                ci_upper = fmt(tt$conf.int[2L], dec))
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
  paper
}

pearson_paper <- function() {
  n <- sample(sizes, 1L)
  rho <- if (runif(1L) < 0.5) sample(rhos, 1L) else runif(1L, -0.99, 0.99)
  x <- rnorm(n)
  y <- rho * x + sqrt(1 - rho^2) * rnorm(n)
  r <- suppressWarnings(cor(x, y))
  if (!is.finite(r)) return(NULL)
  level <- draw_level()
  ct <- cor.test(x, y, conf.level = level)
  dec <- sample(2:3, 1L)
  paper <- list(n = as.character(n), r = fmt(unname(ct$estimate), dec),
                df = as.character(n - 2L), t = fmt(unname(ct$statistic), 2L),
                p = fmt_p(ct$p.value))
  if (n > 3L) {
    paper <- c(paper, list(ci_lower = fmt(ct$conf.int[1L], dec),
                           ci_upper = fmt(ct$conf.int[2L], dec)))
  }
  attr(paper, "level") <- level
  paper
}

independent_paper <- function(var_equal) {
  n1 <- sample(sizes[sizes >= 3], 1L)
  n2 <- sample(sizes[sizes >= 3], 1L)
  scale <- 10^runif(1L, -1, 2)
  x <- rnorm(n1) * scale * runif(1L, 0.3, 3) + runif(1L, -1, 1) * scale
  y <- rnorm(n2) * scale * runif(1L, 0.3, 3) + runif(1L, -1, 1) * scale
  level <- draw_level()
  tt <- t.test(x, y, var.equal = var_equal, conf.level = level)
  if (!is.finite(tt$statistic) || sd(x) == 0 || sd(y) == 0) return(NULL)
  dec <- sample(c(0L, 2L, 3L), 1L, prob = c(0.3, 0.4, 0.3))
  df_dec <- if (var_equal) 0L else sample(1:2, 1L)
  pooled <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  paper <- list(n = as.character(c(n1, n2)), m = c(fmt(mean(x), dec), fmt(mean(y), dec)),
                sd = c(fmt(sd(x), dec), fmt(sd(y), dec)),
                t = fmt(unname(tt$statistic), 2L), df = fmt(unname(tt$parameter), df_dec),
                p = fmt_p(tt$p.value), ci_lower = fmt(tt$conf.int[1L], dec),
                ci_upper = fmt(tt$conf.int[2L], dec))
  if (runif(1L) < 0.3) paper$d <- fmt((mean(x) - mean(y)) / pooled, 2L)
  attr(paper, "level") <- level
  paper
}

one_sample_paper <- function() {
  n <- sample(sizes, 1L)
  scale <- 10^runif(1L, -1, 2)
  x <- rnorm(n) * scale + runif(1L, -1, 1) * scale
  mu <- if (runif(1L) < 0.4) 0 else round(runif(1L, -1, 1) * scale, sample(0:2, 1L))
  level <- draw_level()
  tt <- t.test(x, mu = mu, conf.level = level)
  if (!is.finite(tt$statistic) || sd(x) == 0) return(NULL)
  dec <- sample(c(0L, 2L, 3L), 1L, prob = c(0.3, 0.4, 0.3))
  paper <- list(n = as.character(n), m = fmt(mean(x), dec), sd = fmt(sd(x), dec),
                t = fmt(unname(tt$statistic), 2L), df = as.character(n - 1L),
                p = fmt_p(tt$p.value), ci_lower = fmt(tt$conf.int[1L], dec),
                ci_upper = fmt(tt$conf.int[2L], dec))
  attr(paper, "mu") <- mu
  attr(paper, "level") <- level
  paper
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
       p = fmt_p(truth$p), eta2 = fmt(effect, 2L))
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
                eta2 = fmt(size, 2L))
  attr(paper, "effect") <- effect
  attr(paper, "dims") <- raw$dims
  attr(paper, "balanced") <- raw$balanced
  attr(paper, "type") <- type
  paper
}

# check_F() on the named numbers of a factorial paper; an error is returned, not thrown.
run_factorial_check <- function(paper, keys) {
  args <- c(paper[keys], list(effect = attr(paper, "effect"), dims = attr(paper, "dims")))
  tryCatch(suppressWarnings(do.call(check_F, args)), error = function(e) e)
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
    return(c("f_digit", "p_digit", "df_wrong_n", "decision", "m_change"))
  }
  c("t_digit", "p_digit", "df_wrong_n", "decision",
    if (handler == "cor_pearson") "r_digit" else "m_change", "ci_change")
}

plant_error <- function(paper, handler, type) {
  if (handler == "anova_oneway" && type %in% c("f_digit", "df_wrong_n")) {
    return(if (type == "f_digit") plant_f_digit(paper) else plant_oneway_df(paper))
  }
  switch(type, t_digit = plant_t_digit(paper), p_digit = plant_p_digit(paper),
         df_wrong_n = plant_df_wrong_n(paper, handler), decision = plant_decision(paper),
         r_digit = plant_r_digit(paper), m_change = plant_m_change(paper),
         ci_change = plant_ci_change(paper))
}

# Groups of correct papers, one per handler (independent t split by test), and a driver
# that draws papers with one planted error each. The error type rotates through the
# handler's types so every type gets the same number of papers; a paper that cannot take
# its error (p_digit on "< .001") is replaced by another draw.
planted_groups <- list(
  t_paired = list(handler = "t_paired", make = function() paired_paper(paired_design())),
  t_independent_student = list(handler = "t_independent",
                               make = function() independent_paper(TRUE)),
  t_independent_welch = list(handler = "t_independent",
                             make = function() independent_paper(FALSE)),
  t_one_sample = list(handler = "t_one_sample", make = one_sample_paper),
  cor_pearson = list(handler = "cor_pearson", make = pearson_paper),
  anova_oneway = list(handler = "anova_oneway", make = function() oneway_paper())
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
report_subsets <- function(handler, paper) {
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
run_check <- function(handler, paper, keys) {
  mu <- attr(paper, "mu")
  args <- c(list(handler), paper[keys], if (!is.null(mu)) list(mu = mu))
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

# check_chisq() on the named numbers of a chi-squared paper.
run_chisq_check <- function(paper, keys, ...) {
  args <- c(paper[keys], list(handler = attr(paper, "handler"), dims = attr(paper, "dims"),
                              p0 = attr(paper, "p0")), list(...))
  tryCatch(suppressWarnings(do.call(check_chisq, args)), error = function(e) e)
}

chisq_subsets <- function(paper) {
  keys <- names(paper)
  list(keys, c("n", "chisq", "df", "p"), c("chisq", "df", "p"), c("n", "chisq"), c("n", "p"),
       c("n", "v"), c("chisq", "n_total", "v"), c("n", "chisq", "df", "v"),
       sample(keys, sample(2:length(keys), 1L)))
}
