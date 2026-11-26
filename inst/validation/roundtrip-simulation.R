# Round-trip simulation: for each exact handler, random designs ->
# sufficient_stats() -> reprokit:::reproduce(), compared two ways:
#   (1) with the handler's own reported() on the raw data (what the package says the analysis
#       gives; the oracle is reprokit itself), and
#   (2) with an independent base-R computation on the raw data, written in this script and not
#       using reprokit: t.test(), cor.test(), chisq.test(), summary(aov()), anova() and drop1()
#       for the factorial SS types I, II and III, summary(lm()) and confint() for the regression
#       family, and three lm() fits for the mediation paths. The numbers compared are the ones the
#       handler reports (t, df, p, mean difference, CI bounds, r, chi-squared, V, F, eta2,
#       estimates, SEs, t, p, sigma, R2, adjusted R2, F, the lm confint bounds, the paths and a*b).
# PASS criterion, worst over all designs and all numbers, for both comparisons:
#   scaled difference |original - reproduced| / max(1, |original|) < 1e-8
#   (= absolute for numbers up to 1, relative for larger ones such as a big F).
# The worst absolute difference is shown for information only. "compared" is the smallest
# number of values compared against base R in any design of the row (it must be above 0).
# The script ends with an overall PASS or FAIL line and exits with status 1 on FAIL under Rscript.
# Chi-squared (ADR 0016): independence tables 2 x 2 to 4 x 5 (Yates or Pearson in a 2 x 2) and
# goodness of fit with k = 2..8 (uniform or given p0), both with empty cells.
# Factorial ANOVA (ADR 0017): 2 x 2 to 3 x 4 designs, balanced (types agree) and unbalanced with
# each SS type (III, II, I), unequal SDs and cell means, against anova()/drop1() on the raw data.
# Regression family (ADR 0020): lm_simple (k = 1), lm_multiple (k = 2..6 predictor columns, a factor
# in half the designs, weights in half), lm_moderation (X * Z with 0..2 covariates, weights in half)
# and mediation_simple (small paths, large paths, and a mediator that is nearly X); every design
# is also checked to reach the intended handler, against summary(lm) on the raw data (for mediation,
# against the paths of the three lm() fits through the handler's own reported()).
# Result route (ADR 0026): t, cor, chi-squared and aov results, and two-model mediation (ADR 0028),
# shared from the fitted objects and round-tripped the same way.
# Run: Rscript inst/validation/roundtrip-simulation.R   (after devtools::load_all())

source("inst/validation/record.R")
source("inst/validation/check-designs.R")  # factorial_truth(): anova()/drop1() on the raw data
record_start("roundtrip-simulation", seed = 20261122)

n_designs <- 200L
tolerance <- 1e-8
set.seed(20261122)

worst_diff <- function(original, reproduced) {
  stopifnot(identical(names(original), names(reproduced)))
  d <- ifelse(original == reproduced, 0, abs(original - reproduced))
  c(absolute = max(d), scaled = max(d / pmax(1, abs(original))))
}

# Independent base-R reference values on the raw data, named like h$reported() (a subset of them;
# only the names present in both are compared). Nothing here calls reprokit.
reference_of <- function(name, ex) {
  a <- ex$args
  d <- a$data
  one <- function(x) unlist(x)
  switch(name,
    t_independent = {
      g <- levels(factor(d$g))
      tt <- stats::t.test(d$y[d$g == g[1L]], d$y[d$g == g[2L]], var.equal = a$test == "student",
                          alternative = a$alternative, conf.level = a$conf_level)
      c(t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
        mean_diff = unname(tt$estimate[1L] - tt$estimate[2L]), ci_lower = tt$conf.int[[1L]],
        ci_upper = tt$conf.int[[2L]])
    },
    t_one_sample = {
      tt <- stats::t.test(d$y, mu = a$mu, alternative = a$alternative, conf.level = a$conf_level)
      c(t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
        mean = unname(tt$estimate), ci_lower = tt$conf.int[[1L]], ci_upper = tt$conf.int[[2L]])
    },
    t_paired = {
      tt <- stats::t.test(d$pre, d$post, paired = TRUE, mu = a$mu, alternative = a$alternative,
                          conf.level = a$conf_level)
      c(t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
        mean_diff = mean(d$pre - d$post), ci_lower = tt$conf.int[[1L]], ci_upper = tt$conf.int[[2L]])
    },
    cor_pearson = {
      ct <- stats::cor.test(d$a, d$b, alternative = a$alternative, conf.level = a$conf_level)
      c(r = unname(ct$estimate), t = unname(ct$statistic), df = unname(ct$parameter),
        p = ct$p.value, ci_lower = ct$conf.int[[1L]], ci_upper = ct$conf.int[[2L]])
    },
    chisq_independence = {
      tab <- table(d$a, d$b)
      ct <- suppressWarnings(stats::chisq.test(tab, correct = a$correct))
      pearson <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic
      c(chisq = unname(ct$statistic), df = unname(ct$parameter), p = ct$p.value, n_total = sum(tab),
        v = unname(sqrt(pearson / (sum(tab) * (min(dim(tab)) - 1)))))
    },
    chisq_gof = {
      tab <- table(d$a)
      p0 <- if (is.null(a$p0)) rep(1 / length(tab), length(tab)) else a$p0
      ct <- suppressWarnings(stats::chisq.test(tab, p = p0))
      c(chisq = unname(ct$statistic), df = unname(ct$parameter), p = ct$p.value, n_total = sum(tab),
        v = unname(sqrt(ct$statistic / sum(tab))))
    },
    anova_oneway = {
      fit <- summary(stats::aov(y ~ g, data = d))[[1L]]
      c(f = fit[["F value"]][1L], df1 = fit[["Df"]][1L], df2 = fit[["Df"]][2L],
        p = fit[["Pr(>F)"]][1L], eta2 = fit[["Sum Sq"]][1L] / sum(fit[["Sum Sq"]]))
    },
    anova_factorial = {
      truth <- factorial_truth(d, a$ss_type)
      ss_res <- stats::deviance(stats::lm(y ~ a * b, d))
      out <- c(unlist(lapply(c("A", "B", "AB"), function(e) {
        stats::setNames(c(truth$f[[e]], truth$df1[[e]], truth$p[[e]], truth$eta2[[e]]),
                        paste0(c("f_", "df1_", "p_", "eta2_"), e))
      })), df2 = truth$df2)
      out
    },
    mediation_simple = {
      fa <- summary(stats::lm(m ~ x, d))$coefficients["x", ]
      fb <- summary(stats::lm(y ~ x + m, d))$coefficients
      fc <- summary(stats::lm(y ~ x, d))$coefficients["x", ]
      n <- nrow(d)
      path <- function(row, df, nm) stats::setNames(c(row[1:2], row[3], row[4], df),
        paste0(c("estimate:", "se:", "t:", "p:", "df:"), nm))
      c(path(fa, n - 2L, "a"), path(fb["m", ], n - 3L, "b"), path(fc, n - 2L, "c"),
        path(fb["x", ], n - 3L, "c_prime"), indirect = unname(fa[1L] * fb["m", 1L]))
    },
    # the lm family: ex$x is the fit; summary() and confint() of base R on the raw data
    {
      fit <- ex$x
      sm <- stats::summary.lm(fit)
      cf <- sm$coefficients
      out <- c(stats::setNames(cf[, 1L], paste0("estimate:", rownames(cf))),
               stats::setNames(cf[, 2L], paste0("se:", rownames(cf))),
               stats::setNames(cf[, 3L], paste0("t:", rownames(cf))),
               stats::setNames(cf[, 4L], paste0("p:", rownames(cf))),
               sigma = sm$sigma, r.squared = sm$r.squared, adj.r.squared = sm$adj.r.squared)
      if (!is.null(sm$fstatistic)) {
        out <- c(out, F = unname(sm$fstatistic[["value"]]), numdf = unname(sm$fstatistic[["numdf"]]),
                 dendf = unname(sm$fstatistic[["dendf"]]))
      }
      out
    })
}

# The lm confint() bounds from base R against estimate -/+ qt * se rebuilt from the reproduced
# estimate, SE and residual df (the CI is not among reported(); this checks the quantities a CI
# is made of at the 95% level). Returns the reproduced bounds named like confint(fit)'s.
lm_confint_pair <- function(fit, repro) {
  ci <- stats::confint(fit, level = 0.95)
  terms <- rownames(ci)
  est <- repro[paste0("estimate:", terms)]
  se <- repro[paste0("se:", terms)]
  q <- stats::qt(0.975, stats::df.residual(fit))
  list(ref = c(stats::setNames(ci[, 1L], paste0("lower:", terms)),
               stats::setNames(ci[, 2L], paste0("upper:", terms))),
       repro = c(stats::setNames(est - q * se, paste0("lower:", terms)),
                 stats::setNames(est + q * se, paste0("upper:", terms))))
}

# Both comparisons for one design: returns c(absolute, scaled) against reported(), c(absolute,
# scaled) against base R, and the number of values compared with base R.
compare_both <- function(name_base, h, ex, s) {
  repro <- reprokit:::reproduce(s)
  ours <- worst_diff(do.call(h$reported, c(list(ex$x), ex$args)), repro)
  ref <- reference_of(name_base, ex)
  common <- intersect(names(ref), names(repro))
  stopifnot(length(common) > 0L, !anyNA(ref[common]), !anyNA(repro[common]))
  base <- worst_diff(ref[common], repro[common])
  n_compared <- length(common)
  if (inherits(ex$x, "lm")) {
    ci <- lm_confint_pair(ex$x, repro)
    base_ci <- worst_diff(ci$ref, ci$repro)
    base <- pmax(base, base_ci)
    n_compared <- n_compared + length(ci$ref)
  }
  c(ours, base_absolute = base[["absolute"]], base_scaled = base[["scaled"]], compared = n_compared)
}

# One table row from the per-design results (5 x n matrix from vapply).
row_of <- function(label, diffs) {
  worst <- apply(diffs[1:4, , drop = FALSE], 1L, max)
  data.frame(handler = label, designs = ncol(diffs),
             worst_absolute = signif(worst[["absolute"]], 3),
             worst_scaled = signif(worst[["scaled"]], 3),
             base_R_worst_scaled = signif(worst[["base_scaled"]], 3),
             compared = min(diffs["compared", ]),
             result = if (max(worst[["scaled"]], worst[["base_scaled"]]) < tolerance &&
                          min(diffs["compared", ]) > 0) "PASS" else "FAIL")
}

# One random t-test design: unbalanced groups, unequal SDs, random shift.
t_design <- function() {
  n <- sample(5:60, 2L, replace = TRUE)
  d <- data.frame(
    y = c(rnorm(n[1], 0, runif(1, 0.5, 3)), rnorm(n[2], runif(1, -2, 2), runif(1, 0.5, 3))),
    g = rep(c("a", "b"), n)
  )
  args <- list(data = d, test = sample(c("student", "welch"), 1L),
               alternative = sample(c("two.sided", "less", "greater"), 1L),
               conf_level = sample(c(0.9, 0.95, 0.99), 1L))
  list(x = y ~ g, args = args)
}

# One random lm design: 0-4 numeric predictors, 0-1 factor with 2-4 levels,
# optional interaction, optional no-intercept, optional weights (sometimes with
# zeros) and optional offset. Large-ish cells so disclosure passes.
lm_design <- function() {
  n <- sample(40:300, 1L)
  k_num <- sample(0:4, 1L)
  d <- as.data.frame(matrix(rnorm(n * max(k_num, 1L), sd = runif(1, 0.1, 10)), n))
  names(d) <- paste0("x", seq_len(ncol(d)))
  d <- d[seq_len(k_num)]
  terms <- names(d)
  if (runif(1) < 0.6) {
    d$f <- factor(sample(letters[1:sample(2:4, 1L)], n, replace = TRUE))
    terms <- c(terms, "f")
  }
  if (length(terms) >= 2L && runif(1) < 0.3) terms <- c(terms, paste(terms[1:2], collapse = ":"))
  d$y <- 3 + Reduce(`+`, lapply(names(d), function(v) {
    if (is.numeric(d[[v]])) rnorm(1) * d[[v]] else rnorm(1) * as.integer(d[[v]])
  }), 0) + rnorm(n, sd = runif(1, 0.5, 5))
  rhs <- if (length(terms)) paste(terms, collapse = " + ") else "1"
  if (length(terms) && runif(1) < 0.2) rhs <- paste("0 +", rhs)
  if (runif(1) < 0.4) {
    d$off <- rnorm(n, sd = 2)
    rhs <- paste(rhs, "+ offset(off)")
  }
  d$w <- if (runif(1) < 0.4) runif(n, 0.1, 5) else NULL
  if (!is.null(d$w) && runif(1) < 0.3) d$w[sample(n, 3L)] <- 0
  fit <- if (is.null(d$w)) {
    stats::lm(stats::as.formula(paste("y ~", rhs)), data = d)
  } else {
    # lm() finds w in d; lintr cannot see that.
    stats::lm(stats::as.formula(paste("y ~", rhs)), data = d, weights = w)  # nolint
  }
  list(x = fit, args = list())
}

# Regression family designs. Each draws n = 40..300, a response and the predictors, optionally
# weights; the fit must be routed to the named handler (stopifnot).
reg_data <- function(n, k_num, with_factor) {
  d <- as.data.frame(matrix(rnorm(n * k_num, sd = runif(1, 0.1, 10)), n))
  names(d) <- paste0("x", seq_len(k_num))
  if (with_factor) d$f <- factor(sample(letters[1:sample(2:4, 1L)], n, replace = TRUE))
  d
}
reg_fit <- function(rhs, d, weighted, handler) {
  d$y <- 2 + Reduce(`+`, lapply(setdiff(names(d), "y"), function(v) {
    if (is.numeric(d[[v]])) rnorm(1) * d[[v]] else rnorm(1) * as.integer(d[[v]])
  }), 0) + rnorm(nrow(d), sd = runif(1, 0.5, 5))
  d$w <- if (weighted) runif(nrow(d), 0.1, 5) else NULL
  fit <- if (is.null(d$w)) {
    stats::lm(stats::as.formula(paste("y ~", rhs)), data = d)
  } else {
    stats::lm(stats::as.formula(paste("y ~", rhs)), data = d, weights = w)  # nolint
  }
  stopifnot(identical(find_handler(fit)$name, handler))
  list(x = fit, args = list())
}
lm_simple_design <- function() {
  reg_fit("x1", reg_data(sample(40:300, 1L), 1L, FALSE), runif(1) < 0.5, "lm_simple")
}
lm_multiple_design <- function() {
  k <- sample(2:6, 1L)
  with_factor <- runif(1) < 0.5 && k >= 3L
  d <- reg_data(sample(40:300, 1L), if (with_factor) k - 1L else k, with_factor)
  # a factor with m levels adds m - 1 columns, so k counts columns loosely; k >= 2 holds
  reg_fit(paste(setdiff(names(d), "y"), collapse = " + "), d, runif(1) < 0.5, "lm_multiple")
}
lm_moderation_design <- function() {
  d <- reg_data(sample(40:300, 1L), 2L + sample(0:2, 1L), FALSE)
  extra <- setdiff(names(d), c("x1", "x2"))
  rhs <- paste(c("x1 * x2", extra), collapse = " + ")
  reg_fit(rhs, d, runif(1) < 0.5, "lm_moderation")
}
mediation_design <- function(size) {
  n <- sample(40:300, 1L)
  x <- rnorm(n)
  a <- switch(size, small = 0.1, large = 3, near = 1)
  m <- a * x + rnorm(n, sd = if (size == "near") 0.01 else 1)
  b <- switch(size, small = 0.1, large = 3, near = 0.5)
  y <- 0.3 * x + b * m + rnorm(n)
  list(x = y ~ x, args = list(data = data.frame(x = x, m = m, y = y), test = "mediation",
                              mediator = "m"))
}

random_alternative <- function() sample(c("two.sided", "less", "greater"), 1L)
random_conf_level <- function() sample(c(0.9, 0.95, 0.99), 1L)

# One random one-sample design: random n, location, spread and test value.
t_one_sample_design <- function() {
  n <- sample(10:100, 1L)
  d <- data.frame(y = rnorm(n, runif(1, -2, 2), runif(1, 0.5, 3)))
  list(x = y ~ 1, args = list(data = d, test = "one_sample", mu = runif(1, -1, 1),
                              alternative = random_alternative(),
                              conf_level = random_conf_level()))
}

# One random paired design: correlated pre/post scores, random shift and test value.
t_paired_design <- function() {
  n <- sample(10:100, 1L)
  pre <- rnorm(n, 0, runif(1, 0.5, 3))
  d <- data.frame(pre = pre, post = runif(1, -1, 1) * pre + rnorm(n, runif(1, -2, 2)))
  list(x = Pair(pre, post) ~ 1,
       args = list(data = d, test = "paired", mu = runif(1, -1, 1),
                   alternative = random_alternative(), conf_level = random_conf_level()))
}

# One random correlation design: true r anywhere in (-0.95, 0.95).
cor_pearson_design <- function() {
  n <- sample(10:300, 1L)
  rho <- runif(1, -0.95, 0.95)
  a <- rnorm(n)
  d <- data.frame(a = a * runif(1, 0.1, 10), b = rho * a + sqrt(1 - rho^2) * rnorm(n))
  list(x = ~ a + b, args = list(data = d, test = "pearson", alternative = random_alternative(),
                                conf_level = random_conf_level()))
}

# One random one-way design: k = 2..6 groups, balanced or unbalanced, unequal SDs and means.
oneway_design <- function(balanced) {
  k <- sample(2:6, 1L)
  n <- if (balanced) rep(sample(5:60, 1L), k) else sample(5:60, k, replace = TRUE)
  d <- data.frame(y = unlist(lapply(n, function(m) rnorm(m, runif(1, -2, 2), runif(1, 0.5, 3)))),
                  g = factor(rep(paste0("g", seq_len(k)), n)))
  list(x = y ~ g, args = list(data = d, test = "oneway"))
}

# One random chi-squared independence design: 2 x 2 to 4 x 5, Yates or Pearson in a 2 x 2,
# some cells empty (small N). `threshold = 1` lets sparse tables through the disclosure check.
chisq_independence_design <- function() {
  r <- sample(2:4, 1L)
  cc <- sample(2:5, 1L)
  p <- rexp(r * cc)
  if (runif(1L) < 0.4) p[sample(r * cc, 1L)] <- 0
  total <- sample(c(15, 40, 150, 1000), 1L)
  idx <- sample(r * cc, total, replace = TRUE, prob = p / sum(p))
  d <- data.frame(a = factor(((idx - 1L) %% r) + 1L), b = factor(((idx - 1L) %/% r) + 1L))
  if (nlevels(d$a) < 2L || nlevels(d$b) < 2L) return(chisq_independence_design())
  list(x = ~ a + b, args = list(data = d, test = "chisq", correct = runif(1L) < 0.5,
                                threshold = 1))
}

# One random goodness-of-fit design: k = 2..8 categories, uniform or given p0, empty cells.
chisq_gof_design <- function() {
  k <- sample(2:8, 1L)
  q <- rexp(k) + 0.05
  prob <- rexp(k)
  if (runif(1L) < 0.3) prob[sample(k, 1L)] <- 0
  d <- data.frame(a = factor(sample(seq_len(k), sample(c(15, 60, 400), 1L), replace = TRUE,
                                    prob = prob), levels = seq_len(k)))
  if (sum(table(d$a) > 0) < 2L) return(chisq_gof_design())
  list(x = ~ a, args = c(list(data = d, test = "gof", threshold = 1),
                         if (runif(1L) < 0.5) list(p0 = q / sum(q))))
}

# One random factorial design: a x b = 2..3 x 2..4 cells, balanced or not, unequal SDs and means.
factorial_design <- function(balanced, type) {
  a <- sample(2:3, 1L)
  b <- sample(2:4, 1L)
  n <- if (balanced) rep(sample(4:30, 1L), a * b) else sample(3:30, a * b, replace = TRUE)
  cell <- rep(seq_len(a * b), n)
  mu <- rnorm(a * b, sd = 1.5)
  sds <- runif(a * b, 0.5, 3)
  d <- data.frame(y = rnorm(length(cell), mu[cell], sds[cell]),
                  a = factor(paste0("a", ((cell - 1L) %% a) + 1L)),
                  b = factor(paste0("b", ((cell - 1L) %/% a) + 1L)))
  list(x = y ~ a * b, args = list(data = d, test = "factorial", ss_type = type, threshold = 1))
}

designs <- list(chisq_independence = chisq_independence_design, chisq_gof = chisq_gof_design,
                t_independent = t_design, t_one_sample = t_one_sample_design,
                t_paired = t_paired_design, cor_pearson = cor_pearson_design, lm = lm_design,
                lm_simple = lm_simple_design, lm_multiple = lm_multiple_design,
                lm_moderation = lm_moderation_design,
                mediation_simple_small = function() mediation_design("small"),
                mediation_simple_large = function() mediation_design("large"),
                mediation_simple_near = function() mediation_design("near"),
                anova_oneway_balanced = function() oneway_design(TRUE),
                anova_oneway_unbalanced = function() oneway_design(FALSE),
                anova_factorial_balanced = function() factorial_design(TRUE, "III"),
                anova_factorial_unbalanced_III = function() factorial_design(FALSE, "III"),
                anova_factorial_unbalanced_II = function() factorial_design(FALSE, "II"),
                anova_factorial_unbalanced_I = function() factorial_design(FALSE, "I"))
base_design_of <- function(name) {
  if (name %in% c("lm", "lm_simple", "lm_multiple", "lm_moderation")) return("lm")
  sub("_(small|large|near)$", "", sub("_(un)?balanced(_[IV]+)?$", "", name)) |>
    (\(x) if (x == "mediation_simple") x else x)()
}
handler_of <- function(name) sub("_(small|large|near)$", "", sub("_(un)?balanced(_I+)?$", "", name))

rows <- lapply(names(designs), function(name) {
  h <- get_handler(handler_of(name))
  diffs <- vapply(seq_len(n_designs), function(i) {
    ex <- designs[[name]]()
    s <- do.call(sufficient_stats, c(list(ex$x), ex$args))
    compare_both(base_design_of(name), h, ex, s)
  }, numeric(5))
  row_of(name, diffs)
})
# Result route (ADR 0026): the same designs, shared from the fitted result instead of the
# formula. The stored summaries must reproduce what the formula route reports on the raw data,
# and the object must say which route it came from.
result_of <- function(name, ex) {
  d <- ex$args$data
  a <- ex$args
  switch(name,
    t_independent = stats::t.test(y ~ g, data = d, var.equal = a$test == "student",
                                  alternative = a$alternative, conf.level = a$conf_level),
    t_one_sample = with(d, stats::t.test(y, mu = a$mu, alternative = a$alternative,
                                      conf.level = a$conf_level)),
    t_paired = stats::t.test(Pair(pre, post) ~ 1, data = d, mu = a$mu,
                             alternative = a$alternative, conf.level = a$conf_level),
    cor_pearson = stats::cor.test(~ a + b, data = d, alternative = a$alternative,
                                  conf.level = a$conf_level),
    chisq_independence = suppressWarnings(stats::chisq.test(table(a = d$a, b = d$b), correct = a$correct)),
    anova_oneway = stats::aov(y ~ g, data = d),
    anova_factorial = stats::aov(y ~ a * b, data = d))
}
result_args <- function(name, ex) {
  if (name %in% c("t_independent", "t_one_sample", "t_paired", "cor_pearson")) {
    list(data = ex$args$data)
  } else {
    list()
  }
}
result_designs <- list(t_independent = t_design, t_one_sample = t_one_sample_design,
                       t_paired = t_paired_design, cor_pearson = cor_pearson_design,
                       chisq_independence = chisq_independence_design,
                       anova_oneway = function() oneway_design(FALSE),
                       anova_factorial = function() factorial_design(TRUE, "III"))
route_of <- c(t_independent = "t.test", t_one_sample = "t.test", t_paired = "t.test",
              cor_pearson = "cor.test", chisq_independence = "chisq.test",
              anova_oneway = "aov", anova_factorial = "aov")
result_rows <- lapply(names(result_designs), function(name) {
  h <- get_handler(name)
  diffs <- vapply(seq_len(n_designs), function(i) {
    ex <- result_designs[[name]]()
    if (name == "chisq_independence") ex$args$threshold <- 1
    s <- do.call(sufficient_stats, c(list(result_of(name, ex)), result_args(name, ex),
                                     list(threshold = if (is.null(ex$args$threshold)) 5 else ex$args$threshold)))
    if (!(s$verdict %in% c("apa_suffices", "apa_plus_suffices")) || !identical(s$meta$route, route_of[[name]])) stop(name, ": ", s$verdict, " | ", s$verdict_reason, " | ", s$meta$route)
    compare_both(name, h, ex, s)
  }, numeric(5))
  row_of(paste0(name, "_from_result"), diffs)
})
# Chi-squared goodness-of-fit result route: sufficient_stats(chisq.test(<counts>)) currently gives
# needs_raw_data ("'dimnames' applied to non-array", a known package bug being fixed on main), so
# the case is recorded here as blocked, not run, not counted as a pass, and not worked around.
gof_probe <- tryCatch(sufficient_stats(suppressWarnings(stats::chisq.test(c(5, 7, 9)))),
                      error = function(e) NULL)
gof_result_blocked <- is.null(gof_probe) || !(gof_probe$verdict %in% c("apa_suffices", "apa_plus_suffices"))
cat("\nchisq_gof from a chisq.test() result: ",
    if (gof_result_blocked) "BLOCKED by known package bug (needs_raw_data, 'dimnames' applied to non-array); re-run after the fix"
    else "route works (add it to result_designs)", "\n", sep = "")

# Mediation from two models: sufficient_stats(lm(M ~ X), lm(Y ~ X + M)) must give the same
# summaries as the formula route (same handler, same round trip).
med_rows <- lapply(c("small", "large", "near"), function(size) {
  h <- get_handler("mediation_simple")
  diffs <- vapply(seq_len(n_designs), function(i) {
    ex <- mediation_design(size)
    d <- ex$args$data
    s <- sufficient_stats(stats::lm(m ~ x, data = d), stats::lm(y ~ x + m, data = d))
    stopifnot(s$verdict == "apa_plus_suffices", identical(s$handler, "mediation_simple"))
    compare_both("mediation_simple", h, ex, s)
  }, numeric(5))
  row_of(paste0("mediation_two_models_", size), diffs)
})
table_out <- do.call(rbind, c(rows, result_rows, med_rows))
print(table_out, row.names = FALSE)
pass <- all(table_out$result == "PASS")
cat(if (pass) "\nPASS" else "\nFAIL", ": ", sum(table_out$result == "PASS"), " of ", nrow(table_out),
    " rows within ", format(tolerance), " (scaled) of reported() and of base R on the raw data\n", sep = "")

finish_validation(pass)
