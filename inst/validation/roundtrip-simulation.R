# Round-trip simulation: for each exact handler, random designs ->
# sufficient_stats() -> reproduce() and compare with the base-R result on the raw data.
# PASS criterion, worst over all designs and all reported numbers:
#   scaled difference |original - reproduced| / max(1, |original|) < 1e-8
#   (= absolute for numbers up to 1, relative for larger ones such as a big F).
# The worst absolute difference is shown for information only.
# Chi-squared (ADR 0016): independence tables 2 x 2 to 4 x 5 (Yates or Pearson in a 2 x 2) and
# goodness of fit with k = 2..8 (uniform or given p0), both with empty cells.
# Factorial ANOVA (ADR 0017): 2 x 2 to 3 x 4 designs, balanced (types agree) and unbalanced with
# each SS type (III, II, I), unequal SDs and cell means, against anova()/drop1() on the raw data.
# Regression family (ADR 0020): lm_simple (k = 1), lm_multiple (k = 2..6 predictor columns, a factor
# in half the designs, weights in half), lm_moderation (X * Z with 0..2 covariates, weights in half)
# and mediation_simple (small paths, large paths, and a mediator that is nearly X); every design
# is also checked to reach the intended handler, against summary(lm) on the raw data (for mediation,
# against the paths of the three lm() fits through the handler's own reported()).
# Run: Rscript inst/validation/roundtrip-simulation.R   (after devtools::load_all())

n_designs <- 200L
tolerance <- 1e-8
set.seed(20261122)

worst_diff <- function(original, reproduced) {
  stopifnot(identical(names(original), names(reproduced)))
  d <- ifelse(original == reproduced, 0, abs(original - reproduced))
  c(absolute = max(d), scaled = max(d / pmax(1, abs(original))))
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
handler_of <- function(name) sub("_(small|large|near)$", "", sub("_(un)?balanced(_I+)?$", "", name))

rows <- lapply(names(designs), function(name) {
  h <- get_handler(handler_of(name))
  diffs <- vapply(seq_len(n_designs), function(i) {
    ex <- designs[[name]]()
    s <- do.call(sufficient_stats, c(list(ex$x), ex$args, list(sharing_reason = "simulation")))
    worst_diff(do.call(h$reported, c(list(ex$x), ex$args)), reproduce(s))
  }, numeric(2))
  worst <- apply(diffs, 1L, max)
  data.frame(handler = name, designs = n_designs,
             worst_absolute = signif(worst[["absolute"]], 3),
             worst_scaled = signif(worst[["scaled"]], 3),
             result = if (worst[["scaled"]] < tolerance) "PASS" else "FAIL")
})
print(do.call(rbind, rows), row.names = FALSE)
