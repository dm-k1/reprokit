# Round-trip simulation: for each exact handler, random designs ->
# kit() -> reproduce() and compare with the base-R result on the raw data.
# PASS criterion (ADR 0005), worst over all designs and all reported numbers:
#   scaled difference |original - reproduced| / max(1, |original|) < 1e-8
#   (= absolute for numbers up to 1, relative for larger ones such as a big F).
# The worst absolute difference is shown for information only.
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

designs <- list(t_independent = t_design, lm = lm_design)

rows <- lapply(names(designs), function(name) {
  h <- get_handler(name)
  diffs <- vapply(seq_len(n_designs), function(i) {
    ex <- designs[[name]]()
    k <- do.call(kit, c(list(ex$x), ex$args, list(sharing_reason = "simulation")))
    worst_diff(do.call(h$reported, c(list(ex$x), ex$args)), reproduce(k))
  }, numeric(2))
  worst <- apply(diffs, 1L, max)
  data.frame(handler = name, designs = n_designs,
             worst_absolute = signif(worst[["absolute"]], 3),
             worst_scaled = signif(worst[["scaled"]], 3),
             result = if (worst[["scaled"]] < tolerance) "PASS" else "FAIL")
})
print(do.call(rbind, rows), row.names = FALSE)
