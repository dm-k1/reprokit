# Shared generators of correctly reported papers for the check_reported() validation scripts
# (check-false-positives.R, check-planted-errors.R, check-statcheck.R). Each generator draws raw
# data, runs the real t.test() / cor.test(), and returns the numbers rounded as a paper prints
# them (2 decimals for t, 3 for p and r, "< .001" below .001). Sourced, not run: it draws nothing
# until a generator is called, so the caller owns the seed.

fmt <- function(x, d) formatC(x, format = "f", digits = d)
fmt_p <- function(p) if (p < 0.001) "< .001" else fmt(p, 3L)
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
  tt <- t.test(d$x, d$y, paired = TRUE)
  r <- cor(d$x, d$y)
  if (!is.finite(r) || !is.finite(tt$statistic)) return(NULL)
  dec <- d$dec
  n <- length(d$x)
  list(n = as.character(n), m = c(fmt(mean(d$x), dec), fmt(mean(d$y), dec)),
       sd = c(fmt(sd(d$x), dec), fmt(sd(d$y), dec)), r = fmt(r, 3L),
       t = fmt(unname(tt$statistic), 2L), df = as.character(n - 1L),
       p = fmt_p(tt$p.value), ci_lower = fmt(tt$conf.int[1L], dec),
       ci_upper = fmt(tt$conf.int[2L], dec))
}

pearson_paper <- function() {
  n <- sample(sizes, 1L)
  rho <- if (runif(1L) < 0.5) sample(rhos, 1L) else runif(1L, -0.99, 0.99)
  x <- rnorm(n)
  y <- rho * x + sqrt(1 - rho^2) * rnorm(n)
  r <- suppressWarnings(cor(x, y))
  if (!is.finite(r)) return(NULL)
  ct <- cor.test(x, y)
  dec <- sample(2:3, 1L)
  paper <- list(n = as.character(n), r = fmt(unname(ct$estimate), dec),
                df = as.character(n - 2L), t = fmt(unname(ct$statistic), 2L),
                p = fmt_p(ct$p.value))
  if (n > 3L) {
    paper <- c(paper, list(ci_lower = fmt(ct$conf.int[1L], dec),
                           ci_upper = fmt(ct$conf.int[2L], dec)))
  }
  paper
}

independent_paper <- function(var_equal) {
  n1 <- sample(sizes[sizes >= 3], 1L)
  n2 <- sample(sizes[sizes >= 3], 1L)
  scale <- 10^runif(1L, -1, 2)
  x <- rnorm(n1) * scale * runif(1L, 0.3, 3) + runif(1L, -1, 1) * scale
  y <- rnorm(n2) * scale * runif(1L, 0.3, 3) + runif(1L, -1, 1) * scale
  tt <- t.test(x, y, var.equal = var_equal)
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
  paper
}

one_sample_paper <- function() {
  n <- sample(sizes, 1L)
  scale <- 10^runif(1L, -1, 2)
  x <- rnorm(n) * scale + runif(1L, -1, 1) * scale
  mu <- if (runif(1L) < 0.4) 0 else round(runif(1L, -1, 1) * scale, sample(0:2, 1L))
  tt <- t.test(x, mu = mu)
  if (!is.finite(tt$statistic) || sd(x) == 0) return(NULL)
  dec <- sample(c(0L, 2L, 3L), 1L, prob = c(0.3, 0.4, 0.3))
  paper <- list(n = as.character(n), m = fmt(mean(x), dec), sd = fmt(sd(x), dec),
                t = fmt(unname(tt$statistic), 2L), df = as.character(n - 1L),
                p = fmt_p(tt$p.value), ci_lower = fmt(tt$conf.int[1L], dec),
                ci_upper = fmt(tt$conf.int[2L], dec))
  attr(paper, "mu") <- mu
  paper
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

# p moved across .05 with t and df unchanged.
plant_decision <- function(paper) {
  p0 <- p_value(paper$p)
  significant <- is.na(p0) || p0 < 0.05
  paper$p <- fmt_p(if (significant) runif(1L, 0.055, 0.6) else runif(1L, 0.002, 0.044))
  list(paper = paper, quantity = "p", size = "large")
}

planted_types <- function(handler) {
  c("t_digit", "p_digit", "df_wrong_n", "decision",
    if (handler == "cor_pearson") "r_digit" else "m_change")
}

plant_error <- function(paper, handler, type) {
  switch(type, t_digit = plant_t_digit(paper), p_digit = plant_p_digit(paper),
         df_wrong_n = plant_df_wrong_n(paper, handler), decision = plant_decision(paper),
         r_digit = plant_r_digit(paper), m_change = plant_m_change(paper))
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
  cor_pearson = list(handler = "cor_pearson", make = pearson_paper)
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
  desc <- switch(handler, t_paired = c("n", "m", "sd", "r", "t", "p"),
                 cor_pearson = c("n", "r", "t", "p"), c("n", "m", "sd", "t", "p"))
  subsets <- list(all = names(paper), `t/df/p` = c("t", "df", "p"), `desc+t+p` = desc)
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
