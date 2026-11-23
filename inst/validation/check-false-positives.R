# False-positive check for check_reported(): the headline number.
# Claim: a paper that reports real, correct output of t.test(paired = TRUE) or
# cor.test(), rounded the way a paper prints it, is never flagged inconsistent
# (a false positive), and check_reported() never errors on it.
# Rounding as printed: 2 decimals for t, M and SD (0 decimals in the "coarse"
# designs), 3 for p and r, "< .001" below .001, CI at the decimals of M.
# Each design is checked on several reported subsets (all numbers, no CI,
# descriptives + t + p, t/df/p only, n/r/df/t, no r, random subsets).
# The draws deliberately include the cases that once broke it: |r| >= .995 with r
# rounding to 1.000, small n (3 to 6) and SD reported to zero decimals.
# PASS criterion: false positives == 0 AND errors == 0. A false positive is a
# release blocker.
# Size: 300 paired designs + 300 Pearson designs, about 4 subsets each (the
# paired "no r" subset takes about 3 s, so it is run on every 15th design only).
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-false-positives.R")'

n_designs <- 300L
set.seed(20261123)

fmt <- function(x, d) formatC(x, format = "f", digits = d)
fmt_p <- function(p) if (p < 0.001) "< .001" else fmt(p, 3L)
sizes <- c(3, 3, 4, 4, 5, 6, 8, 10, 15, 20, 30, 60, 150)
rhos <- c(0, 0.3, 0.7, 0.95, 0.99, 0.995, 0.999, 0.9999, -0.5, -0.9, -0.999)

run_checks <- function(handler, paper, subsets) {
  out <- list(checks = 0L, fp = list(), err = list())
  for (keys in subsets) {
    res <- tryCatch(suppressWarnings(do.call(check_reported, c(list(handler), paper[keys]))),
                    error = function(e) e)
    out$checks <- out$checks + 1L
    if (inherits(res, "error")) {
      out$err[[length(out$err) + 1L]] <- list(keys = keys, msg = conditionMessage(res))
    } else if (!res$consistent) {
      out$fp[[length(out$fp) + 1L]] <- list(keys = keys, paper = paper[keys])
    }
  }
  out
}

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

paired_subsets <- function(paper, with_no_r) {
  keys <- names(paper)
  subsets <- list(keys,
                  setdiff(keys, c("ci_lower", "ci_upper")),
                  c("n", "m", "sd", "r", "t", "p"),
                  c("t", "df", "p"),
                  c("n", "r", "df", "t"),
                  sample(keys, sample(2:length(keys), 1L)))
  if (with_no_r) subsets <- c(subsets, list(setdiff(keys, "r")))
  subsets
}

pearson_subsets <- function(paper) {
  keys <- names(paper)
  list(keys, c("n", "r", "df", "t", "p"), c("r", "n"), c("n", "r", "p"), c("t", "df", "p"),
       sample(keys, sample(2:length(keys), 1L)))
}

tally <- function(handler, n_designs, make, subsets_of) {
  total <- list(designs = 0L, checks = 0L, fp = list(), err = list(), r_one = 0L, small_n = 0L,
                zero_dec = 0L)
  for (i in seq_len(n_designs)) {
    paper <- make()
    if (is.null(paper)) next
    total$designs <- total$designs + 1L
    total$r_one <- total$r_one + (abs(as.numeric(paper$r)) >= 0.995)
    total$small_n <- total$small_n + (as.numeric(paper$n) <= 6)
    coarse_sd <- length(paper$sd) > 0L && !grepl(".", paper$sd[1L], fixed = TRUE)
    total$zero_dec <- total$zero_dec + coarse_sd
    res <- run_checks(handler, paper, subsets_of(paper, i))
    total$checks <- total$checks + res$checks
    total$fp <- c(total$fp, res$fp)
    total$err <- c(total$err, res$err)
  }
  total
}

paired <- tally("t_paired", n_designs,
                function() paired_paper(paired_design()),
                function(paper, i) paired_subsets(paper, i %% 15L == 0L))
pearson <- tally("cor_pearson", n_designs, pearson_paper,
                 function(paper, i) pearson_subsets(paper))

results <- list(t_paired = paired, cor_pearson = pearson)
table_out <- do.call(rbind, lapply(names(results), function(h) {
  r <- results[[h]]
  data.frame(handler = h, designs = r$designs, checks = r$checks,
             `|r|>=.995` = r$r_one, `n<=6` = r$small_n, `sd 0 dec` = r$zero_dec,
             false_positives = length(r$fp), errors = length(r$err),
             check.names = FALSE)
}))
print(table_out, row.names = FALSE)
for (h in names(results)) {
  for (f in utils::head(results[[h]]$fp, 3L)) {
    cat("\nFALSE POSITIVE", h, ":\n")
    str(f$paper)
  }
  for (f in utils::head(results[[h]]$err, 3L)) cat("\nERROR", h, ":", f$msg, "\n")
}
pass <- sum(table_out$false_positives) == 0L && sum(table_out$errors) == 0L
cat(if (pass) "\nPASS" else "\nFAIL", ": false positives = ", sum(table_out$false_positives),
    ", errors = ", sum(table_out$errors), "\n", sep = "")
