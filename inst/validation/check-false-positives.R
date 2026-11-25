# False-positive check for check_reported(): the headline number.
# Claim: a paper that reports real, correct output of t.test(paired = TRUE),
# t.test() for independent groups (Student with var.equal = TRUE, and Welch), t.test()
# against mu (mu = 0 and mu != 0) or cor.test(), rounded the way a paper prints it,
# is never flagged inconsistent (a false positive), and check_reported() never errors
# on it. Independent-groups designs have unequal n and SDs, a Welch df printed with 1
# or 2 decimals, and sometimes a pooled Cohen's d.
# CIs are printed at 90, 95 or 99% (a quarter of the papers each at 90 and 99) and
# must fit one of the three levels; a paired paper also prints d in 30% of the papers, in
# one of the conventions d_z, d_rm or d_av (Lakens 2013), and a separate paired-d run forces d
# into every paper: those must be 100% not inconsistent, in every subset that includes d.
# One-way ANOVA: oneway.test(var.equal = TRUE) output for k = 2..6 groups, balanced
# and unbalanced, printed as F 2 dp, p 3 dp or "< .001", M and SD 2 dp, eta2 or omega2 2 dp
# (half each; a negative omega2 is printed with its sign), on subsets of every kind; a second run
# draws groups with equal true means (F < 1 often) and keeps the designs whose omega2 prints
# negative (ADR 0022), every one of which must be not inconsistent and not an input error.
# Rounding as printed: 2 decimals for t, M and SD (0 decimals in the "coarse"
# designs), 3 for p and r, "< .001" below .001, CI at the decimals of M.
# Each design is checked on several reported subsets (all numbers, no CI,
# descriptives + t + p, t/df/p only, n/r/df/t, no r, random subsets; for Pearson also r/df/p
# and r/df/p/CI, where n is not reported and is implied by df).
# The draws deliberately include the cases that once broke it: |r| >= .995 with r
# rounding to 1.000, small n (3 to 6) and SD reported to zero decimals.
# Chi-squared (ADR 0016): chisq.test() output for independence tables (2 x 2 to 4 x 5, Pearson
# or Yates in a 2 x 2, empty cells, V or signed phi) and goodness of fit (k = 2..8, uniform or
# given p0, w reported); one more group is forced to Cohen's w > 1 (k = 4, 97% of the counts in
# one category, uniform p0), which must not be flagged: w is not bounded by 1 (ADR 0021).
# Factorial ANOVA (ADR 0017): one effect (A, B or the interaction) of a classical type I, II or
# III table for 2 x 2 to 3 x 4 designs, balanced and unbalanced, F 2 dp, p 3 dp or "< .001", cell
# M and SD 2 dp, partial eta2 or partial omega2 (half each; a negative omega2 is printed with its
# sign), on subsets of every kind; the SS type the paper used is never told to the checker, so a
# paper printed at type I or II must fit by its own type and not be flagged.
# Regression family (ADR 0020): real lm() fits for simple, multiple (k = 2..6 columns, with and
# without a factor, weighted or not) and moderation (with covariates, weighted or not), printed as
# b, SE, t, p, the CI (90/95/99%), R2, F, df, n; each paper is checked on every kind of subset:
# everything, no CI, a subset of the terms (with and without k declared), t/p/df only, the model
# row only, n and df2 without k (n is not checkable), the adjusted R2 printed as R2, the
# unstandardised b printed as beta (with and without the SDs), the simple regression's r and
# SDs, and random subsets. Simple mediation: three lm() fits, the paths a, b, c and c' printed
# (b, SE, t, p, df and the path CI at 90, 95 or 99 per cent), a*b with a bootstrap CI printed in some subsets: no row may be inconsistent, and the a*b numbers
# must come out "not checkable", never fitting or not fitting (a false positive too).
# PASS criterion: false positives == 0 AND errors == 0. A false positive is a
# release blocker.
# Size: 300 designs per handler (anova_oneway, anova_factorial (all shapes, and 2 x 2 only), chisq_independence, chisq_gof, chisq_gof w > 1,
# t_paired, t_paired with d, cor_pearson, t_independent Student
# and Welch, t_one_sample), about 4 to 6 subsets each (the paired "no r" subset takes about
# 3 s, so it is run on every 15th design only).
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-false-positives.R")'

source("inst/validation/record.R")
record_start("check-false-positives", seed = 20261123)

n_designs <- 300L
set.seed(20261123)

source("inst/validation/check-designs.R")

run_checks <- function(handler, paper, subsets) {
  out <- list(checks = 0L, fp = list(), err = list())
  mu <- attr(paper, "mu")
  for (keys in subsets) {
    args <- c(list(handler), paper[keys], if (!is.null(mu)) list(mu = mu))
    res <- tryCatch(suppressWarnings(do.call(check_reported, args)),
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

generic_subsets <- function(paper, i) {
  keys <- names(paper)
  list(keys, setdiff(keys, c("ci_lower", "ci_upper")), c("n", "m", "sd", "t", "p"),
       c("t", "df", "p"), c("n", "df", "t"), c("n", "m", "sd", "df"),
       intersect(keys, c("n", "m", "sd", "ci_lower", "ci_upper")),
       intersect(keys, c("n", "m", "sd", "df", "ci_lower", "ci_upper")),
       sample(keys, sample(2:length(keys), 1L)))
}

paired_subsets <- function(paper, with_no_r) {
  keys <- names(paper)
  subsets <- list(keys,
                  setdiff(keys, c("ci_lower", "ci_upper")),
                  c("n", "m", "sd", "r", "t", "p"),
                  c("t", "df", "p"),
                  c("n", "r", "df", "t"),
                  intersect(keys, c("n", "m", "sd", "r", "ci_lower", "ci_upper")),
                  intersect(keys, c("n", "m", "sd", "r", "d")),
                  intersect(keys, c("n", "m", "sd", "d")),
                  intersect(keys, c("m", "sd", "r", "d")),
                  sample(keys, sample(2:length(keys), 1L)))
  if (with_no_r) subsets <- c(subsets, list(setdiff(keys, "r")))
  subsets
}

pearson_subsets <- function(paper) {
  keys <- names(paper)
  # r/df/p and r/df/p/ci: n is not reported and is implied by df (n = df + 2)
  subsets <- list(keys, c("n", "r", "df", "t", "p"), c("r", "n"), c("n", "r", "p"),
                  c("t", "df", "p"), c("r", "df", "p"),
                  sample(keys, sample(2:length(keys), 1L)))
  if (all(c("ci_lower", "ci_upper") %in% keys)) {
    subsets <- c(subsets, list(c("r", "df", "p", "ci_lower", "ci_upper")))
  }
  subsets
}

tally <- function(handler, n_designs, make, subsets_of) {
  total <- list(designs = 0L, checks = 0L, fp = list(), err = list(), r_one = 0L, small_n = 0L,
                zero_dec = 0L)
  for (i in seq_len(n_designs)) {
    paper <- make()
    if (is.null(paper)) next
    total$designs <- total$designs + 1L
    total$r_one <- total$r_one + isTRUE(abs(as.numeric(paper$r)) >= 0.995)
    total$small_n <- total$small_n + any(as.numeric(paper$n) <= 6)
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
# d forced into every paper; every subset below contains d, so each check tests a convention.
paired_d <- tally("t_paired", n_designs, function() {
  repeat {
    paper <- paired_paper(paired_design())
    if (!is.null(paper) && !is.null(paper$d)) return(paper)
  }
}, function(paper, i) {
  keep <- function(keys) intersect(keys, names(paper))
  list(keep(c("n", "m", "sd", "r", "d")), keep(c("n", "m", "sd", "d")),
       keep(c("m", "sd", "r", "d")), names(paper), keep(c("n", "m", "sd", "r", "t", "d")))
})
pearson <- tally("cor_pearson", n_designs, pearson_paper,
                 function(paper, i) pearson_subsets(paper))

student <- tally("t_independent", n_designs, function() independent_paper(TRUE),
                 generic_subsets)
welch <- tally("t_independent", n_designs, function() independent_paper(FALSE),
               generic_subsets)
one_sample <- tally("t_one_sample", n_designs, one_sample_paper, generic_subsets)

oneway_subsets <- function(paper, i) {
  keys <- names(paper)
  list(keys, setdiff(keys, "eta2"), c("n", "m", "sd", "f", "p"), c("f", "df1", "df2", "p"),
       c("n", "df1", "df2", "f"), c("n", "m", "sd", "df1", "df2"),
       c("n", "m", "sd", "f", "eta2"), c("f", "df1", "df2", "eta2"),
       sample(keys, sample(2:length(keys), 1L)))
}
oneway <- tally("anova_oneway", n_designs, function() oneway_paper(), oneway_subsets)
# F < 1: equal true means, negative omega2 printed with its sign.
oneway_neg <- tally("anova_oneway", 3L * n_designs,
                    function() oneway_paper(oneway_raw(null = TRUE), negative_omega = TRUE),
                    oneway_subsets)

factorial_subsets <- function(paper, i) {
  keys <- names(paper)
  list(keys, setdiff(keys, "eta2"), c("n", "m", "sd", "f", "p"), c("f", "df1", "df2", "p"),
       c("n", "df1", "df2", "f"), c("n", "m", "sd", "df1", "df2"),
       c("n", "m", "sd", "f", "eta2"), c("f", "df1", "df2", "eta2"),
       sample(keys, sample(2:length(keys), 1L)))
}
factorial_tally <- function(make, n_designs) {
  total <- list(designs = 0L, checks = 0L, fp = list(), err = list(), unbalanced = 0L)
  for (i in seq_len(n_designs)) {
    paper <- make()
    if (is.null(paper)) next
    total$designs <- total$designs + 1L
    total$unbalanced <- total$unbalanced + !isTRUE(attr(paper, "balanced"))
    for (keys in factorial_subsets(paper, i)) {
      res <- run_factorial_check(paper, keys)
      total$checks <- total$checks + 1L
      if (inherits(res, "error")) {
        total$err[[length(total$err) + 1L]] <- list(msg = conditionMessage(res))
      } else if (!res$consistent) {
        total$fp[[length(total$fp) + 1L]] <- list(keys = keys, paper = paper[keys])
      }
    }
  }
  total
}
factorial_all <- factorial_tally(factorial_paper, n_designs)
factorial_22 <- factorial_tally(function() factorial_paper(factorial_raw(c(2L, 2L))), n_designs)

chisq_tally <- function(make, n_designs) {
  total <- list(designs = 0L, checks = 0L, fp = list(), err = list(), yates = 0L, w1 = 0L)
  for (i in seq_len(n_designs)) {
    paper <- make()
    if (is.null(paper)) next
    total$designs <- total$designs + 1L
    total$yates <- total$yates + isTRUE(attr(paper, "truth")$yates)
    total$w1 <- total$w1 + isTRUE(attr(paper, "truth")$w > 1)
    for (keys in chisq_subsets(paper)) {
      res <- run_chisq_check(paper, keys)
      total$checks <- total$checks + 1L
      if (inherits(res, "error")) {
        total$err[[length(total$err) + 1L]] <- list(msg = conditionMessage(res))
      } else if (!res$consistent) {
        total$fp[[length(total$fp) + 1L]] <- list(keys = keys, paper = paper[keys])
      }
    }
  }
  total
}
chisq_indep <- chisq_tally(chisq_independence_paper, n_designs)
chisq_indep22 <- chisq_tally(function() chisq_independence_paper(force_2x2 = TRUE), n_designs)
chisq_gof_all <- chisq_tally(chisq_gof_paper, n_designs)
chisq_gof_w1 <- chisq_tally(function() chisq_gof_paper(high_w = TRUE, k = 4L), n_designs)

# Regression family -----------------------------------------------------------------------
lm_cases <- function(paper, i) {
  keys <- names(paper)
  model <- c("r2", "f", "df1", "df2", "p_model")
  coefs <- names(paper$b)
  some <- if (length(coefs) > 1L) sample(coefs, sample(seq_len(length(coefs) - 1L), 1L)) else coefs
  cases <- list(
    list(keys = keys), list(keys = setdiff(keys, c("ci_lower", "ci_upper"))),
    list(keys = keys, terms_only = some), list(keys = keys, terms_only = some, k = attr(paper, "k")),
    list(keys = c("t", "p", "df2")), list(keys = c("b", "se", "t")), list(keys = model),
    list(keys = c("df2", "n")), list(keys = c("df2", "n", "t", "p")),
    list(keys = c("df2", "n"), k = attr(paper, "k")),
    list(keys = c("adj_r2", "f", "df1", "df2", "p_model")),
    list(keys = c("adj_r2", "f", "df1", "df2", "n")),
    list(keys = c("b", "se", "beta", "sd")), list(keys = c("b", "beta")),
    list(keys = c("b", "se", "t", "beta", "sd", "r2", "f", "df1", "df2", "n")),
    list(keys = setdiff(keys, c("ci_lower", "ci_upper")), terms_only = some),
    list(keys = c("b", "se", "ci_lower", "ci_upper", "df2")),
    list(keys = sample(keys, sample(2:length(keys), 1L))))
  if (attr(paper, "handler") == "lm_simple") {
    cases <- c(cases, list(list(keys = c("b", "r", "sd", "r2", "t", "f", "df1", "df2", "n")),
                           list(keys = c("b", "r", "sd")), list(keys = c("r2", "r", "n")),
                           list(keys = c("t", "f", "df1", "df2"))))
  }
  cases
}
lm_tally <- function(make, n_designs) {
  total <- list(designs = 0L, checks = 0L, fp = list(), err = list())
  for (i in seq_len(n_designs)) {
    paper <- make()
    if (is.null(paper)) next
    total$designs <- total$designs + 1L
    for (case in lm_cases(paper, i)) {
      res <- run_lm_check(paper, case[["keys"]], case[["terms_only"]], case[["k"]])
      total$checks <- total$checks + 1L
      if (inherits(res, "error")) {
        total$err[[length(total$err) + 1L]] <- list(msg = conditionMessage(res), keys = case$keys)
      } else if (!res$consistent) {
        total$fp[[length(total$fp) + 1L]] <- list(keys = case$keys, paper = paper[case$keys])
      }
    }
  }
  total
}
lm_make <- function(handler, ...) function() lm_paper(lm_raw(handler, ...))
lm_simple_fp <- lm_tally(lm_make("lm_simple", weighted = FALSE), n_designs)
lm_simple_w_fp <- lm_tally(lm_make("lm_simple", weighted = TRUE), n_designs)
lm_multiple_fp <- lm_tally(lm_make("lm_multiple", weighted = FALSE, with_factor = FALSE), n_designs)
lm_multiple_f_fp <- lm_tally(lm_make("lm_multiple", weighted = FALSE, with_factor = TRUE), n_designs)
lm_multiple_w_fp <- lm_tally(lm_make("lm_multiple", weighted = TRUE), n_designs)
lm_moderation_fp <- lm_tally(lm_make("lm_moderation", weighted = FALSE), n_designs)
lm_moderation_w_fp <- lm_tally(lm_make("lm_moderation", weighted = TRUE), n_designs)

mediation_cases <- function(paper) {
  keys <- names(paper)
  paths <- c("a", "b", "c", "c_prime")
  list(list(keys = keys), list(keys = setdiff(keys, c("indirect", "indirect_ci_lower", "indirect_ci_upper"))),
       list(keys = c("b", "se", "t", "p", "df")), list(keys = keys, paths = c("a", "b")),
       list(keys = c("b", "se", "df", "ci_lower", "ci_upper")),
       list(keys = c("ci_lower", "ci_upper", "df", "n")),
       list(keys = c("b", "se", "ci_lower", "ci_upper"), paths = sample(paths, 2L)),
       list(keys = keys, paths = sample(paths, 2L)), list(keys = c("t", "df", "p")),
       list(keys = c("n", "indirect", "indirect_ci_lower", "indirect_ci_upper")),
       list(keys = c("b", "se", "t", "indirect", "indirect_ci_lower", "indirect_ci_upper")),
       list(keys = c("b", "se", "t", "p", "df", "n", "indirect")),
       list(keys = sample(keys, sample(2:length(keys), 1L))))
}
mediation_tally <- function(size, n_designs) {
  total <- list(designs = 0L, checks = 0L, fp = list(), err = list())
  for (i in seq_len(n_designs)) {
    paper <- mediation_paper(mediation_raw(size))
    total$designs <- total$designs + 1L
    for (case in mediation_cases(paper)) {
      res <- run_mediation_check(paper, case$keys, case$paths)
      total$checks <- total$checks + 1L
      if (inherits(res, "error")) {
        total$err[[length(total$err) + 1L]] <- list(msg = conditionMessage(res), keys = case$keys)
      } else if (!res$consistent || !indirect_untouched(res)) {
        total$fp[[length(total$fp) + 1L]] <- list(keys = case$keys, paper = paper[case$keys])
      }
    }
  }
  total
}
mediation_small_fp <- mediation_tally("small", n_designs)
mediation_large_fp <- mediation_tally("large", n_designs)
mediation_mixed_fp <- mediation_tally("mixed", n_designs)

results <- list(chisq_independence = chisq_indep, chisq_independence_2x2 = chisq_indep22,
                chisq_gof = chisq_gof_all, chisq_gof_w_gt_1 = chisq_gof_w1, t_paired = paired, t_paired_d = paired_d, cor_pearson = pearson,
                t_independent_student = student, t_independent_welch = welch,
                t_one_sample = one_sample, anova_oneway = oneway, anova_oneway_negative_omega2 = oneway_neg,
                anova_factorial = factorial_all, anova_factorial_2x2 = factorial_22,
                lm_simple = lm_simple_fp, lm_simple_weighted = lm_simple_w_fp,
                lm_multiple = lm_multiple_fp, lm_multiple_factor = lm_multiple_f_fp,
                lm_multiple_weighted = lm_multiple_w_fp, lm_moderation = lm_moderation_fp,
                lm_moderation_weighted = lm_moderation_w_fp, mediation_small = mediation_small_fp,
                mediation_large = mediation_large_fp, mediation_mixed = mediation_mixed_fp)
table_out <- do.call(rbind, lapply(names(results), function(h) {
  r <- results[[h]]
  data.frame(handler = h, designs = r$designs, checks = r$checks,
             `|r|>=.995` = if (is.null(r$r_one)) 0L else r$r_one,
             `n<=6` = if (is.null(r$small_n)) 0L else r$small_n,
             `sd 0 dec` = if (is.null(r$zero_dec)) 0L else r$zero_dec,
             yates = if (is.null(r$yates)) 0L else r$yates,
             `w>1` = if (is.null(r$w1)) 0L else r$w1,
             unbalanced = if (is.null(r$unbalanced)) 0L else r$unbalanced,
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
w1_count <- chisq_gof_w1$w1
pass <- pass && w1_count > 0L && oneway_neg$designs > 0L
cat("one-way designs with F < 1 and a negative omega2 checked and not flagged:",
    oneway_neg$designs, "\n")
cat("goodness-of-fit designs with w > 1 checked and not flagged:", w1_count, "\n")
cat(if (pass) "\nPASS" else "\nFAIL", ": false positives = ", sum(table_out$false_positives),
    ", errors = ", sum(table_out$errors), "\n", sep = "")

record_stop()
