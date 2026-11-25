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
# Added in the hardening pass:
# * Integer data (Likert 1-5 and 1-7, Poisson counts) with n chosen so that means land on exact
#   .5 ties (n = 4, 8, 10, 16, 20, 24, 40, 50, printed at 0 to 2 decimals): every draw is printed
#   twice, with round-half-even (sprintf, R) and round-half-up (SPSS, Excel), for Student, Welch,
#   one-sample, paired and Pearson. Both must be not inconsistent; the number of draws where the
#   two printings differ (an exact tie met) is printed and must be > 0 (non-vacuity).
# * One-sided papers (alternative = "less" / "greater", the p printed one-sided), for the t and
#   Pearson handlers, checked with variants = "less"/"greater" (the caller states the tail) and
#   with all tails. One-way, factorial, chi-squared and gof have no tail to state (F and chi-squared
#   are upper-tail tests) and lm and mediation take no tail variant: recorded as not tested.
# * A Welch df printed at 0 decimals, continuous and Likert data.
# * lm with n = 10 to 19 (simple, multiple, moderation).
# * Mediation: the indirect effect printed as round(a * b) and as round(round(a) * round(b)); both
#   must be "coherent" whenever a, b and the indirect are checked together.
# Non-vacuity: for every group the table lists how many checks compared at least one relation that
# can falsify ("checked") and the status of every reported number. A group fails if its checked
# share is below the minimum stated in `min_checked_share` below (the share observed in the
# recorded run minus a margin of 5 percentage points, rounded down to 5), and if any design's
# all-numbers subset was not checked. A group with no stated minimum fails.
# Parallel: designs are drawn in chunks of 15 per group, each chunk with its own L'Ecuyer-CMRG
# stream (parallel-helpers.R), so the output is identical for any number of cores
# (REPROKIT_VALIDATION_CORES).
# PASS criterion: false positives == 0 AND errors == 0 AND every group's checked share >= its
# minimum. A false positive is a release blocker.
# Size: 300 designs per handler (anova_oneway, anova_factorial (all shapes, and 2 x 2 only), chisq_independence, chisq_gof, chisq_gof w > 1,
# t_paired, t_paired with d, cor_pearson, t_independent Student
# and Welch, t_one_sample), about 4 to 6 subsets each (the paired "no r" subset takes about
# 3 s, so it is run on every 15th design only).
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-false-positives.R")'
# Exits with status 1 on FAIL under Rscript.


source("inst/validation/record.R")
source("inst/validation/parallel-helpers.R")
source("inst/validation/check-designs.R")
record_start("check-false-positives", seed = 20261123)

n_designs <- 300L
chunk <- 15L
calibrate <- nzchar(Sys.getenv("REPROKIT_VALIDATION_CALIBRATE"))
started <- Sys.time()

# Subsets of the reported numbers each family is checked on --------------------------------
generic_subsets <- function(paper, i) {
  keys <- names(paper)
  list(keys, setdiff(keys, c("ci_lower", "ci_upper")), c("n", "m", "sd", "t", "p"),
       c("t", "df", "p"), c("n", "df", "t"), c("n", "m", "sd", "df"),
       intersect(keys, c("n", "m", "sd", "ci_lower", "ci_upper")),
       intersect(keys, c("n", "m", "sd", "df", "ci_lower", "ci_upper")),
       sample(keys, sample(2:length(keys), 1L)))
}

paired_subsets <- function(paper, i) {
  keys <- names(paper)
  with_no_r <- i %% 15L == 0L
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

paired_d_subsets <- function(paper, i) {
  keep <- function(keys) intersect(keys, names(paper))
  list(keep(c("n", "m", "sd", "r", "d")), keep(c("n", "m", "sd", "d")),
       keep(c("m", "sd", "r", "d")), names(paper), keep(c("n", "m", "sd", "r", "t", "d")))
}

pearson_subsets <- function(paper, i) {
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

oneway_subsets <- function(paper, i) {
  keys <- names(paper)
  list(keys, setdiff(keys, "eta2"), c("n", "m", "sd", "f", "p"), c("f", "df1", "df2", "p"),
       c("n", "df1", "df2", "f"), c("n", "m", "sd", "df1", "df2"),
       c("n", "m", "sd", "f", "eta2"), c("f", "df1", "df2", "eta2"),
       sample(keys, sample(2:length(keys), 1L)))
}
factorial_subsets <- oneway_subsets

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

mediation_cases <- function(paper, i) {
  keys <- names(paper)
  paths <- c("a", "b", "c", "c_prime")
  cases <- list(
    list(keys = keys), list(keys = setdiff(keys, c("indirect", "indirect_ci_lower", "indirect_ci_upper"))),
    list(keys = c("b", "se", "t", "p", "df")), list(keys = keys, paths = c("a", "b")),
    list(keys = c("b", "se", "df", "ci_lower", "ci_upper")),
    list(keys = c("ci_lower", "ci_upper", "df", "n")),
    list(keys = c("b", "se", "ci_lower", "ci_upper"), paths = sample(paths, 2L)),
    list(keys = keys, paths = sample(paths, 2L)), list(keys = c("t", "df", "p")),
    list(keys = c("n", "indirect", "indirect_ci_lower", "indirect_ci_upper")),
    list(keys = c("b", "se", "t", "indirect", "indirect_ci_lower", "indirect_ci_upper")),
    list(keys = c("b", "se", "t", "p", "df", "n", "indirect")),
    list(keys = sample(keys, sample(2:length(keys), 1L))))
  # the rounded-product variant of the indirect effect only matters where it is checked
  if (!is.null(attr(paper, "variant"))) {
    cases <- Filter(function(case) "indirect" %in% case[["keys"]], cases)
  }
  cases
}

chisq_cases <- function(paper, i) lapply(chisq_subsets(paper), function(keys) list(keys = keys))
as_cases <- function(subsets_of) function(paper, i) lapply(subsets_of(paper, i), function(keys) list(keys = keys))
# One-sided papers: every subset is checked with the tail stated, and the all-numbers subset also
# with all tails open.
one_sided_cases <- function(subsets_of) function(paper, i) {
  alt <- attr(paper, "alternative")
  c(list(list(keys = names(paper))),
    lapply(subsets_of(paper, i), function(keys) list(keys = keys, variants = alt)))
}

# Runners: check_reported() on a case; an error is returned, not thrown -----------------------
run_t <- function(test) function(paper, case) {
  mu <- attr(paper, "mu")
  args <- c(list(test = test), paper[case[["keys"]]], if (!is.null(mu)) list(mu = mu),
            if (!is.null(case[["variants"]])) list(variants = case[["variants"]]))
  tryCatch(suppressWarnings(do.call(check_reported, args)), error = function(e) e)
}
run_oneway <- run_t("oneway")
run_factorial <- function(paper, case) run_factorial_check(paper, case[["keys"]])
run_chisq <- function(paper, case) run_chisq_check(paper, case[["keys"]])
run_lm <- function(paper, case) run_lm_check(paper, case[["keys"]], case[["terms_only"]], case[["k"]])
run_mediation <- function(paper, case) run_mediation_check(paper, case[["keys"]], case[["paths"]])

# A false positive: not consistent. Mediation: also an indirect effect that is not "coherent" when
# a, b and the indirect are checked together, or a bootstrap CI that is judged.
bad_default <- function(res, paper, case) !res$consistent
bad_mediation <- function(res, paper, case) {
  if (!res$consistent || !indirect_untouched(res)) return(TRUE)
  together <- all(c("b", "indirect") %in% case[["keys"]]) &&
    (is.null(case[["paths"]]) || all(c("a", "b") %in% case[["paths"]]))
  if (together) {
    status <- res$numbers$status[res$numbers$quantity == "indirect"]
    if (!identical(status, "coherent")) return(TRUE)
  }
  FALSE
}

extra_t <- function(paper) {
  c(`|r|>=.995` = isTRUE(abs(as.numeric(paper$r)) >= 0.995), `n<=6` = any(as.numeric(paper$n) <= 6),
    `sd 0 dec` = length(paper$sd) > 0L && !grepl(".", paper$sd[1L], fixed = TRUE))
}
extra_chisq <- function(paper) {
  c(yates = isTRUE(attr(paper, "truth")$yates), `w>1` = isTRUE(attr(paper, "truth")$w > 1))
}
extra_factorial <- function(paper) c(unbalanced = !isTRUE(attr(paper, "balanced")))
extra_welch_df0 <- function(paper) c(`df 0 dec` = !grepl(".", paper$df, fixed = TRUE))
extra_one_sided <- function(paper) c(`one-sided` = !identical(attr(paper, "alternative"), "two.sided"))

# A family: make() returns a list of papers (empty if the draw is degenerate).
family <- function(make, cases, run, bad = bad_default, extra = NULL, designs = n_designs) {
  list(make = make, cases = cases, run = run, bad = bad, extra = extra, designs = designs)
}
one <- function(f) function() { p <- f(); if (is.null(p)) list() else list(p) }
modes <- function(f) function() {
  papers <- both_roundings(f)
  attr(papers, "tie") <- tie_hit(papers)
  papers
}
random_tail <- function() sample(c("less", "greater"), 1L)
pair_d <- function() {
  repeat {
    paper <- paired_paper(paired_design())
    if (!is.null(paper) && !is.null(paper$d)) return(paper)
  }
}
lm_make <- function(handler, ...) one(function() lm_paper(lm_raw(handler, ...)))
lm_fam <- function(handler, designs = n_designs, ...) {
  family(lm_make(handler, ...), lm_cases, run_lm, extra = NULL, designs = designs)
}
med_make <- function(size) function() {
  paper <- mediation_paper(mediation_raw(size))
  alt <- paper
  alt$indirect <- attr(paper, "indirect_rounded_product")
  attr(alt, "variant") <- "rounded_product"
  list(paper, alt)
}

families <- list(
  t_paired = family(one(function() paired_paper(paired_design())), as_cases(paired_subsets), run_t("paired"), extra = extra_t),
  t_paired_d = family(one(pair_d), as_cases(paired_d_subsets), run_t("paired"), extra = extra_t),
  cor_pearson = family(one(pearson_paper), as_cases(pearson_subsets), run_t("pearson"), extra = extra_t),
  t_independent_student = family(one(function() independent_paper(TRUE)), as_cases(generic_subsets), run_t("student"), extra = extra_t),
  t_independent_welch = family(one(function() independent_paper(FALSE)), as_cases(generic_subsets), run_t("welch"), extra = extra_t),
  t_one_sample = family(one(one_sample_paper), as_cases(generic_subsets), run_t("one_sample"), extra = extra_t),
  anova_oneway = family(one(function() oneway_paper()), as_cases(oneway_subsets), run_oneway),
  # F < 1: equal true means, negative omega2 printed with its sign.
  anova_oneway_negative_omega2 = family(one(function() oneway_paper(oneway_raw(null = TRUE), negative_omega = TRUE)),
                                        as_cases(oneway_subsets), run_oneway, designs = 3L * n_designs),
  anova_factorial = family(one(factorial_paper), as_cases(factorial_subsets), run_factorial, extra = extra_factorial),
  anova_factorial_2x2 = family(one(function() factorial_paper(factorial_raw(c(2L, 2L)))),
                               as_cases(factorial_subsets), run_factorial, extra = extra_factorial),
  chisq_independence = family(one(chisq_independence_paper), chisq_cases, run_chisq, extra = extra_chisq),
  chisq_independence_2x2 = family(one(function() chisq_independence_paper(force_2x2 = TRUE)), chisq_cases, run_chisq, extra = extra_chisq),
  chisq_gof = family(one(chisq_gof_paper), chisq_cases, run_chisq, extra = extra_chisq),
  chisq_gof_w_gt_1 = family(one(function() chisq_gof_paper(high_w = TRUE, k = 4L)), chisq_cases, run_chisq, extra = extra_chisq),
  lm_simple = lm_fam("lm_simple", weighted = FALSE),
  lm_simple_weighted = lm_fam("lm_simple", weighted = TRUE),
  lm_multiple = lm_fam("lm_multiple", weighted = FALSE, with_factor = FALSE),
  lm_multiple_factor = lm_fam("lm_multiple", weighted = FALSE, with_factor = TRUE),
  lm_multiple_weighted = lm_fam("lm_multiple", weighted = TRUE),
  lm_moderation = lm_fam("lm_moderation", weighted = FALSE),
  lm_moderation_weighted = lm_fam("lm_moderation", weighted = TRUE),
  mediation_small = family(med_make("small"), mediation_cases, run_mediation, bad_mediation),
  mediation_large = family(med_make("large"), mediation_cases, run_mediation, bad_mediation),
  mediation_mixed = family(med_make("mixed"), mediation_cases, run_mediation, bad_mediation),
  # Welch df printed with no decimals (a paper that rounds the df to a whole number)
  t_welch_df_0_decimals = family(one(function() independent_paper(FALSE, df_dec = 0L)), as_cases(generic_subsets),
                                 run_t("welch"), extra = extra_welch_df0),
  # integer data, printed half-even and half-up
  likert_student = family(modes(function() likert_independent_paper(TRUE)), as_cases(generic_subsets),
                          run_t("student"), extra = extra_t, designs = 150L),
  likert_welch = family(modes(function() likert_independent_paper(FALSE)), as_cases(generic_subsets),
                        run_t("welch"), extra = extra_t, designs = 150L),
  likert_welch_df_0_decimals = family(modes(function() likert_independent_paper(FALSE, df_dec = 0L)), as_cases(generic_subsets),
                                      run_t("welch"), extra = extra_welch_df0, designs = 150L),
  likert_one_sample = family(modes(likert_one_sample_paper), as_cases(generic_subsets),
                             run_t("one_sample"), extra = extra_t, designs = 150L),
  likert_paired = family(modes(likert_paired_paper), as_cases(paired_subsets), run_t("paired"),
                         extra = extra_t, designs = 90L),
  likert_pearson = family(modes(likert_pearson_paper), as_cases(pearson_subsets), run_t("pearson"),
                          extra = extra_t, designs = 150L),
  # one-sided papers, the tail stated by the caller
  one_sided_paired = family(one(function() paired_paper(paired_design(), random_tail())),
                            one_sided_cases(paired_subsets), run_t("paired"), extra = extra_one_sided, designs = 150L),
  one_sided_pearson = family(one(function() pearson_paper(alternative = random_tail())),
                             one_sided_cases(pearson_subsets), run_t("pearson"), extra = extra_one_sided, designs = 150L),
  one_sided_student = family(one(function() independent_paper(TRUE, alternative = random_tail())),
                             one_sided_cases(generic_subsets), run_t("student"), extra = extra_one_sided, designs = 150L),
  one_sided_welch = family(one(function() independent_paper(FALSE, alternative = random_tail())),
                           one_sided_cases(generic_subsets), run_t("welch"), extra = extra_one_sided, designs = 150L),
  one_sided_one_sample = family(one(function() one_sample_paper(alternative = random_tail())),
                                one_sided_cases(generic_subsets), run_t("one_sample"), extra = extra_one_sided, designs = 150L),
  one_sided_likert_student = family(modes(function() likert_independent_paper(TRUE, random_tail())),
                                    one_sided_cases(generic_subsets), run_t("student"), extra = extra_one_sided, designs = 100L),
  one_sided_likert_one_sample = family(modes(function() likert_one_sample_paper(random_tail())),
                                       one_sided_cases(generic_subsets), run_t("one_sample"), extra = extra_one_sided, designs = 100L),
  # regression with a small n (10 to 19)
  lm_simple_small_n = family(one(function() lm_paper(lm_raw("lm_simple", weighted = FALSE, n = sample(10:19, 1L)))),
                             lm_cases, run_lm, designs = 150L),
  lm_multiple_small_n = family(one(function() lm_paper(lm_raw("lm_multiple", weighted = FALSE, with_factor = FALSE, n = sample(10:19, 1L)))),
                               lm_cases, run_lm, designs = 150L),
  lm_moderation_small_n = family(one(function() lm_paper(lm_raw("lm_moderation", weighted = FALSE, n = sample(10:19, 1L)))),
                                 lm_cases, run_lm, designs = 150L)
)

# One chunk of designs of one family ------------------------------------------------------
add_counts <- function(into, counts) {
  for (nm in names(counts)) into[nm] <- (if (is.na(into[nm])) 0L else into[nm]) + counts[[nm]]
  into
}
run_chunk <- function(task) {
  fam <- families[[task$group]]
  tot <- list(designs = 0L, checks = 0L, checked = 0L, first_checked = 0L, ties = 0L,
              extra = integer(), status = integer(), fp = list(), err = list())
  for (i in seq_len(task$n)) {
    papers <- fam$make()
    if (isTRUE(attr(papers, "tie"))) tot$ties <- tot$ties + 1L
    for (paper in papers) {
      tot$designs <- tot$designs + 1L
      if (!is.null(fam$extra)) {
        ex <- fam$extra(paper)
        tot$extra <- add_counts(tot$extra, as.list(stats::setNames(as.integer(ex), names(ex))))
      }
      cases <- fam$cases(paper, task$offset + i)
      for (j in seq_along(cases)) {
        case <- cases[[j]]
        res <- fam$run(paper, case)
        tot$checks <- tot$checks + 1L
        if (inherits(res, "error")) {
          tot$err[[length(tot$err) + 1L]] <- list(msg = conditionMessage(res), keys = case[["keys"]], seed = task$seed_note)
          next
        }
        tot$checked <- tot$checked + isTRUE(res$checked)
        if (j == 1L && is.null(attr(paper, "variant"))) tot$first_checked <- tot$first_checked + isTRUE(res$checked)
        tot$status <- add_counts(tot$status, as.list(table(res$numbers$status)))
        if (fam$bad(res, paper, case)) {
          tot$fp[[length(tot$fp) + 1L]] <- list(keys = case[["keys"]], variants = case[["variants"]],
                                               paper = paper[case[["keys"]]], chunk = task$id)
        }
      }
    }
  }
  tot
}

tasks <- list()
for (g in names(families)) {
  offsets <- seq(0L, families[[g]]$designs - 1L, by = chunk)
  for (o in offsets) {
    tasks[[length(tasks) + 1L]] <- list(group = g, offset = o, n = min(chunk, families[[g]]$designs - o),
                                       id = paste0(g, "#", o / chunk + 1L))
  }
}
seed <- 20261123
smoke <- nzchar(Sys.getenv("REPROKIT_VALIDATION_SMOKE"))  # one chunk per group, to test the script itself
if (smoke) tasks <- tasks[!duplicated(vapply(tasks, function(t) t$group, ""))]
cat("tasks:", length(tasks), "chunks of up to", chunk, "designs; cores:", par_cores(), "\n")
chunks <- par_run(tasks, run_chunk, seed)

results <- lapply(names(families), function(g) {
  mine <- chunks[vapply(tasks, function(t) t$group == g, logical(1))]
  if (smoke && length(mine) == 0L) return(NULL)
  out <- list(designs = 0L, checks = 0L, checked = 0L, first_checked = 0L, ties = 0L,
              extra = integer(), status = integer(), fp = list(), err = list())
  for (m in mine) {
    for (nm in c("designs", "checks", "checked", "first_checked", "ties")) out[[nm]] <- out[[nm]] + m[[nm]]
    out$extra <- add_counts(out$extra, as.list(m$extra))
    out$status <- add_counts(out$status, as.list(m$status))
    out$fp <- c(out$fp, m$fp)
    out$err <- c(out$err, m$err)
  }
  out
})
names(results) <- names(families)

# Minimum checked share per group: the share observed in the recorded run minus 5 percentage
# points, rounded down to a multiple of 5 points. It guards against a relation silently
# dropping out of a handler (a group that checks nothing proves nothing).
min_checked_share <- c(
  t_paired = 0.60,
  t_paired_d = 0.30,
  cor_pearson = 0.70,
  t_independent_student = 0.75,
  t_independent_welch = 0.75,
  t_one_sample = 0.90,
  anova_oneway = 0.75,
  anova_oneway_negative_omega2 = 0.75,
  anova_factorial = 0.85,
  anova_factorial_2x2 = 0.90,
  chisq_independence = 0.70,
  chisq_independence_2x2 = 0.70,
  chisq_gof = 0.70,
  chisq_gof_w_gt_1 = 0.70,
  lm_simple = 0.80,
  lm_simple_weighted = 0.80,
  lm_multiple = 0.75,
  lm_multiple_factor = 0.75,
  lm_multiple_weighted = 0.75,
  lm_moderation = 0.75,
  lm_moderation_weighted = 0.75,
  mediation_small = 0.75,
  mediation_large = 0.75,
  mediation_mixed = 0.75,
  t_welch_df_0_decimals = 0.75,
  likert_student = 0.75,
  likert_welch = 0.75,
  likert_welch_df_0_decimals = 0.75,
  likert_one_sample = 0.90,
  likert_paired = 0.60,
  likert_pearson = 0.75,
  one_sided_paired = 0.60,
  one_sided_pearson = 0.75,
  one_sided_student = 0.75,
  one_sided_welch = 0.75,
  one_sided_one_sample = 0.90,
  one_sided_likert_student = 0.75,
  one_sided_likert_one_sample = 0.90,
  lm_simple_small_n = 0.80,
  lm_multiple_small_n = 0.75,
  lm_moderation_small_n = 0.75)

share <- vapply(results, function(r) r$checked / r$checks, 0)
minimum <- if (calibrate) stats::setNames(rep(0, length(share)), names(share)) else
  min_checked_share[names(share)]
below <- is.na(minimum) | share < minimum

extra_cols <- c("|r|>=.995", "n<=6", "sd 0 dec", "yates", "w>1", "unbalanced", "df 0 dec", "one-sided")
table_out <- do.call(rbind, lapply(names(results), function(h) {
  r <- results[[h]]
  data.frame(handler = h, designs = r$designs, checks = r$checks,
             `checked %` = round(100 * share[[h]], 1), `min %` = round(100 * minimum[[h]], 1),
             ties = r$ties,
             as.data.frame(lapply(stats::setNames(extra_cols, extra_cols), function(e)
               if (is.na(r$extra[e])) 0L else unname(r$extra[e])), check.names = FALSE),
             false_positives = length(r$fp), errors = length(r$err), check.names = FALSE)
}))
print(table_out, row.names = FALSE)

cat("\nStatus of every reported number, per handler/variant:\n")
status_names <- sort(unique(unlist(lapply(results, function(r) names(r$status)))))
status_table <- do.call(rbind, lapply(names(results), function(h) {
  v <- results[[h]]$status
  data.frame(handler = h, as.data.frame(lapply(stats::setNames(status_names, status_names), function(s)
    if (is.na(v[s])) 0L else unname(v[s])), check.names = FALSE), check.names = FALSE)
}))
print(status_table, row.names = FALSE)

for (h in names(results)) {
  for (f in utils::head(results[[h]]$fp, 3L)) {
    cat("\nFALSE POSITIVE", h, "(chunk", f$chunk, "of seed", seed, "L'Ecuyer-CMRG):",
        if (!is.null(f$variants)) paste("variants =", f$variants), "\n")
    str(f$paper)
  }
  for (f in utils::head(results[[h]]$err, 3L)) cat("\nERROR", h, ":", f$msg, "\n")
}

fp_total <- sum(table_out$false_positives)
err_total <- sum(table_out$errors)
# the rounded-product mediation papers have no all-numbers case: compare with the designs that do
first_ok <- vapply(names(results), function(h) {
  r <- results[[h]]
  want <- if (grepl("^mediation", h)) r$designs / 2L else r$designs
  # t_paired_d's first case is a d-only subset (d is not checkable), so only its share is gated
  h == "t_paired_d" || r$first_checked == want
}, logical(1))
# likert_pearson has no tie gate: a Pearson r of integer data is not a mean of integers, so an exact
# half in its printed decimals does not occur (0 met in the calibration run); every other Likert
# group must meet at least one exact tie.
tie_groups <- setdiff(grep("^(one_sided_)?likert", names(results), value = TRUE), "likert_pearson")
ties_ok <- all(vapply(results[tie_groups], function(r) r$ties > 0L, logical(1)))
w1_count <- results$chisq_gof_w_gt_1$extra[["w>1"]]
pass <- fp_total == 0L && err_total == 0L && !any(below) && all(first_ok) && ties_ok &&
  w1_count > 0L && results$anova_oneway_negative_omega2$designs > 0L
cat("\none-way designs with F < 1 and a negative omega2 checked and not flagged:",
    results$anova_oneway_negative_omega2$designs, "\n")
cat("goodness-of-fit designs with w > 1 checked and not flagged:", w1_count, "\n")
cat("integer-data draws printed half-even and half-up that met an exact tie (the two printings differ):\n")
print(vapply(results[tie_groups], function(r) r$ties, 0L))
cat("not tested: one-sided papers for oneway, factorial, chisq, gof (upper-tail tests, no tail to",
    "state), lm and mediation (no tail variant)\n")
if (any(below)) cat("checked share below its minimum (or no minimum stated):", names(share)[below], "\n")
if (!all(first_ok)) cat("all-numbers subset not checked in every design:", names(first_ok)[!first_ok], "\n")
cat("runtime (min):", round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1), "\n")
cat(if (pass) "\nPASS" else "\nFAIL", ": false positives = ", fp_total, ", errors = ", err_total,
    ", groups below their checked-share minimum = ", sum(below), "\n", sep = "")

finish_validation(pass)
