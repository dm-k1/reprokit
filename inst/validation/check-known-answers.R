# Known answers: cases whose correct verdict is known a priori, not from our code.
#
# Case 1 (POSITIVE FIXTURE): APA Publication Manual 7th ed., Table 7.8, row 2.
#   9-year-olds M = 759, SD = 87, n = 24; 16-year-olds M = 694, SD = 42, n = 18;
#   reported t(40) = 2.877, p = .006, Cohen's d = 0.840. Student df = 24 + 18 - 2 = 40.
#   The table is correctly reported as far as t, df and p go and must never be
#   flagged inconsistent. That is what belongs to the false-positive gate.
#
#   WARNING, do not reinstate: an earlier version of this fixture used n = 16 for
#   the 16-year-olds, read from a low-resolution render (the page re-read at 400 dpi
#   says n = 18). With n = 16, Student df = 38, so t(40) looked impossible and the
#   fixture asserted the table WAS inconsistent. It encoded a misreading, and it
#   would have made the checker's own test demand a false positive on a correct
#   table. Never assert "flagged" for this table.
#
#   Claims shown below as recomputed ranges over the rounding box of the table's own
#   descriptives (M and SD to the unit, n exact), beside the reported values:
#   1. t and df are inside the Student ranges and p from t at df 40 rounds to .006:
#      none of them is inconsistent.
#   2. The note says the tests assume unequal variance, but the reported t and df fit
#      Student and not Welch. The variant that fits is not the variant claimed.
#   3. Cohen's d = 0.840 is outside the range implied by the descriptives. The
#      convention for d is not declared (the manual requires none), so the value is
#      unfalsifiable, not wrong.
#   4. The same numbers through check_reported("t_independent"): consistent overall,
#      Student fits and Welch does not, no row is "inconsistent", and d gets the status
#      "not reproducible" (no effect-size convention reproduces it).
#   PASS: claims 1 to 4 hold (printed as TRUE).
#
# Case 2 (POSITIVE FIXTURE): the sleep data (Student 1908, as shipped in R's datasets and
#   printed in ?t.test): Welch t(17.776) = -1.8608, p = .07939, 95% CI [-3.365, 0.205]; the
#   paired test t(9) = -4.0621, p = .002833, 95% CI [-2.460, -0.700], d_z = -1.28 (mean
#   difference -1.58 over SD of the differences 1.2299). Claims, with CIs computed by stats
#   (not by reprokit) at 90, 95 and 99%:
#   5. each CI at its level fits that level and is not inconsistent (independent and paired);
#   6. the paired d_z = -1.28 verifies, and d = +1.28 (wrong sign) is "not reproducible",
#      never inconsistent.
#   PASS: claims 5 and 6 hold.
#
# Case 4 (POSITIVE FIXTURE, chi-squared, ADR 0016): R's HairEyeColor (Hair x Eye, 4 x 4, N = 592),
#   UCBAdmissions (Admit x Gender, 2 x 2, N = 4526, with and without Yates' correction) and
#   table(mtcars$cyl) (goodness of fit, uniform and with p0 = .3, .2, .5). chi-squared, df, p
#   and V (w) are taken from stats::chisq.test() and printed as a paper does. Claims:
#   7. every correct report (all numbers, with the table) is consistent and no row inconsistent;
#   8. the 2 x 2 report fits the test variant that produced it and not the other (a Yates
#      chi-squared is not explained by Pearson, and the reverse); the 4 x 4 table has no variant;
#   9. a wrong df (the 4 x 4 table with df = 12, the 2 x 2 table with df = 2) is flagged.
#   PASS: claims 7 to 9 hold.
#
# Case 3 (POSITIVE FIXTURE, one-way ANOVA): R's PlantGrowth (3 groups of 10),
#   reported rounded as a paper prints it (M and SD 2 dp, F 2 dp, p 3 dp, eta2 2 dp), with
#   the true F and eta2 computed by stats::aov from the raw data (not by reprokit). Claims:
#   7. the true F and eta2 lie inside the ranges computed from the rounded M and SD;
#   8. check_F() on the rounded APA report (with descriptives) is consistent and no number
#      is inconsistent; the same with only f, df1, df2, p;
#   9. a planted wrong df2 (29, N instead of N - k) is flagged.
#   PASS: claims 7 to 9 hold.
#
# Case 5 (POSITIVE FIXTURE, factorial ANOVA, ADR 0017): two between-subjects designs.
#   ToothGrowth supp x dose (2 x 3, balanced, n = 10 per cell; dose as a factor) and mtcars
#   am x vs (2 x 2, cells 12, 6, 7, 7: unbalanced). The expected F, df, p are NOT taken from
#   reprokit: the ToothGrowth values are the widely published two-way table (supp F = 15.572,
#   dose F = 92.000, supp x dose F = 4.107, df2 = 54) and the mtcars values are computed here
#   with stats::anova() (type I, both entry orders) and stats::drop1() (types II and III, the
#   latter with sum contrasts); car::Anova() is compared when car is installed. Claims:
#   10. ToothGrowth: every effect's F, df1, df2, p and partial eta2 from reproduce() match the
#       published/aov values, in all three SS types (one balanced design: all types agree);
#   11. mtcars: reproduce() at each SS type equals anova()/drop1() to 1e-8 scaled, for F and p
#       of both main effects, and the three types differ for the main effects (the SS type is a
#       real choice) while the interaction is the same in all;
#   12. check_F() on the correct rounded report of each effect is consistent with no row
#       inconsistent, and for mtcars it identifies the type that produced it (and only it);
#   13. a planted wrong df2 (N - 1 instead of N - cells) is flagged.
#   PASS: claims 10 to 13 hold.
# Run: Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-known-answers.R")'
#
# Case 6 (POSITIVE FIXTURE, regression family, ADR 0020): mtcars (n = 32). The expected values
#   are NOT taken from reprokit: they are summary(lm()) on the raw data, for simple (mpg ~ wt),
#   multiple (mpg ~ wt + hp + factor(am)) and moderation (mpg ~ wt * hp), and for a simple
#   mediation (X = wt, M = hp, Y = mpg) the four paths from three separate lm() fits (M ~ X,
#   Y ~ X, Y ~ X + M). Claims:
#   12. reproduce() of the stored sufficient statistics equals summary(lm) for every b, SE, t,
#       p, R2, adjusted R2 and F of the three lm rows, and pf() of the reproduced F, df1, df2
#       equals the model p (1e-8 scaled);
#   13. the mediation paths a, b, c and c' (b, SE, t, p, df) equal the three lm() fits;
#   14. the table of each correct report, printed to the decimals a paper uses, is consistent
#       and has no inconsistent row;
#   15. mediation: a*b and its CI are "not checkable" even when they are exactly right, and an
#       absurd a*b changes nothing.
#   PASS: claims 12 to 15 hold.

n1 <- 24
n2 <- 18
rep_t <- 2.877
rep_d <- 0.840
rep_df <- 40
row2 <- list(m = c("759", "694"), sd = c("87", "42"), t = "2.877", p = ".006")

# Rounding box: M and SD reported to the unit, so each is +/- 0.5. Student t, Welch t
# and d are monotone in each SD and in the mean difference, so corners are exact; the
# Welch df is not monotone in the SDs, so it is scanned on a grid.
box <- expand.grid(m1 = 759 + c(-.5, .5), m2 = 694 + c(-.5, .5),
                   s1 = 87 + c(-.5, .5), s2 = 42 + c(-.5, .5))
student_t <- function(m1, m2, s1, s2) {
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  (m1 - m2) / (sp * sqrt(1 / n1 + 1 / n2))
}
welch_t <- function(m1, m2, s1, s2) (m1 - m2) / sqrt(s1^2 / n1 + s2^2 / n2)
welch_df <- function(s1, s2) {
  se4 <- (s1^2 / n1 + s2^2 / n2)^2
  se4 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
}
pooled_d <- function(m1, m2, s1, s2) {
  (m1 - m2) / sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
}
rng <- function(f) range(do.call(f, unname(as.list(box))))
t_student <- rng(student_t)
t_welch <- rng(welch_t)
d_pooled <- rng(pooled_d)
grid <- expand.grid(s1 = seq(86.5, 87.5, length.out = 21L),
                    s2 = seq(41.5, 42.5, length.out = 21L))
df_welch <- range(welch_df(grid$s1, grid$s2))
df_student <- n1 + n2 - 2
p_at <- function(df) 2 * pt(-c(2.8765, 2.8775), df)
p40 <- p_at(40)
p_rounds_to <- unique(formatC(p40, format = "f", digits = 3L))

inside <- function(x, r) x >= r[1L] - 1e-9 && x <= r[2L] + 1e-9
q_names <- c("t, Student", "t, Welch", "df, Student", "df, Welch", "Cohen's d (pooled SD)",
             "p from t = 2.877 at df 40")
q_reported <- c(rep_t, rep_t, rep_df, rep_df, rep_d, 0.006)
q_lower <- c(t_student[1L], t_welch[1L], df_student, df_welch[1L], d_pooled[1L], p40[1L])
q_upper <- c(t_student[2L], t_welch[2L], df_student, df_welch[2L], d_pooled[2L], p40[2L])
ranges <- data.frame(quantity = q_names, reported = q_reported,
                     range_lower = q_lower, range_upper = q_upper)
ranges$reported_inside <- c(inside(rep_t, t_student), inside(rep_t, t_welch),
                            inside(rep_df, c(df_student, df_student)),
                            inside(rep_df, df_welch), inside(rep_d, d_pooled),
                            all(p_rounds_to == "0.006"))
ranges[, 2:4] <- round(ranges[, 2:4], 3L)
cat("== Table 7.8, row 2: recomputed ranges beside the reported values ==\n")
cat("n = ", n1, " and ", n2, "; M and SD to the unit (box +/- 0.5);\n",
    "p range is from t's interval 2.8765 to 2.8775\n", sep = "")
print(ranges, row.names = FALSE)

cat("\n== The engine: check_reported(\"t_independent\", ...) ==\n")
res_ind <- check_reported("t_independent", n = c("24", "18"), m = row2$m, sd = row2$sd,
                          t = row2$t, df = "40", p = row2$p, d = "0.840")
print(res_ind)
fits <- function(kind, variant) {
  v <- res_ind$variants[[kind]]
  v$fits[v$variant == variant]
}

checks <- c(res_ind$consistent,
            !any(res_ind$numbers$status == "inconsistent"),
            fits("test", "student"),
            !fits("test", "welch"),
            res_ind$numbers$status[res_ind$numbers$quantity == "d"] == "not reproducible",
            inside(rep_t, t_student),
            rep_df == df_student,
            identical(p_rounds_to, "0.006"),
            !inside(rep_t, t_welch),
            !inside(rep_df, df_welch),
            !inside(rep_d, d_pooled))
labels <- c("engine: overall consistent",
            "engine: no number inconsistent",
            "engine: Student fits",
            "engine: Welch does not fit",
            "engine: d is not reproducible (not inconsistent)",
            "reported t inside the Student range",
            "reported df equals Student df (n1 + n2 - 2)",
            "p from t at df 40 rounds to .006",
            "fits Student, not Welch (t outside the Welch range)",
            "fits Student, not Welch (df outside the Welch range)",
            "Cohen's d outside the pooled-d range (undeclared convention, not an error)")
names(checks) <- labels
cat("\n== Assertions ==\n")
print(data.frame(assertion = names(checks), holds = unname(checks)), row.names = FALSE)
cat("\nCohen's d = 0.840 is outside the range the descriptives imply (",
    format(round(d_pooled[1L], 3L), nsmall = 3L), " to ",
    format(round(d_pooled[2L], 3L), nsmall = 3L),
    "). The convention for d is not declared and the manual requires none, so this is\n",
    "an unfalsifiable value, not an inconsistency. The note's claim of unequal variance\n",
    "does not match the numbers, which fit Student.\n", sep = "")
if (all(checks)) {
  cat("\nPASS: Table 7.8 row 2 is not flagged inconsistent; Student fits, Welch does not;\n",
      "d is not reproducible.\n", sep = "")
} else {
  cat("\nFAIL: see the assertions that do not hold.\n")
}

# Case 2: sleep data, CIs at three levels and the paired d --------------------------------
g1 <- datasets::sleep$extra[datasets::sleep$group == 1]
g2 <- datasets::sleep$extra[datasets::sleep$group == 2]
two <- function(x) formatC(x, format = "f", digits = 2L)
level_rows <- do.call(rbind, lapply(c(0.90, 0.95, 0.99), function(level) {
  w <- t.test(g1, g2, conf.level = level)
  pr <- t.test(g1, g2, paired = TRUE, conf.level = level)
  ind <- check_reported("t_independent", n = c("10", "10"), m = two(c(mean(g1), mean(g2))),
                        sd = two(c(sd(g1), sd(g2))), ci_lower = two(w$conf.int[1L]),
                        ci_upper = two(w$conf.int[2L]))
  pair <- check_reported("t_paired", n = "10", m = two(c(mean(g1), mean(g2))),
                         sd = two(c(sd(g1), sd(g2))), r = two(cor(g1, g2)),
                         ci_lower = two(pr$conf.int[1L]), ci_upper = two(pr$conf.int[2L]))
  want <- sprintf("%.2f", level)
  fit <- function(x) x$variants$level$variant[x$variants$level$fits]
  data.frame(level = want,
             independent_fits = want %in% fit(ind), independent_consistent = ind$consistent,
             paired_fits = want %in% fit(pair), paired_consistent = pair$consistent)
}))
cat("\n== Case 2: sleep data, CI at its own level ==\n")
print(level_rows, row.names = FALSE)
d_args <- list("t_paired", n = "10", m = two(c(mean(g1), mean(g2))),
               sd = two(c(sd(g1), sd(g2))), r = two(cor(g1, g2)))
d_z <- mean(g1 - g2) / sd(g1 - g2)
d_good <- do.call(check_reported, c(d_args, list(d = two(d_z))))
d_bad <- do.call(check_reported, c(d_args, list(d = two(-d_z))))
status <- function(x) x$numbers$status[x$numbers$quantity == "d"]
cat("paired d_z =", two(d_z), "-> status", status(d_good), "; wrong sign ->", status(d_bad), "\n")
checks2 <- c(all(level_rows$independent_fits), all(level_rows$independent_consistent),
             all(level_rows$paired_fits), all(level_rows$paired_consistent),
             identical(two(d_z), "-1.28"), status(d_good) == "verified",
             status(d_bad) == "not reproducible", d_bad$consistent)
cat(if (all(checks2)) "\nPASS" else "\nFAIL",
    ": sleep CIs fit their levels (", sum(checks2[1:4]), "/4 groups of claims), paired d ",
    "verified and sign-flipped d not reproducible (", sum(checks2[5:8]), "/4)\n", sep = "")

# Case 3: PlantGrowth one-way ANOVA -------------------------------------------------------
pg <- datasets::PlantGrowth
fit <- summary(aov(weight ~ group, data = pg))[[1L]]
true_f <- fit[["F value"]][1L]
true_eta2 <- fit[["Sum Sq"]][1L] / sum(fit[["Sum Sq"]])
pg_m <- tapply(pg$weight, pg$group, mean)
pg_sd <- tapply(pg$weight, pg$group, sd)
paper <- list(n = c("10", "10", "10"), m = two(pg_m), sd = two(pg_sd),
              f = two(true_f), df1 = "2", df2 = "27",
              p = formatC(fit[["Pr(>F)"]][1L], format = "f", digits = 3L), eta2 = two(true_eta2))
pg_box <- list(n = list(lower = rep(10, 3), upper = rep(10, 3)),
               m = list(lower = as.numeric(two(pg_m)) - 0.005,
                        upper = as.numeric(two(pg_m)) + 0.005),
               sd = list(lower = as.numeric(two(pg_sd)) - 0.005,
                         upper = as.numeric(two(pg_sd)) + 0.005))
f_range <- relation_range(get_handler("anova_oneway")$relations[[3L]], pg_box, NULL)
eta_range <- f_range * 2 / (f_range * 2 + 27)
full <- check_F(f = paper$f, df1 = paper$df1, df2 = paper$df2, p = paper$p, eta2 = paper$eta2,
                n = paper$n, m = paper$m, sd = paper$sd)
slim <- check_F(f = paper$f, df1 = paper$df1, df2 = paper$df2, p = paper$p)
bad <- check_F(f = paper$f, df1 = paper$df1, df2 = "29", p = paper$p)
cat("\n== Case 3: PlantGrowth one-way ANOVA ==\n")
print(data.frame(quantity = c("F", "eta2"), true = round(c(true_f, true_eta2), 4L),
                 range_lower = round(c(f_range[[1L]], eta_range[[1L]]), 4L),
                 range_upper = round(c(f_range[[2L]], eta_range[[2L]]), 4L)), row.names = FALSE)
checks3 <- c(true_f >= f_range[[1L]] && true_f <= f_range[[2L]],
             true_eta2 >= eta_range[[1L]] && true_eta2 <= eta_range[[2L]],
             full$consistent, !any(full$numbers$status == "inconsistent"),
             slim$consistent, !bad$consistent)
names(checks3) <- c("true F inside range", "true eta2 inside range", "full report consistent",
                    "no number inconsistent", "f/df/p only is consistent",
                    "planted df2 = 29 flagged")
print(data.frame(assertion = names(checks3), holds = unname(checks3)), row.names = FALSE)
cat(if (all(checks3)) "\nPASS" else "\nFAIL",
    ": PlantGrowth F and eta2 inside their ranges, correct report consistent, ",
    "planted df2 flagged (", sum(checks3), "/6)\n", sep = "")

# Case 4: chi-squared -----------------------------------------------------------------------
pc <- function(p) if (p < 0.001) "< .001" else formatC(p, format = "f", digits = 3L)
vtxt <- function(x) sub("^0\\.", ".", two(x))
chisq_paper <- function(tab, correct = TRUE, p0 = NULL) {
  gof <- is.null(dim(tab)) || length(dim(tab)) == 1L
  r <- if (gof) suppressWarnings(chisq.test(tab, p = if (is.null(p0)) rep(1 / length(tab), length(tab)) else p0))
       else suppressWarnings(chisq.test(tab, correct = correct))
  pearson <- if (gof) r$statistic else suppressWarnings(chisq.test(tab, correct = FALSE))$statistic
  v <- sqrt(pearson / (sum(tab) * (if (gof) 1 else min(dim(tab)) - 1)))
  list(n = if (gof) as.character(as.vector(tab)) else matrix(as.character(tab), nrow(tab)),
       chisq = two(unname(r$statistic)), df = as.character(unname(r$parameter)),
       p = pc(r$p.value), v = vtxt(unname(v)))
}
fixtures <- list(
  list(name = "HairEyeColor Hair x Eye", tab = unclass(margin.table(HairEyeColor, c(1, 2))),
       correct = FALSE, wrong_df = "12"),
  list(name = "UCBAdmissions Admit x Gender, Yates", tab = unclass(margin.table(UCBAdmissions, c(1, 2))),
       correct = TRUE, wrong_df = "2"),
  list(name = "UCBAdmissions Admit x Gender, Pearson", tab = unclass(margin.table(UCBAdmissions, c(1, 2))),
       correct = FALSE, wrong_df = "2"),
  list(name = "cyl uniform", tab = table(mtcars$cyl), correct = NA, wrong_df = "3"),
  list(name = "cyl p0 = .3/.2/.5", tab = table(mtcars$cyl), correct = NA, wrong_df = "3",
       p0 = c(0.3, 0.2, 0.5)))
rows4 <- do.call(rbind, lapply(fixtures, function(f) {
  paper <- chisq_paper(f$tab, isTRUE(f$correct), f$p0)
  gof <- is.null(dim(paper$n))
  run <- function(p, ...) do.call(check_chisq, c(p, list(p0 = f$p0), list(...)))
  ok <- run(paper)
  bad <- run(replace(paper, "df", f$wrong_df), dims = if (gof) length(f$tab) else dim(f$tab))
  fits <- if (!is.null(ok$variants$test)) setNames(ok$variants$test$fits, ok$variants$test$variant)
  expect_variant <- if (is.na(f$correct)) NA else if (!is.null(fits)) {
    fits[[if (f$correct) "yates" else "pearson"]] && !fits[[if (f$correct) "pearson" else "yates"]]
  } else {
    !(f$correct) || FALSE
  }
  data.frame(case = f$name, chisq = paper$chisq, df = paper$df, p = paper$p, v = paper$v,
             consistent = ok$consistent, none_inconsistent = !any(ok$numbers$status == "inconsistent"),
             variant_right = expect_variant, wrong_df_flagged = !bad$consistent)
}))
cat("\n== Case 4: chi-squared from stats::chisq.test() ==\n")
print(rows4, row.names = FALSE)
checks4 <- c(all(rows4$consistent), all(rows4$none_inconsistent),
             all(rows4$variant_right, na.rm = TRUE), all(rows4$wrong_df_flagged))
cat(if (all(checks4)) "\nPASS" else "\nFAIL",
    ": correct chi-squared reports consistent, test variant identified, wrong df flagged (",
    sum(checks4), "/4)\n", sep = "")

# Case 5: factorial ANOVA -------------------------------------------------------------------
scaled_close <- function(a, b) all(abs(a - b) / pmax(1, abs(b)) < 1e-8)
tg <- datasets::ToothGrowth
tg$dose <- factor(tg$dose)
tg_fit <- aov(len ~ supp * dose, data = tg)
tg_tab <- summary(tg_fit)[[1L]]
published <- c(f_A = 15.572, f_B = 92.000, f_AB = 4.107)
tg_rep <- lapply(ss_type_codes(), function(type) {
  reproduce(sufficient_stats(len ~ supp * dose, data = tg, test = "factorial", ss_type = type,
                             sharing_reason = "validation"))
})
tg_ok <- vapply(tg_rep, function(r) {
  all(abs(r[c("f_A", "f_B", "f_AB")] - published) < 5e-4) &&
    scaled_close(r[c("f_A", "f_B", "f_AB")], tg_tab[["F value"]][1:3]) &&
    scaled_close(r[c("p_A", "p_B", "p_AB")], tg_tab[["Pr(>F)"]][1:3]) &&
    identical(unname(r[c("df1_A", "df1_B", "df1_AB", "df2")]), c(1, 2, 2, 54)) &&
    scaled_close(r[c("eta2_A", "eta2_B", "eta2_AB")],
                 tg_tab[["Sum Sq"]][1:3] / (tg_tab[["Sum Sq"]][1:3] + tg_tab[["Sum Sq"]][4L]))
}, logical(1))

mt <- datasets::mtcars
mt$am <- factor(mt$am)
mt$vs <- factor(mt$vs)
base_ss <- function(type) {
  full <- lm(mpg ~ am * vs, mt)
  if (type == "I") {
    c(A = anova(full)["am", "Sum Sq"], B = anova(lm(mpg ~ vs * am, mt))["vs", "Sum Sq"],
      AB = anova(full)["am:vs", "Sum Sq"])
  } else if (type == "II") {
    d <- drop1(lm(mpg ~ am + vs, mt), scope = ~ am + vs)
    c(A = d["am", "Sum of Sq"], B = d["vs", "Sum of Sq"], AB = anova(full)["am:vs", "Sum Sq"])
  } else {
    d <- drop1(lm(mpg ~ am * vs, mt, contrasts = list(am = "contr.sum", vs = "contr.sum")),
               scope = ~ am + vs + am:vs)
    c(A = d["am", "Sum of Sq"], B = d["vs", "Sum of Sq"], AB = d["am:vs", "Sum of Sq"])
  }
}
mt_full <- lm(mpg ~ am * vs, mt)
sse <- deviance(mt_full)
df2 <- df.residual(mt_full)
mt_rows <- lapply(ss_type_codes(), function(type) {
  r <- reproduce(sufficient_stats(mpg ~ am * vs, data = mt, test = "factorial", ss_type = type,
                                  sharing_reason = "validation"))
  f <- base_ss(type) / (sse / df2)
  list(type = type, f_match = scaled_close(r[c("f_A", "f_B", "f_AB")], f),
       p_match = scaled_close(r[c("p_A", "p_B", "p_AB")], pf(f, 1, df2, lower.tail = FALSE)),
       car_match = if (type != "I" && requireNamespace("car", quietly = TRUE)) {
         ct <- suppressMessages(car::Anova(
           lm(mpg ~ am * vs, mt, contrasts = list(am = "contr.sum", vs = "contr.sum")),
           type = type))
         scaled_close(r[c("f_A", "f_B", "f_AB")], ct[c("am", "vs", "am:vs"), "F value"])
       } else NA, f = r[c("f_A", "f_B", "f_AB")])
})
differ <- function(i, j) abs(mt_rows[[i]]$f[["f_A"]] - mt_rows[[j]]$f[["f_A"]]) > 0.1
types_differ <- differ(1, 2) && differ(1, 3) && differ(2, 3)
same_ab <- scaled_close(mt_rows[[1L]]$f[["f_AB"]], mt_rows[[2L]]$f[["f_AB"]]) &&
  scaled_close(mt_rows[[1L]]$f[["f_AB"]], mt_rows[[3L]]$f[["f_AB"]])

fx <- function(x, d) formatC(x, format = "f", digits = d)
paper_for <- function(d, a, b, y, effect, type) {
  r <- anova_factorial_reported(as.formula(paste(y, "~", a, "*", b)), d, "factorial", ss_type = type)
  tab <- function(fun, digits) matrix(fx(as.vector(tapply(d[[y]], list(d[[a]], d[[b]]), fun)), digits),
                                      nlevels(d[[a]]))
  list(n = matrix(as.character(as.vector(table(d[[a]], d[[b]]))), nlevels(d[[a]])),
       m = tab(mean, 2L), sd = tab(sd, 2L), f = fx(r[[paste0("f_", effect)]], 2L),
       df1 = as.character(r[[paste0("df1_", effect)]]), df2 = as.character(r[["df2"]]),
       p = fx(r[[paste0("p_", effect)]], 3L), eta2 = fx(r[[paste0("eta2_", effect)]], 2L),
       effect = effect, dims = c(nlevels(d[[a]]), nlevels(d[[b]])))
}
fits_of <- function(x) {
  v <- x$variants$test
  if (is.null(v)) NULL else v$variant[v$fits]
}
tg_checks <- do.call(rbind, lapply(c("A", "B", "AB"), function(effect) {
  ok <- do.call(check_F, paper_for(tg, "supp", "dose", "len", effect, "III"))
  bad <- paper_for(tg, "supp", "dose", "len", effect, "III")
  bad$df2 <- "59"
  data.frame(design = "ToothGrowth", effect = effect, type = "-", consistent = ok$consistent,
             none_inconsistent = !any(ok$numbers$status == "inconsistent"),
             type_identified = is.null(fits_of(ok)),
             wrong_df2_flagged = !do.call(check_F, bad)$consistent)
}))
mt_checks <- do.call(rbind, lapply(ss_type_codes(), function(type) {
  do.call(rbind, lapply(c("A", "B", "AB"), function(effect) {
    paper <- paper_for(mt, "am", "vs", "mpg", effect, type)
    ok <- do.call(check_F, paper)
    bad <- paper
    bad$df2 <- "31"
    data.frame(design = "mtcars", effect = effect, type = type, consistent = ok$consistent,
               none_inconsistent = !any(ok$numbers$status == "inconsistent"),
               type_identified = if (effect == "AB") is.null(fits_of(ok)) else
                 identical(fits_of(ok), type),
               wrong_df2_flagged = !do.call(check_F, bad)$consistent)
  }))
}))
checks5_table <- rbind(tg_checks, mt_checks)
cat("\n== Case 5: factorial ANOVA ==\n")
print(data.frame(design = "ToothGrowth", type = ss_type_codes(), matches_published_and_aov = tg_ok),
      row.names = FALSE)
print(data.frame(design = "mtcars", type = ss_type_codes(),
                 F_matches_base_R = vapply(mt_rows, `[[`, logical(1), "f_match"),
                 p_matches_base_R = vapply(mt_rows, `[[`, logical(1), "p_match"),
                 matches_car = vapply(mt_rows, function(x) x$car_match, logical(1))),
      row.names = FALSE)
cat("(matches_car is NA for type I, which car::Anova() does not offer, and when car is not installed.)\n")
print(checks5_table, row.names = FALSE)
checks5 <- c(all(tg_ok), all(vapply(mt_rows, `[[`, logical(1), "f_match")),
             all(vapply(mt_rows, `[[`, logical(1), "p_match")),
             all(vapply(mt_rows, function(x) isTRUE(x$car_match) || is.na(x$car_match), logical(1))),
             types_differ, same_ab, all(checks5_table$consistent),
             all(checks5_table$none_inconsistent), all(checks5_table$type_identified),
             all(checks5_table$wrong_df2_flagged))
names(checks5) <- c("ToothGrowth matches published values and aov (all types)",
                    "mtcars F equals anova()/drop1() at each type",
                    "mtcars p equals anova()/drop1() at each type",
                    "mtcars equals car::Anova() when installed",
                    "mtcars main-effect F differs across the three types",
                    "mtcars interaction F is the same in every type",
                    "every correct report consistent", "no row inconsistent",
                    "SS type identified (mtcars) / no variant (balanced, interaction)",
                    "planted wrong df2 flagged")
print(data.frame(assertion = names(checks5), holds = unname(checks5)), row.names = FALSE)
cat(if (all(checks5)) "\nPASS" else "\nFAIL",
    ": factorial known answers (", sum(checks5), "/", length(checks5), ")\n", sep = "")

# Case 6: regression family ----------------------------------------------------------------
mt6 <- datasets::mtcars
mt6$am <- factor(mt6$am)
d2 <- function(x, d = 2L) formatC(x, format = "f", digits = d)
dp <- function(p) if (p < 0.001) "< .001" else sub("^0", "", d2(p, 3L))
scaled <- function(a, b) max(abs(a - b) / pmax(1, abs(a)))
reg_fits <- list(simple = lm(mpg ~ wt, mt6), multiple = lm(mpg ~ wt + hp + am, mt6),
                 moderation = lm(mpg ~ wt * hp, mt6))
reg_rows <- do.call(rbind, lapply(names(reg_fits), function(nm) {
  f <- reg_fits[[nm]]
  sm <- summary(f)
  s <- sufficient_stats(f, sharing_reason = "known answer")
  r <- reproduce(s)
  cf <- sm$coefficients
  terms <- rownames(cf)
  coef_diff <- max(vapply(seq_along(terms), function(i) {
    scaled(c(cf[i, 1L], cf[i, 2L], cf[i, 3L], cf[i, 4L]),
           unname(c(r[[paste0("estimate:", terms[i])]], r[[paste0("se:", terms[i])]],
                    r[[paste0("t:", terms[i])]], r[[paste0("p:", terms[i])]])))
  }, numeric(1)))
  model_diff <- scaled(c(sm$r.squared, sm$adj.r.squared, sm$fstatistic[["value"]]),
                       unname(c(r[["r.squared"]], r[["adj.r.squared"]], r[["F"]])))
  pm <- pf(sm$fstatistic[[1L]], sm$fstatistic[[2L]], sm$fstatistic[[3L]], lower.tail = FALSE)
  # a correct report of the non-intercept terms, printed as a paper does
  keep <- terms[terms != "(Intercept)"]
  rep <- list(b = setNames(d2(cf[keep, 1L]), keep), se = setNames(d2(cf[keep, 2L]), keep),
              t = setNames(d2(cf[keep, 3L]), keep),
              p = setNames(vapply(cf[keep, 4L], dp, ""), keep),
              r2 = sub("^0", "", d2(sm$r.squared)), f = d2(sm$fstatistic[["value"]]),
              df1 = as.character(sm$fstatistic[["numdf"]]),
              df2 = as.character(sm$fstatistic[["dendf"]]), n = as.character(nobs(f)))
  ck <- do.call(check_lm, c(rep, list(handler = paste0("lm_", nm))))
  data.frame(row = nm, coef_diff = signif(coef_diff, 3L), model_diff = signif(model_diff, 3L),
             p_model_diff = signif(abs(pm - pf(r[["F"]], r[["numdf"]], r[["dendf"]], lower.tail = FALSE)), 3L),
             report_consistent = ck$consistent,
             no_inconsistent_row = !any(ck$numbers$status == "inconsistent"))
}))
cat("
== Case 6: regression family against summary(lm) on mtcars ==
")
print(reg_rows, row.names = FALSE)

# mediation: X = wt, M = hp, Y = mpg
med_s <- sufficient_stats(mpg ~ wt, data = mt6, test = "mediation", mediator = "hp",
                          sharing_reason = "known answer")
med_r <- reproduce(med_s)
fa <- summary(lm(hp ~ wt, mt6))$coefficients
fc <- summary(lm(mpg ~ wt, mt6))$coefficients
fb <- summary(lm(mpg ~ wt + hp, mt6))$coefficients
med_expect <- list(a = c(fa["wt", ], df = 30), c = c(fc["wt", ], df = 30),
                   b = c(fb["hp", ], df = 29), c_prime = c(fb["wt", ], df = 29))
med_rows <- do.call(rbind, lapply(names(med_expect), function(path) {
  e <- med_expect[[path]]
  got <- unname(c(med_r[[paste0("estimate:", path)]], med_r[[paste0("se:", path)]],
                  med_r[[paste0("t:", path)]], med_r[[paste0("p:", path)]],
                  med_r[[paste0("df:", path)]]))
  data.frame(path = path, estimate = signif(got[1L], 5L), scaled_diff = signif(scaled(e, got), 3L))
}))
cat("
-- mediation paths against three lm() fits --
")
print(med_rows, row.names = FALSE)
med_ck <- check_mediation(
  b = c(a = d2(fa["wt", 1L]), b = d2(fb["hp", 1L]), c = d2(fc["wt", 1L]), c_prime = d2(fb["wt", 1L])),
  se = c(a = d2(fa["wt", 2L]), b = d2(fb["hp", 2L]), c = d2(fc["wt", 2L]), c_prime = d2(fb["wt", 2L])),
  t = c(a = d2(fa["wt", 3L]), b = d2(fb["hp", 3L]), c = d2(fc["wt", 3L]), c_prime = d2(fb["wt", 3L])),
  n = "32", indirect = d2(fa["wt", 1L] * fb["hp", 1L]),
  indirect_ci_lower = "-5.00", indirect_ci_upper = "-1.00")
med_odd <- check_mediation(b = c(a = "1.00"), se = c(a = "0.20"), t = c(a = "5.00"),
                           indirect = "99", indirect_ci_lower = "-7", indirect_ci_upper = "-6")
ind_status <- function(ck) ck$numbers$status[grepl("^indirect", ck$numbers$quantity)]
cat("
-- mediation report with a*b and a bootstrap CI --
")
print(med_ck$numbers, row.names = FALSE)

checks6 <- c(all(reg_rows$coef_diff < 1e-8), all(reg_rows$model_diff < 1e-8),
             all(reg_rows$p_model_diff < 1e-8),
             all(reg_rows$report_consistent), all(reg_rows$no_inconsistent_row),
             all(med_rows$scaled_diff < 1e-8), med_ck$consistent,
             all(ind_status(med_ck) == "not checkable"), all(ind_status(med_odd) == "not checkable"),
             med_odd$consistent)
names(checks6) <- c("b, SE, t, p equal summary(lm) (3 rows)", "R2, adjusted R2, F equal summary(lm)",
                    "model p equals pf()", "each correct report consistent",
                    "no inconsistent row in a correct report",
                    "mediation paths equal the three lm() fits", "mediation report consistent",
                    "a*b and its CI not checkable (right values)",
                    "a*b and its CI not checkable (absurd values)", "absurd a*b does not make it inconsistent")
cat("
")
print(data.frame(assertion = names(checks6), holds = unname(checks6)), row.names = FALSE)
cat(if (all(checks6)) "\nPASS" else "\nFAIL",
    ": regression known answers (", sum(checks6), "/", length(checks6), ")\n", sep = "")
