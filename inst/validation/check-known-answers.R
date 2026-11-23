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
# Run: Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-known-answers.R")'

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
