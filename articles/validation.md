# Validation

This vignette says what evidence backs the claims the package makes.
Every number quoted about the package comes from a script in
`inst/validation/`, which you can run yourself. Running them takes
minutes to an hour, so this vignette does not run them: it shows the
results each script wrote to `inst/validation/results/` when it last
ran, from the final tree of this release. Each result starts with a
header that gives the script, the seed, the package version, the date
and the R version, and ends with the script’s PASS or FAIL line.

To rerun a script, use a source checkout of the repository (GitHub:
`dm-k1/reprokit`). The scripts call internal functions, so from the
repository root run, for example:

``` r

devtools::load_all()
source("inst/validation/check-false-positives.R")
```

They do not run from the installed package. Each rerun overwrites its
results file.

## Coverage table (generated from the registry)

``` r

library(reprokit)
knitr::kable(coverage_table())
```

| test | apa | sufficient | missing | checkable | not_checkable | choices |
|:---|:---|:---|:---|:---|:---|:---|
| Pearson correlation test | n, M, SD per variable, r, df, p, CI | n, r | — | df, p, CI | M, SD per variable (r does not depend on them) | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%) |
| Paired-samples t-test | n, M, SD per condition, t, df, p, d, CI | n, M, SD per condition, r | r (the SD of the differences depends on the correlation between conditions; per-condition M and SD do not determine it) | t, df, p, d, CI | — | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); effect-size convention (d_z (SD of the differences), d_rm (repeated measures), d_av (average SD), g_av (average SD, Hedges’ correction)); direction of the difference: first minus second in Pair(first, second) |
| Moderation | n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p | n, M, SD per variable, the correlations, the X·Z moments | the correlations (coefficients depend on how the predictors overlap); the X·Z moments (cannot be derived from X and Z summaries) | b, SE, t, p, CI, R², F, df1, df2, model p | — | confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²); centering; coding |
| Multiple regression | n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p | n, M, SD per variable, the correlations | the correlations (coefficients depend on how the predictors overlap) | b, SE, t, p, CI, R², F, df1, df2, model p | — | confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²); factor coding |
| Simple mediation | n, M, SD per variable, b, SE, t, p, CI, a·b and its CI | n, M, SD and correlations of X, M, Y | the correlations of X, M, Y | a, b, c, c′ (b, SE, t, p, CI each) | the CI of a·b (inferred by bootstrap, which resamples cases; summaries cannot) | confidence level (90%, 95%, 99%) |
| Simple regression | n, M, SD per variable, b, SE, t, p, CI, R², F, df1, df2, model p | n, M, SD per variable, r | — | b, SE, t, p, CI, R², F, df1, df2, model p | — | confidence level (90%, 95%, 99%); effect-size convention (R², adjusted R²) |
| Chi-squared goodness of fit | the observed counts, chisq, df, p, N | the observed counts, H0 proportions | H0 proportions (the expected counts depend on them; not needed when they are equal) | chisq, df, p, N | — | effect-size convention (Cohen’s w) |
| Chi-squared test of independence | the contingency table, chisq, df, p, N, V | the contingency table | — | chisq, df, p, N, V | — | test (Pearson chi-squared, Yates’ continuity correction); effect-size convention (Cramér’s V) |
| Factorial ANOVA, two factors (between-subjects) | n, M, SD per condition, F, df1, df2, p, partial eta2 | n, M, SD per condition | — | F, df1, df2, p, partial eta2 | — | SS type for unbalanced designs (type III, type II, type I); effect-size convention (partial eta squared, partial omega squared) |
| Independent-samples t-test (Student, Welch) | n, M, SD per condition, t, df, p, d, CI | n, M, SD per condition | — | t, df, p, d, CI | — | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); test (Student’s t, Welch’s t); effect-size convention (Cohen’s d (pooled SD), Hedges’ g, Cohen’s d (root mean square of the SDs), Glass’s Δ (SD of the first group), Glass’s Δ (SD of the second group)) |
| One-sample t-test | n, M, SD, t, df, p, d, CI | n, M, SD | — | t, df, p, d, CI | — | tails (two-sided, one-sided (less), one-sided (greater)); confidence level (90%, 95%, 99%); effect-size convention (Cohen’s d, Hedges’ g); test value mu (default 0) |
| One-way ANOVA (between-subjects) | n, M, SD per condition, F, df1, df2, p, eta2 | n, M, SD per condition | — | F, df1, df2, p, eta2 | — | effect-size convention (eta squared, omega squared); variance assumption: equal variances (classical F test, not Welch’s) |

## 1. Round trip

Script: `inst/validation/roundtrip-simulation.R`. The round-trip check
(the internal `reproduce`, which
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
uses underneath) on the
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
result matches the original analysis, for every supported test, over
random designs (scaled difference below 1e-8).

    script: inst/validation/roundtrip-simulation.R
    seed: 20261122
    package: reprokit 0.1.0
    date: 2026-11-25
    R: R version 4.6.1 (2026-06-24)

                            handler designs worst_absolute worst_scaled result
                 chisq_independence     200       0.00e+00     0.00e+00   PASS
                          chisq_gof     200       0.00e+00     0.00e+00   PASS
                      t_independent     200       4.26e-14     6.27e-15   PASS
                       t_one_sample     200       7.11e-15     3.58e-16   PASS
                           t_paired     200       4.97e-14     4.31e-15   PASS
                        cor_pearson     200       0.00e+00     0.00e+00   PASS
                                 lm     200       8.56e-09     6.63e-13   PASS
                          lm_simple     200       1.60e-06     3.53e-12   PASS
                        lm_multiple     200       6.24e-09     3.73e-13   PASS
                      lm_moderation     200       3.18e-08     7.05e-13   PASS
             mediation_simple_small     200       1.78e-14     3.89e-15   PASS
             mediation_simple_large     200       8.76e-12     1.03e-12   PASS
              mediation_simple_near     200       3.09e-08     3.43e-11   PASS
              anova_oneway_balanced     200       5.68e-14     2.19e-15   PASS
            anova_oneway_unbalanced     200       1.42e-14     2.11e-15   PASS
           anova_factorial_balanced     200       5.79e-13     5.79e-13   PASS
     anova_factorial_unbalanced_III     200       2.10e-13     1.84e-13   PASS
      anova_factorial_unbalanced_II     200       4.04e-12     4.04e-12   PASS
       anova_factorial_unbalanced_I     200       4.55e-13     1.32e-14   PASS
          t_independent_from_result     200       4.26e-14     8.85e-16   PASS
           t_one_sample_from_result     200       3.55e-15     4.16e-16   PASS
               t_paired_from_result     200       4.26e-14     2.44e-15   PASS
            cor_pearson_from_result     200       0.00e+00     0.00e+00   PASS
     chisq_independence_from_result     200       0.00e+00     0.00e+00   PASS
           anova_oneway_from_result     200       2.13e-14     1.44e-15   PASS
        anova_factorial_from_result     200       4.00e-13     4.00e-13   PASS
         mediation_two_models_small     200       1.95e-14     2.89e-15   PASS
         mediation_two_models_large     200       1.12e-11     8.81e-13   PASS
          mediation_two_models_near     200       2.70e-08     3.62e-11   PASS

## 2. False positives (release gate)

Script: `inst/validation/check-false-positives.R`. Correctly reported,
correctly rounded output of every supported test is never flagged
inconsistent, and
[`check_reported()`](https://dm-k1.github.io/reprokit/reference/check_reported.md)
never errors on it. The release gate is 0 false positives.

    script: inst/validation/check-false-positives.R
    seed: 20261123
    package: reprokit 0.1.0
    date: 2026-11-25
    R: R version 4.6.1 (2026-06-24)

                          handler designs checks |r|>=.995 n<=6 sd 0 dec yates w>1
               chisq_independence     254   2286         0    0        0     9   0
           chisq_independence_2x2     285   2565         0    0        0   149   0
                        chisq_gof     300   2700         0    0        0     0 200
                 chisq_gof_w_gt_1     300   2700         0    0        0     0 277
                         t_paired     300   3020        53  138       91     0   0
                       t_paired_d     300   1500        56  131       81     0   0
                      cor_pearson     300   2354        66  133        0     0   0
            t_independent_student     300   2700         0  212       87     0   0
              t_independent_welch     300   2700         0  211       93     0   0
                     t_one_sample     300   2700         0  126       92     0   0
                     anova_oneway     300   2700         0  192        0     0   0
     anova_oneway_negative_omega2     476   4284         0  335        0     0   0
                  anova_factorial     300   2700         0    0        0     0   0
              anova_factorial_2x2     300   2700         0    0        0     0   0
                        lm_simple     300   6600         0    0        0     0   0
               lm_simple_weighted     300   6600         0    0        0     0   0
                      lm_multiple     300   5400         0    0        0     0   0
               lm_multiple_factor     300   5400         0    0        0     0   0
             lm_multiple_weighted     300   5400         0    0        0     0   0
                    lm_moderation     300   5400         0    0        0     0   0
           lm_moderation_weighted     300   5400         0    0        0     0   0
                  mediation_small     300   3900         0    0        0     0   0
                  mediation_large     300   3900         0    0        0     0   0
                  mediation_mixed     300   3900         0    0        0     0   0
     unbalanced false_positives errors
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
            168               0      0
            182               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
              0               0      0
    one-way designs with F < 1 and a negative omega2 checked and not flagged: 476 
    goodness-of-fit designs with w > 1 checked and not flagged: 277 

    PASS: false positives = 0, errors = 0

## 3. Known answers

Script: `inst/validation/check-known-answers.R`. Published or
hand-computed cases give the expected status and range.

    script: inst/validation/check-known-answers.R
    seed: none (fixed cases)
    package: reprokit 0.1.0
    date: 2026-11-25
    R: R version 4.6.1 (2026-06-24)

    == Table 7.8, row 2: recomputed ranges beside the reported values ==
    n = 24 and 18; M and SD to the unit (box +/- 0.5);
    p range is from t's interval 2.8765 to 2.8775
                      quantity reported range_lower range_upper reported_inside
                    t, Student    2.877       2.855       2.983            TRUE
                      t, Welch    2.877       3.125       3.270           FALSE
                   df, Student   40.000      40.000      40.000            TRUE
                     df, Welch   40.000      34.649      35.248           FALSE
         Cohen's d (pooled SD)    0.840       0.890       0.930           FALSE
     p from t = 2.877 at df 40    0.006       0.006       0.006            TRUE

    == The engine: check_reported("student", ...) and check_reported("welch", ...) ==
    The analysis is consistent with the reported statistics.
    Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
    Variants that fit: two-sided
    Tests that fit: Student's t
    Effect-size conventions that fit: none

     quantity reported           status
            n   24, 18         coherent
            m 759, 694         coherent
           sd   87, 42         coherent
            t    2.877         verified
           df       40         verified
            p     .006         coherent
            d    0.840 not reproducible
                                             checked_against
                              df_from_n; t_from_descriptives
                                         t_from_descriptives
                              df_from_n; t_from_descriptives
                               t_from_descriptives; p_from_t
                                         df_from_n; p_from_t
                                                    p_from_t
     no convention tried reproduces it (d_from_descriptives)

    An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.
    The analysis is inconsistent with the reported statistics.
    Level 3: the sufficient set is reported (the numbers contain everything needed to reproduce the analysis; that is not itself a check).
    Variants that fit: two-sided
    Tests that fit: none
    Effect-size conventions that fit: none

     quantity reported           status
            n   24, 18     inconsistent
            m 759, 694     inconsistent
           sd   87, 42     inconsistent
            t    2.877     inconsistent
           df       40     inconsistent
            p     .006         coherent
            d    0.840 not reproducible
                                             checked_against
                              df_from_n; t_from_descriptives
                                         t_from_descriptives
                              df_from_n; t_from_descriptives
                               t_from_descriptives; p_from_t
                                         df_from_n; p_from_t
                                                    p_from_t
     no convention tried reproduces it (d_from_descriptives)

    An inconsistency shows the numbers cannot all be correct as reported, not which one is wrong or why.

    == Assertions ==
                                                                      assertion
                                                     engine: overall consistent
                                                 engine: no number inconsistent
                                                           engine: Student fits
                                                     engine: Welch does not fit
                               engine: d is not reproducible (not inconsistent)
                                            reported t inside the Student range
                                    reported df equals Student df (n1 + n2 - 2)
                                               p from t at df 40 rounds to .006
                            fits Student, not Welch (t outside the Welch range)
                           fits Student, not Welch (df outside the Welch range)
     Cohen's d outside the pooled-d range (undeclared convention, not an error)
     holds
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE
      TRUE

    Cohen's d = 0.840 is outside the range the descriptives imply (0.890 to 0.930). The convention for d is not declared and the manual requires none, so this is
    an unfalsifiable value, not an inconsistency. The note's claim of unequal variance
    does not match the numbers, which fit Student.

    PASS: Table 7.8 row 2 is not flagged inconsistent; Student fits, Welch does not;
    d is not reproducible.

    == Case 2: sleep data, CI at its own level ==
     level independent_fits independent_consistent paired_fits paired_consistent
      0.90             TRUE                   TRUE        TRUE              TRUE
      0.95             TRUE                   TRUE        TRUE              TRUE
      0.99             TRUE                   TRUE        TRUE              TRUE
    paired d_z = -1.28 -> status verified ; wrong sign -> not reproducible 

    PASS: sleep CIs fit their levels (4/4 groups of claims), paired d verified and sign-flipped d not reproducible (4/4)

    == Case 3: PlantGrowth one-way ANOVA ==
     quantity   true range_lower range_upper
            F 4.8461      4.7625      5.1555
         eta2 0.2641      0.2608      0.2764
                     assertion holds
           true F inside range  TRUE
        true eta2 inside range  TRUE
        full report consistent  TRUE
        no number inconsistent  TRUE
     f/df/p only is consistent  TRUE
      planted df2 = 29 flagged  TRUE

    PASS: PlantGrowth F and eta2 inside their ranges, correct report consistent, planted df2 flagged (6/6)

    == Case 4: chi-squared from stats::chisq.test() ==
                                      case  chisq df      p   v consistent
                   HairEyeColor Hair x Eye 138.29  9 < .001 .28       TRUE
       UCBAdmissions Admit x Gender, Yates  91.61  1 < .001 .14       TRUE
     UCBAdmissions Admit x Gender, Pearson  92.21  1 < .001 .14       TRUE
                               cyl uniform   2.31  2  0.315 .27       TRUE
                         cyl p0 = .3/.2/.5   0.51  2  0.775 .13       TRUE
     none_inconsistent variant_right wrong_df_flagged
                  TRUE          TRUE             TRUE
                  TRUE          TRUE             TRUE
                  TRUE          TRUE             TRUE
                  TRUE            NA             TRUE
                  TRUE            NA             TRUE

    PASS: correct chi-squared reports consistent, test variant identified, wrong df flagged (4/4)

    == Case 5: factorial ANOVA ==
          design type matches_published_and_aov
     ToothGrowth  III                      TRUE
     ToothGrowth   II                      TRUE
     ToothGrowth    I                      TRUE
     design type F_matches_base_R p_matches_base_R matches_car
     mtcars  III             TRUE             TRUE        TRUE
     mtcars   II             TRUE             TRUE        TRUE
     mtcars    I             TRUE             TRUE          NA
    (matches_car is NA for type I, which car::Anova() does not offer, and when car is not installed.)
          design effect type consistent none_inconsistent type_identified
     ToothGrowth      A    -       TRUE              TRUE            TRUE
     ToothGrowth      B    -       TRUE              TRUE            TRUE
     ToothGrowth     AB    -       TRUE              TRUE            TRUE
          mtcars      A  III       TRUE              TRUE            TRUE
          mtcars      B  III       TRUE              TRUE            TRUE
          mtcars     AB  III       TRUE              TRUE            TRUE
          mtcars      A   II       TRUE              TRUE            TRUE
          mtcars      B   II       TRUE              TRUE            TRUE
          mtcars     AB   II       TRUE              TRUE            TRUE
          mtcars      A    I       TRUE              TRUE            TRUE
          mtcars      B    I       TRUE              TRUE            TRUE
          mtcars     AB    I       TRUE              TRUE            TRUE
     wrong_df2_flagged
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                  TRUE
                                                            assertion holds
             ToothGrowth matches published values and aov (all types)  TRUE
                         mtcars F equals anova()/drop1() at each type  TRUE
                         mtcars p equals anova()/drop1() at each type  TRUE
                            mtcars equals car::Anova() when installed  TRUE
                  mtcars main-effect F differs across the three types  TRUE
                       mtcars interaction F is the same in every type  TRUE
                                      every correct report consistent  TRUE
                                                  no row inconsistent  TRUE
     SS type identified (mtcars) / no variant (balanced, interaction)  TRUE
                                            planted wrong df2 flagged  TRUE

    PASS: factorial known answers (10/10)

    == Case 6: regression family against summary(lm) on mtcars ==
            row coef_diff model_diff p_model_diff report_consistent
         simple  4.99e-16   3.11e-16     4.91e-25              TRUE
       multiple  2.35e-15   8.71e-16     3.07e-25              TRUE
     moderation  1.52e-14   1.82e-14     6.75e-26              TRUE
     no_inconsistent_row
                    TRUE
                    TRUE
                    TRUE

    -- mediation paths against three lm() fits --
        path  estimate scaled_diff
           a 46.160000    1.08e-15
           c -5.344500    4.99e-16
           b -0.031773    1.51e-15
     c_prime -3.877800    1.59e-15

    -- mediation report with a*b and a bootstrap CI --
              quantity reported        status                 checked_against
                   b:a    46.16      coherent t_from_b_se:a; indirect_from_ab
                   b:b    -0.03      coherent t_from_b_se:b; indirect_from_ab
                   b:c    -5.34      coherent                   t_from_b_se:c
             b:c_prime    -3.88      coherent             t_from_b_se:c_prime
                  se:a     9.63      coherent                   t_from_b_se:a
                  se:b     0.01      coherent                   t_from_b_se:b
                  se:c     0.56      coherent                   t_from_b_se:c
            se:c_prime     0.63      coherent             t_from_b_se:c_prime
                   t:a     4.80      coherent                   t_from_b_se:a
                   t:b    -3.52      coherent                   t_from_b_se:b
                   t:c    -9.56      coherent                   t_from_b_se:c
             t:c_prime    -6.13      coherent             t_from_b_se:c_prime
                     n       32 not checkable                            <NA>
              indirect    -1.47      coherent                indirect_from_ab
     indirect_ci_lower    -5.00 not checkable                            <NA>
     indirect_ci_upper    -1.00 not checkable                            <NA>

    -- mediation path CIs against confint() at three levels --
     level consistent fits_printed_level_only moved_bound_inconsistent
      0.90       TRUE                    TRUE                     TRUE
      0.95       TRUE                    TRUE                     TRUE
      0.99       TRUE                    TRUE                     TRUE

                                                       assertion holds
                          b, SE, t, p equal summary(lm) (3 rows)  TRUE
                            R2, adjusted R2, F equal summary(lm)  TRUE
                                             model p equals pf()  TRUE
                                  each correct report consistent  TRUE
                         no inconsistent row in a correct report  TRUE
                       mediation paths equal the three lm() fits  TRUE
                                     mediation report consistent  TRUE
     a*b coherent, its bootstrap CI not checkable (right values)  TRUE
                   a*b and its CI not checkable (b path missing)  TRUE
                        absurd a*b does not make it inconsistent  TRUE
               path CIs from confint() consistent at 90, 95, 99%  TRUE
                     the printed level is the only one that fits  TRUE
                           a moved path CI bound is inconsistent  TRUE

    PASS: regression known answers (13/13)

## 4. Planted errors

Script: `inst/validation/check-planted-errors.R`. Errors planted in
correct papers are detected, and the cases that cannot be detected from
the numbers are the ones the oracle in the script says cannot be.

    script: inst/validation/check-planted-errors.R
    seed: 20261124, 20261125
    package: reprokit 0.1.0
    date: 2026-11-25
    R: R version 4.6.1 (2026-06-24)

    Detection by handler x error type x reported subset
                     group       type   subset cases % flagged % unevaluable % missed
              anova_oneway   decision      all   200     100.0           0.0      0.0
              anova_oneway   decision desc+f+p   200     100.0           0.0      0.0
              anova_oneway   decision   f/df/p   200     100.0           0.0      0.0
              anova_oneway df_wrong_n      all   200     100.0           0.0      0.0
              anova_oneway df_wrong_n   f/df/p   200      22.5           0.0     77.5
              anova_oneway    f_digit      all   200      68.5           0.0     31.5
              anova_oneway    f_digit desc+f+p   200      68.5           0.0     31.5
              anova_oneway    f_digit   f/df/p   200      55.0           0.0     45.0
              anova_oneway   m_change      all   200      70.0           0.0     30.0
              anova_oneway   m_change desc+f+p   200      70.0           0.0     30.0
              anova_oneway    p_digit      all   200      94.0           0.0      6.0
              anova_oneway    p_digit desc+f+p   200      94.0           0.0      6.0
              anova_oneway    p_digit   f/df/p   200      94.0           0.0      6.0
               cor_pearson  ci_change      all   166      95.2           0.0      4.8
               cor_pearson  ci_change  desc+ci   166      95.2           0.0      4.8
               cor_pearson   decision      all   167      99.4           0.0      0.6
               cor_pearson   decision desc+t+p   167      99.4           0.0      0.6
               cor_pearson   decision   r/df/p   167      82.6          16.2      1.2
               cor_pearson   decision   t/df/p   167      99.4           0.0      0.6
               cor_pearson df_wrong_n      all   167     100.0           0.0      0.0
               cor_pearson df_wrong_n   r/df/p   167      53.3          16.8     29.9
               cor_pearson df_wrong_n   t/df/p   167      44.3           0.0     55.7
               cor_pearson    p_digit      all   167      91.0           0.0      9.0
               cor_pearson    p_digit desc+t+p   167      91.0           0.0      9.0
               cor_pearson    p_digit   r/df/p   167      73.1           5.4     21.6
               cor_pearson    p_digit   t/df/p   167      91.0           0.0      9.0
               cor_pearson    r_digit      all   166      97.6           0.0      2.4
               cor_pearson    r_digit  desc+ci   146      93.2           0.0      6.8
               cor_pearson    r_digit desc+t+p   166      97.6           0.0      2.4
               cor_pearson    r_digit   r/df/p   166      74.1           0.0     25.9
               cor_pearson    t_digit      all   167      63.5           0.0     36.5
               cor_pearson    t_digit desc+t+p   167      63.5           0.0     36.5
               cor_pearson    t_digit   t/df/p   167      53.9           0.0     46.1
     t_independent_student  ci_change      all   166     100.0           0.0      0.0
     t_independent_student  ci_change  desc+ci   166     100.0           0.0      0.0
     t_independent_student   decision      all   167      99.4           0.0      0.6
     t_independent_student   decision desc+t+p   167      99.4           0.0      0.6
     t_independent_student   decision   t/df/p   167      99.4           0.0      0.6
     t_independent_student df_wrong_n      all   167     100.0           0.0      0.0
     t_independent_student df_wrong_n   t/df/p   167      30.5           0.0     69.5
     t_independent_student   m_change      all   166     100.0           0.0      0.0
     t_independent_student   m_change  desc+ci   166     100.0           0.0      0.0
     t_independent_student   m_change desc+t+p   166      70.5           3.6     25.9
     t_independent_student    p_digit      all   167      88.6           0.0     11.4
     t_independent_student    p_digit desc+t+p   167      88.6           0.0     11.4
     t_independent_student    p_digit   t/df/p   167      88.6           0.0     11.4
     t_independent_student    t_digit      all   167      89.8           0.0     10.2
     t_independent_student    t_digit desc+t+p   167      89.8           0.0     10.2
     t_independent_student    t_digit   t/df/p   167      86.8           0.0     13.2
       t_independent_welch  ci_change      all   166      95.2           4.2      0.6
       t_independent_welch  ci_change  desc+ci   166      95.2           4.2      0.6
       t_independent_welch   decision      all   167     100.0           0.0      0.0
       t_independent_welch   decision desc+t+p   167      96.4           1.8      1.8
       t_independent_welch   decision   t/df/p   167     100.0           0.0      0.0
       t_independent_welch df_wrong_n      all   167      98.8           0.0      1.2
       t_independent_welch df_wrong_n   t/df/p   167      77.8           0.0     22.2
       t_independent_welch   m_change      all   166      91.6           7.8      0.6
       t_independent_welch   m_change  desc+ci   166      91.6           7.8      0.6
       t_independent_welch   m_change desc+t+p   166      73.5           7.8     18.7
       t_independent_welch    p_digit      all   167      86.2           0.0     13.8
       t_independent_welch    p_digit desc+t+p   167      79.0           6.0     15.0
       t_independent_welch    p_digit   t/df/p   167      86.2           0.0     13.8
       t_independent_welch    t_digit      all   167      91.6           0.0      8.4
       t_independent_welch    t_digit desc+t+p   167      80.8           8.4     10.8
       t_independent_welch    t_digit   t/df/p   167      84.4           0.0     15.6
              t_one_sample  ci_change      all   166     100.0           0.0      0.0
              t_one_sample  ci_change  desc+ci   166     100.0           0.0      0.0
              t_one_sample   decision      all   167      99.4           0.0      0.6
              t_one_sample   decision desc+t+p   167      99.4           0.0      0.6
              t_one_sample   decision   t/df/p   167      99.4           0.0      0.6
              t_one_sample df_wrong_n      all   167     100.0           0.0      0.0
              t_one_sample df_wrong_n   t/df/p   167      49.1           0.0     50.9
              t_one_sample   m_change      all   166     100.0           0.0      0.0
              t_one_sample   m_change  desc+ci   166     100.0           0.0      0.0
              t_one_sample   m_change desc+t+p   166      83.1           4.2     12.7
              t_one_sample    p_digit      all   167      91.0           0.0      9.0
              t_one_sample    p_digit desc+t+p   167      91.0           0.0      9.0
              t_one_sample    p_digit   t/df/p   167      91.0           0.0      9.0
              t_one_sample    t_digit      all   167      88.0           0.0     12.0
              t_one_sample    t_digit desc+t+p   167      88.0           0.0     12.0
              t_one_sample    t_digit   t/df/p   167      74.9           0.0     25.1
                  t_paired  ci_change      all   166      93.4           0.0      6.6
                  t_paired  ci_change  desc+ci   166      93.4           0.0      6.6
                  t_paired   decision      all   167     100.0           0.0      0.0
                  t_paired   decision desc+t+p   167     100.0           0.0      0.0
                  t_paired   decision   t/df/p   167     100.0           0.0      0.0
                  t_paired df_wrong_n      all   167     100.0           0.0      0.0
                  t_paired df_wrong_n   t/df/p   167      44.9           0.0     55.1
                  t_paired   m_change      all   166      90.4           0.0      9.6
                  t_paired   m_change  desc+ci   166      90.4           0.0      9.6
                  t_paired   m_change desc+t+p   166      68.1          10.8     21.1
                  t_paired    p_digit      all   167      90.4           0.0      9.6
                  t_paired    p_digit desc+t+p   167      90.4           0.0      9.6
                  t_paired    p_digit   t/df/p   167      90.4           0.0      9.6
                  t_paired    t_digit      all   167      67.1           0.0     32.9
                  t_paired    t_digit desc+t+p   167      67.1           0.0     32.9
                  t_paired    t_digit   t/df/p   167      60.5           0.0     39.5

    Detection by size of the error (digit changes, M, df; decision errors are all large)
                     group       type  size   subset cases % flagged % unevaluable % missed
              anova_oneway df_wrong_n n<=10      all    12     100.0           0.0      0.0
              anova_oneway df_wrong_n n<=10   f/df/p    12      91.7           0.0      8.3
              anova_oneway df_wrong_n  n>10      all   188     100.0           0.0      0.0
              anova_oneway df_wrong_n  n>10   f/df/p   188      18.1           0.0     81.9
              anova_oneway    f_digit large      all   141      70.9           0.0     29.1
              anova_oneway    f_digit large desc+f+p   141      70.9           0.0     29.1
              anova_oneway    f_digit large   f/df/p   141      57.4           0.0     42.6
              anova_oneway    f_digit small      all    59      62.7           0.0     37.3
              anova_oneway    f_digit small desc+f+p    59      62.7           0.0     37.3
              anova_oneway    f_digit small   f/df/p    59      49.2           0.0     50.8
              anova_oneway   m_change large      all   146      72.6           0.0     27.4
              anova_oneway   m_change large desc+f+p   146      72.6           0.0     27.4
              anova_oneway   m_change small      all    54      63.0           0.0     37.0
              anova_oneway   m_change small desc+f+p    54      63.0           0.0     37.0
              anova_oneway    p_digit large      all   141      99.3           0.0      0.7
              anova_oneway    p_digit large desc+f+p   141      99.3           0.0      0.7
              anova_oneway    p_digit large   f/df/p   141      99.3           0.0      0.7
              anova_oneway    p_digit small      all    59      81.4           0.0     18.6
              anova_oneway    p_digit small desc+f+p    59      81.4           0.0     18.6
              anova_oneway    p_digit small   f/df/p    59      81.4           0.0     18.6
               cor_pearson  ci_change large      all   115      94.8           0.0      5.2
               cor_pearson  ci_change large  desc+ci   115      94.8           0.0      5.2
               cor_pearson  ci_change small      all    51      96.1           0.0      3.9
               cor_pearson  ci_change small  desc+ci    51      96.1           0.0      3.9
               cor_pearson df_wrong_n n<=10      all   101     100.0           0.0      0.0
               cor_pearson df_wrong_n n<=10   r/df/p   101      73.3          15.8     10.9
               cor_pearson df_wrong_n n<=10   t/df/p   101      73.3           0.0     26.7
               cor_pearson df_wrong_n  n>10      all    66     100.0           0.0      0.0
               cor_pearson df_wrong_n  n>10   r/df/p    66      22.7          18.2     59.1
               cor_pearson df_wrong_n  n>10   t/df/p    66       0.0           0.0    100.0
               cor_pearson    p_digit large      all   109      99.1           0.0      0.9
               cor_pearson    p_digit large desc+t+p   109      99.1           0.0      0.9
               cor_pearson    p_digit large   r/df/p   109      89.0           8.3      2.8
               cor_pearson    p_digit large   t/df/p   109      99.1           0.0      0.9
               cor_pearson    p_digit small      all    58      75.9           0.0     24.1
               cor_pearson    p_digit small desc+t+p    58      75.9           0.0     24.1
               cor_pearson    p_digit small   r/df/p    58      43.1           0.0     56.9
               cor_pearson    p_digit small   t/df/p    58      75.9           0.0     24.1
               cor_pearson    r_digit large      all   105     100.0           0.0      0.0
               cor_pearson    r_digit large  desc+ci    93      98.9           0.0      1.1
               cor_pearson    r_digit large desc+t+p   105     100.0           0.0      0.0
               cor_pearson    r_digit large   r/df/p   105      83.8           0.0     16.2
               cor_pearson    r_digit small      all    61      93.4           0.0      6.6
               cor_pearson    r_digit small  desc+ci    53      83.0           0.0     17.0
               cor_pearson    r_digit small desc+t+p    61      93.4           0.0      6.6
               cor_pearson    r_digit small   r/df/p    61      57.4           0.0     42.6
               cor_pearson    t_digit large      all   133      68.4           0.0     31.6
               cor_pearson    t_digit large desc+t+p   133      68.4           0.0     31.6
               cor_pearson    t_digit large   t/df/p   133      56.4           0.0     43.6
               cor_pearson    t_digit small      all    34      44.1           0.0     55.9
               cor_pearson    t_digit small desc+t+p    34      44.1           0.0     55.9
               cor_pearson    t_digit small   t/df/p    34      44.1           0.0     55.9
     t_independent_student  ci_change large      all   106     100.0           0.0      0.0
     t_independent_student  ci_change large  desc+ci   106     100.0           0.0      0.0
     t_independent_student  ci_change small      all    60     100.0           0.0      0.0
     t_independent_student  ci_change small  desc+ci    60     100.0           0.0      0.0
     t_independent_student df_wrong_n n<=10      all    39     100.0           0.0      0.0
     t_independent_student df_wrong_n n<=10   t/df/p    39      64.1           0.0     35.9
     t_independent_student df_wrong_n  n>10      all   128     100.0           0.0      0.0
     t_independent_student df_wrong_n  n>10   t/df/p   128      20.3           0.0     79.7
     t_independent_student   m_change large      all   111     100.0           0.0      0.0
     t_independent_student   m_change large  desc+ci   111     100.0           0.0      0.0
     t_independent_student   m_change large desc+t+p   111      76.6           2.7     20.7
     t_independent_student   m_change small      all    55     100.0           0.0      0.0
     t_independent_student   m_change small  desc+ci    55     100.0           0.0      0.0
     t_independent_student   m_change small desc+t+p    55      58.2           5.5     36.4
     t_independent_student    p_digit large      all   108      99.1           0.0      0.9
     t_independent_student    p_digit large desc+t+p   108      99.1           0.0      0.9
     t_independent_student    p_digit large   t/df/p   108      99.1           0.0      0.9
     t_independent_student    p_digit small      all    59      69.5           0.0     30.5
     t_independent_student    p_digit small desc+t+p    59      69.5           0.0     30.5
     t_independent_student    p_digit small   t/df/p    59      69.5           0.0     30.5
     t_independent_student    t_digit large      all   129      93.8           0.0      6.2
     t_independent_student    t_digit large desc+t+p   129      93.8           0.0      6.2
     t_independent_student    t_digit large   t/df/p   129      92.2           0.0      7.8
     t_independent_student    t_digit small      all    38      76.3           0.0     23.7
     t_independent_student    t_digit small desc+t+p    38      76.3           0.0     23.7
     t_independent_student    t_digit small   t/df/p    38      68.4           0.0     31.6
       t_independent_welch  ci_change large      all   122      95.9           4.1      0.0
       t_independent_welch  ci_change large  desc+ci   122      95.9           4.1      0.0
       t_independent_welch  ci_change small      all    44      93.2           4.5      2.3
       t_independent_welch  ci_change small  desc+ci    44      93.2           4.5      2.3
       t_independent_welch df_wrong_n n<=10      all    38     100.0           0.0      0.0
       t_independent_welch df_wrong_n n<=10   t/df/p    38      94.7           0.0      5.3
       t_independent_welch df_wrong_n  n>10      all   129      98.4           0.0      1.6
       t_independent_welch df_wrong_n  n>10   t/df/p   129      72.9           0.0     27.1
       t_independent_welch   m_change large      all   112      92.9           7.1      0.0
       t_independent_welch   m_change large  desc+ci   112      92.9           7.1      0.0
       t_independent_welch   m_change large desc+t+p   112      77.7           7.1     15.2
       t_independent_welch   m_change small      all    54      88.9           9.3      1.9
       t_independent_welch   m_change small  desc+ci    54      88.9           9.3      1.9
       t_independent_welch   m_change small desc+t+p    54      64.8           9.3     25.9
       t_independent_welch    p_digit large      all   108     100.0           0.0      0.0
       t_independent_welch    p_digit large desc+t+p   108      91.7           8.3      0.0
       t_independent_welch    p_digit large   t/df/p   108     100.0           0.0      0.0
       t_independent_welch    p_digit small      all    59      61.0           0.0     39.0
       t_independent_welch    p_digit small desc+t+p    59      55.9           1.7     42.4
       t_independent_welch    p_digit small   t/df/p    59      61.0           0.0     39.0
       t_independent_welch    t_digit large      all   134      94.8           0.0      5.2
       t_independent_welch    t_digit large desc+t+p   134      83.6           8.2      8.2
       t_independent_welch    t_digit large   t/df/p   134      90.3           0.0      9.7
       t_independent_welch    t_digit small      all    33      78.8           0.0     21.2
       t_independent_welch    t_digit small desc+t+p    33      69.7           9.1     21.2
       t_independent_welch    t_digit small   t/df/p    33      60.6           0.0     39.4
              t_one_sample  ci_change large      all   107     100.0           0.0      0.0
              t_one_sample  ci_change large  desc+ci   107     100.0           0.0      0.0
              t_one_sample  ci_change small      all    59     100.0           0.0      0.0
              t_one_sample  ci_change small  desc+ci    59     100.0           0.0      0.0
              t_one_sample df_wrong_n n<=10      all    94     100.0           0.0      0.0
              t_one_sample df_wrong_n n<=10   t/df/p    94      80.9           0.0     19.1
              t_one_sample df_wrong_n  n>10      all    73     100.0           0.0      0.0
              t_one_sample df_wrong_n  n>10   t/df/p    73       8.2           0.0     91.8
              t_one_sample   m_change large      all   111     100.0           0.0      0.0
              t_one_sample   m_change large  desc+ci   111     100.0           0.0      0.0
              t_one_sample   m_change large desc+t+p   111      85.6           2.7     11.7
              t_one_sample   m_change small      all    55     100.0           0.0      0.0
              t_one_sample   m_change small  desc+ci    55     100.0           0.0      0.0
              t_one_sample   m_change small desc+t+p    55      78.2           7.3     14.5
              t_one_sample    p_digit large      all   111      99.1           0.0      0.9
              t_one_sample    p_digit large desc+t+p   111      99.1           0.0      0.9
              t_one_sample    p_digit large   t/df/p   111      99.1           0.0      0.9
              t_one_sample    p_digit small      all    56      75.0           0.0     25.0
              t_one_sample    p_digit small desc+t+p    56      75.0           0.0     25.0
              t_one_sample    p_digit small   t/df/p    56      75.0           0.0     25.0
              t_one_sample    t_digit large      all   129      91.5           0.0      8.5
              t_one_sample    t_digit large desc+t+p   129      91.5           0.0      8.5
              t_one_sample    t_digit large   t/df/p   129      78.3           0.0     21.7
              t_one_sample    t_digit small      all    38      76.3           0.0     23.7
              t_one_sample    t_digit small desc+t+p    38      76.3           0.0     23.7
              t_one_sample    t_digit small   t/df/p    38      63.2           0.0     36.8
                  t_paired  ci_change large      all   122      96.7           0.0      3.3
                  t_paired  ci_change large  desc+ci   122      96.7           0.0      3.3
                  t_paired  ci_change small      all    44      84.1           0.0     15.9
                  t_paired  ci_change small  desc+ci    44      84.1           0.0     15.9
                  t_paired df_wrong_n n<=10      all   102     100.0           0.0      0.0
                  t_paired df_wrong_n n<=10   t/df/p   102      69.6           0.0     30.4
                  t_paired df_wrong_n  n>10      all    65     100.0           0.0      0.0
                  t_paired df_wrong_n  n>10   t/df/p    65       6.2           0.0     93.8
                  t_paired   m_change large      all   118      89.8           0.0     10.2
                  t_paired   m_change large  desc+ci   118      89.8           0.0     10.2
                  t_paired   m_change large desc+t+p   118      71.2          10.2     18.6
                  t_paired   m_change small      all    48      91.7           0.0      8.3
                  t_paired   m_change small  desc+ci    48      91.7           0.0      8.3
                  t_paired   m_change small desc+t+p    48      60.4          12.5     27.1
                  t_paired    p_digit large      all   112      99.1           0.0      0.9
                  t_paired    p_digit large desc+t+p   112      99.1           0.0      0.9
                  t_paired    p_digit large   t/df/p   112      99.1           0.0      0.9
                  t_paired    p_digit small      all    55      72.7           0.0     27.3
                  t_paired    p_digit small desc+t+p    55      72.7           0.0     27.3
                  t_paired    p_digit small   t/df/p    55      72.7           0.0     27.3
                  t_paired    t_digit large      all   129      66.7           0.0     33.3
                  t_paired    t_digit large desc+t+p   129      66.7           0.0     33.3
                  t_paired    t_digit large   t/df/p   129      59.7           0.0     40.3
                  t_paired    t_digit small      all    38      68.4           0.0     31.6
                  t_paired    t_digit small desc+t+p    38      68.4           0.0     31.6
                  t_paired    t_digit small   t/df/p    38      63.2           0.0     36.8

    CI bound changed by 5 to 20 units (ci_change), by confidence level of the paper
                     group  subset level cases % flagged % unevaluable % missed
               cor_pearson     all  0.90    37     100.0           0.0      0.0
               cor_pearson     all  0.95    85      92.9           0.0      7.1
               cor_pearson     all  0.99    44      95.5           0.0      4.5
               cor_pearson desc+ci  0.90    37     100.0           0.0      0.0
               cor_pearson desc+ci  0.95    85      92.9           0.0      7.1
               cor_pearson desc+ci  0.99    44      95.5           0.0      4.5
     t_independent_student     all  0.90    49     100.0           0.0      0.0
     t_independent_student     all  0.95    70     100.0           0.0      0.0
     t_independent_student     all  0.99    47     100.0           0.0      0.0
     t_independent_student desc+ci  0.90    49     100.0           0.0      0.0
     t_independent_student desc+ci  0.95    70     100.0           0.0      0.0
     t_independent_student desc+ci  0.99    47     100.0           0.0      0.0
       t_independent_welch     all  0.90    36      97.2           2.8      0.0
       t_independent_welch     all  0.95    90      96.7           3.3      0.0
       t_independent_welch     all  0.99    40      90.0           7.5      2.5
       t_independent_welch desc+ci  0.90    36      97.2           2.8      0.0
       t_independent_welch desc+ci  0.95    90      96.7           3.3      0.0
       t_independent_welch desc+ci  0.99    40      90.0           7.5      2.5
              t_one_sample     all  0.90    49     100.0           0.0      0.0
              t_one_sample     all  0.95    74     100.0           0.0      0.0
              t_one_sample     all  0.99    43     100.0           0.0      0.0
              t_one_sample desc+ci  0.90    49     100.0           0.0      0.0
              t_one_sample desc+ci  0.95    74     100.0           0.0      0.0
              t_one_sample desc+ci  0.99    43     100.0           0.0      0.0
                  t_paired     all  0.90    44      97.7           0.0      2.3
                  t_paired     all  0.95    86      89.5           0.0     10.5
                  t_paired     all  0.99    36      97.2           0.0      2.8
                  t_paired desc+ci  0.90    44      97.7           0.0      2.3
                  t_paired desc+ci  0.95    86      89.5           0.0     10.5
                  t_paired desc+ci  0.99    36      97.2           0.0      2.8
    CI-only detection (desc+ci subset), independent t, % flagged: Student all levels 100 , Student 95% 100 , Welch all levels 95.2 , Welch 95% 96.7 

    Oracle check (t/df/p and r/df/p: every type; desc+t+p: t_digit, m_change, r_digit)
           oracle     outcome Freq
       detectable     flagged 5187
     undetectable      missed 1229
     undetectable unevaluable   46
    edge cases left out: 410 
    joint-only (detectable only by tying the Student/Welch variant across relations; limitation, not a criterion):
    < table of extent 0 >

    flagged on t/df/p or desc+t+p but not on all: 3 of which Welch df printed with decimals (wider df interval than the one derived from n, SD): 3 
    flagged on r/df/p but not on all (printed t is wider than the t derived from r): 4 

    pooled large-error rates (%): decision_all = 99.7, decision_tdfp = 99.6, decision_desc = 98.9, decision_rdfp = 82.6, df_wrong_n_all = 99.8, m_large_all = 90 
    informational: lowest handler rate, decision/df_wrong_n/m_change-large on all (%): 72.6 
    planted papers: 6000  checks: 16582  runtime (min): 2.1 
    PASS: errors = 0, oracle-detectable not flagged = 0, oracle-undetectable flagged = 0, flagged on a subset but not on all = 0, cells never flagged = 0 (lowest large-error rate, informational = 72.6%)

    == Chi-squared planted errors: detection by handler x type x subset ==
                handler          type       subset   n flagged % flagged
              chisq_gof   chisq_digit   chisq/df/p 125      10       8.0
     chisq_independence   chisq_digit   chisq/df/p 100      28      28.0
              chisq_gof      df_wrong   chisq/df/p 125     125     100.0
     chisq_independence      df_wrong   chisq/df/p 100     100     100.0
              chisq_gof       p_digit   chisq/df/p 125     122      97.6
     chisq_independence       p_digit   chisq/df/p 100      99      99.0
     chisq_independence yates_pearson   chisq/df/p  97      97     100.0
              chisq_gof   chisq_digit n/chisq/df/p 125     125     100.0
     chisq_independence   chisq_digit n/chisq/df/p 100     100     100.0
              chisq_gof  count_change n/chisq/df/p 125     124      99.2
     chisq_independence  count_change n/chisq/df/p 100      99      99.0
              chisq_gof      df_wrong n/chisq/df/p 125     125     100.0
     chisq_independence      df_wrong n/chisq/df/p 100     100     100.0
              chisq_gof       p_digit n/chisq/df/p 125     122      97.6
     chisq_independence       p_digit n/chisq/df/p 100     100     100.0
     chisq_independence yates_pearson n/chisq/df/p  97      97     100.0
              chisq_gof   chisq_digit    n/chisq/p 125     125     100.0
     chisq_independence   chisq_digit    n/chisq/p 100     100     100.0
              chisq_gof  count_change    n/chisq/p 125     124      99.2
     chisq_independence  count_change    n/chisq/p 100      99      99.0
              chisq_gof       p_digit    n/chisq/p 125     122      97.6
     chisq_independence       p_digit    n/chisq/p 100     100     100.0
     chisq_independence yates_pearson    n/chisq/p  97      97     100.0

    Oracle check
           oracle outcome Freq
       detectable flagged 2340
     undetectable flagged    0
       detectable  missed    0
     undetectable  missed  201

    PASS: chi-squared planted errors, errors = 0, oracle-detectable not flagged = 0, oracle-undetectable flagged = 0, cells never flagged = 0

    == Factorial ANOVA planted errors ==
                 type   subset   n flagged % flagged
     cell_mean_change      all 120      41      34.2
            df2_wrong      all 120     120     100.0
              f_digit      all 120      85      70.8
              p_digit      all 120     112      93.3
             type_mix      all 120     102      85.0
     cell_mean_change desc+f+p 120      41      34.2
              f_digit desc+f+p 120      85      70.8
              p_digit desc+f+p 120     112      93.3
             type_mix desc+f+p 120     102      85.0
            df2_wrong   f/df/p 120      14      11.7
              f_digit   f/df/p 120      81      67.5
              p_digit   f/df/p 120     109      90.8
             type_mix   f/df/p 120     102      85.0
           oracle     outcome Freq
       detectable     flagged  306
     undetectable     flagged    0
       detectable not flagged    0
     undetectable not flagged  144

    PASS: factorial planted errors, errors = 0, oracle-detectable not flagged = 0, oracle-undetectable flagged = 0, flagged on f/df/p but not on all = 0, cells never flagged = 0

    == Regression family planted errors ==
           type     subset  n flagged % flagged
        b_digit        all 88      88     100.0
     df_wrong_k        all 87      87     100.0
        f_vs_r2        all 87      83      95.4
         med_ci        all 87      87     100.0
         med_df        all 87      87     100.0
        p_digit        all 88      78      88.6
       se_digit        all 88      88     100.0
        t_digit        all 88      88     100.0
         med_ci b/se/df/ci 87      87     100.0
        b_digit     b/se/t 88      86      97.7
       se_digit     b/se/t 88      80      90.9
        t_digit     b/se/t 88      85      96.6
     df_wrong_k  n/df1/df2 87      87     100.0
     df_wrong_k  n/df2 + k 87      87     100.0
        f_vs_r2    r2/f/df 87      63      72.4
         med_df     t/df/p 87      87     100.0
        p_digit     t/df/p 88      78      88.6
           oracle     outcome Freq
       detectable     flagged  740
     undetectable     flagged    0
       detectable not flagged    0
     undetectable not flagged   47

    PASS: regression planted errors, errors = 0, oracle-detectable not flagged = 0, oracle-undetectable flagged = 0, flagged on a narrow subset but not on all = 0, cells never flagged = 0

## 5. Range exactness

Script: `inst/validation/check-range-exactness.R`. The implied ranges
that
[`check_reported()`](https://dm-k1.github.io/reprokit/reference/check_reported.md)
gives for a missing number are exact against a brute-force oracle.

    script: inst/validation/check-range-exactness.R
    seed: 20261123, 20261125
    package: reprokit 0.1.0
    date: 2026-11-25
    R: R version 4.6.1 (2026-06-24)

                                                  relation boxes escaped_samples
                                       t_paired df_from_n    300               0
                             t_paired t_from_descriptives    288               0
                               t_paired p_from_t two.sided   300               0
                                    t_paired p_from_t less   300               0
                                 t_paired p_from_t greater   300               0
                  t_paired ci_lower_from_descriptives 0.90   300               0
                  t_paired ci_lower_from_descriptives 0.95   300               0
                  t_paired ci_lower_from_descriptives 0.99   300               0
                  t_paired ci_upper_from_descriptives 0.90   300               0
                  t_paired ci_upper_from_descriptives 0.95   300               0
                  t_paired ci_upper_from_descriptives 0.99   300               0
                          t_paired d_from_descriptives d_z   288               0
                         t_paired d_from_descriptives d_rm   288               0
                      t_paired d_av_from_descriptives d_av   292               0
                      t_paired d_av_from_descriptives g_av   292               0
                                    cor_pearson df_from_n    300               0
                                     cor_pearson t_from_r    181               0
                            cor_pearson p_from_t two.sided   300               0
                                 cor_pearson p_from_t less   300               0
                              cor_pearson p_from_t greater   300               0
                          cor_pearson ci_lower_from_r 0.90   265               0
                          cor_pearson ci_lower_from_r 0.95   265               0
                          cor_pearson ci_lower_from_r 0.99   265               0
                          cor_pearson ci_upper_from_r 0.90   265               0
                          cor_pearson ci_upper_from_r 0.95   265               0
                          cor_pearson ci_upper_from_r 0.99   265               0
                           t_independent df_from_n student   300               0
                             t_independent df_from_n welch   290               0
                 t_independent t_from_descriptives student   290               0
                   t_independent t_from_descriptives welch   290               0
                          t_independent p_from_t two.sided   300               0
                               t_independent p_from_t less   300               0
                            t_independent p_from_t greater   300               0
     t_independent ci_lower_from_descriptives 0.90+student   300               0
     t_independent ci_lower_from_descriptives 0.95+student   300               0
     t_independent ci_lower_from_descriptives 0.99+student   300               0
       t_independent ci_lower_from_descriptives 0.90+welch   290               0
       t_independent ci_lower_from_descriptives 0.95+welch   290               0
       t_independent ci_lower_from_descriptives 0.99+welch   290               0
     t_independent ci_upper_from_descriptives 0.90+student   300               0
     t_independent ci_upper_from_descriptives 0.95+student   300               0
     t_independent ci_upper_from_descriptives 0.99+student   300               0
       t_independent ci_upper_from_descriptives 0.90+welch   290               0
       t_independent ci_upper_from_descriptives 0.95+welch   290               0
       t_independent ci_upper_from_descriptives 0.99+welch   290               0
                  t_independent d_from_descriptives pooled   290               0
                t_independent d_from_descriptives hedges_g   290               0
                 t_independent d_from_descriptives average   290               0
               t_independent d_from_descriptives glass_sd1   281               0
               t_independent d_from_descriptives glass_sd2   285               0
                                   t_one_sample df_from_n    300               0
                         t_one_sample t_from_descriptives    282               0
                           t_one_sample p_from_t two.sided   300               0
                                t_one_sample p_from_t less   300               0
                             t_one_sample p_from_t greater   300               0
              t_one_sample ci_lower_from_descriptives 0.90   300               0
              t_one_sample ci_lower_from_descriptives 0.95   300               0
              t_one_sample ci_lower_from_descriptives 0.99   300               0
              t_one_sample ci_upper_from_descriptives 0.90   300               0
              t_one_sample ci_upper_from_descriptives 0.95   300               0
              t_one_sample ci_upper_from_descriptives 0.99   300               0
                    t_one_sample d_from_descriptives cohen   282               0
                 t_one_sample d_from_descriptives hedges_g   282               0
     skipped_not_finite
                      0
                     12
                      0
                      0
                      0
                      0
                      0
                      0
                      0
                      0
                      0
                     12
                     12
                      8
                      8
                      0
                    119
                      0
                      0
                      0
                     35
                     35
                     35
                     35
                     35
                     35
                      0
                     10
                     10
                     10
                      0
                      0
                      0
                      0
                      0
                      0
                     10
                     10
                     10
                      0
                      0
                      0
                     10
                     10
                     10
                     10
                     10
                     10
                     19
                     15
                      0
                     18
                      0
                      0
                      0
                      0
                      0
                      0
                      0
                      0
                      0
                     18
                     18

                                                case corner_only_upper true_upper
          t_from_descriptives (sd1 1.95-2.05, r .99)            15.762     15.851
                p_from_t two.sided (t = 0.00, df 19)             0.996      1.000
       df_from_n welch (n 5/6, sd 0.9-1.1 / 1.0-1.4)             8.987      9.000
     d_from_descriptives d_rm (sd1 1.95-2.05, r .99)             0.498      0.501
     scan_upper
         15.851
          1.000
          9.000
          0.501

    PASS: escaped samples = 0, named cases show corner-only < true = TRUE

    == One-way F range ==
                                        check     value
             random boxes with a finite range 3.000e+02
                boxes skipped (not evaluable) 0.000e+00
                    samples outside the range 0.000e+00
             max |exact - optimize()| SSb min 5.599e-16
     corner-only minimum F, overlapping means 3.300e-04
           exact minimum F, overlapping means 0.000e+00

    PASS: one-way F, escaped samples = 0, SSb min agrees with optimize() = TRUE, corner-only min > 0 and exact min = 0: TRUE

    == Cramer's V and Cohen's w ==
                                         check value
      max |range - chisq.test() V|, 200 tables     0
     max |range - chisq.test() w|, 200 vectors     0
                      random chi-squared boxes   600
                     samples outside the range     0

    PASS: V and w ranges, point from counts equals chisq.test() = TRUE, escaped samples = 0

    == Factorial ANOVA F and p ranges ==
     design effect df1 balanced type boxes skipped escaped upper_attained
        2x2      A   1     TRUE    -    60       0       0           TRUE
        2x2      A   1    FALSE  III    60       0       0           TRUE
        2x2      A   1    FALSE   II    60       0       0           TRUE
        2x2      A   1    FALSE    I    60       0       0           TRUE
        2x2      B   1     TRUE    -    60       0       0           TRUE
        2x2      B   1    FALSE  III    60       0       0           TRUE
        2x2      B   1    FALSE   II    60       0       0           TRUE
        2x2      B   1    FALSE    I    60       0       0           TRUE
        2x2     AB   1     TRUE    -    60       0       0           TRUE
        2x2     AB   1    FALSE    -    60       0       0           TRUE
        2x2     AB   1    FALSE    -    60       0       0           TRUE
        2x2     AB   1    FALSE    -    60       0       0           TRUE
        2x3      A   1     TRUE    -    60       0       0           TRUE
        2x3      A   1    FALSE  III    60       0       0           TRUE
        2x3      A   1    FALSE   II    60       0       0           TRUE
        2x3      A   1    FALSE    I    60       0       0           TRUE
        2x4      A   1     TRUE    -    60       0       0           TRUE
        2x4      A   1    FALSE  III    60       0       0           TRUE
        2x4      A   1    FALSE   II    60       0       0           TRUE
        2x4      A   1    FALSE    I    60       0       0           TRUE
        3x2      B   1     TRUE    -    60       0       0           TRUE
        3x2      B   1    FALSE  III    60       0       0           TRUE
        3x2      B   1    FALSE   II    60       0       0           TRUE
        3x2      B   1    FALSE    I    60       0       0           TRUE
        2x3      B   2     TRUE    -    60       0       0           TRUE
        2x3      B   2    FALSE  III     0      60       0           TRUE
        2x3      B   2    FALSE   II     0      60       0           TRUE
        2x3      B   2    FALSE    I     0      60       0           TRUE
        3x3      A   2     TRUE    -    60       0       0           TRUE
        3x3      A   2    FALSE  III     0      60       0           TRUE
        3x3      A   2    FALSE   II     0      60       0           TRUE
        3x3      A   2    FALSE    I     0      60       0           TRUE
        3x4      B   3     TRUE    -    60       0       0           TRUE
        3x4      B   3    FALSE  III     0      60       0           TRUE
        3x4      B   3    FALSE   II     0      60       0           TRUE
        3x4      B   3    FALSE    I     0      60       0           TRUE
     worst_lower_gap worst_upper_gap
               0.179           1.000
               0.171           1.000
               0.170           0.781
               0.139           0.744
               0.185           0.718
               0.170           0.811
               0.180           0.818
               0.194           1.000
               0.173           0.745
               0.155           0.747
               0.177           1.000
               0.171           1.000
               0.219           1.000
               0.236           0.820
               0.222           0.879
               0.207           1.000
               0.268           0.874
               0.253           0.874
               0.238           1.000
               0.234           0.916
               0.204           0.833
               0.227           0.842
               0.180           1.000
               0.211           0.813
               0.225           0.787
               0.000           0.000
               0.000           0.000
               0.000           0.000
               0.256           0.876
               0.000           0.000
               0.000           0.000
               0.000           0.000
               0.285           0.896
               0.000           0.000
               0.000           0.000
               0.000           0.000
    no-exact-range cases (interaction with df1 > 1, unbalanced df1 > 1) unbounded: TRUE

    PASS: factorial ranges, escaped samples = 0, F upper bound attained at a vertex = TRUE, sampled minimum within 50% of the width of the lower bound = TRUE, others unbounded = TRUE

    == Regression family ranges ==
                 relation variant boxes skipped escaped vertex_mismatch
            t_from_b_se:x       -   195       5       0               0
     ci_lower_from_b_se:x    0.90   200       0       0               0
     ci_lower_from_b_se:x    0.95   200       0       0               0
     ci_lower_from_b_se:x    0.99   200       0       0               0
     ci_upper_from_b_se:x    0.90   200       0       0               0
     ci_upper_from_b_se:x    0.95   200       0       0               0
     ci_upper_from_b_se:x    0.99   200       0       0               0
                f_from_r2      r2   200       0       0               0
                f_from_r2  adj_r2   200       0       0               0
                 r_from_b       -   200       0       0               0
               p_from_t:x       -   200       0       0              NA
    p from t on a box spanning t = 0: upper bound is 1 in 50 of 50 boxes; named case t in [-0.5, 0.7], df 20: corner-only upper = 0.6225, true upper = 1

    PASS: regression ranges, escaped draws = 0, vertex mismatches = 0, skipped boxes = 5

## 6. Agreement with statcheck

Script: `inst/validation/check-statcheck.R`. On the results statcheck
can parse,
[`check_reported()`](https://dm-k1.github.io/reprokit/reference/check_reported.md)
agrees with statcheck, and every disagreement is explained.

    script: inst/validation/check-statcheck.R
    seed: 20261124
    package: reprokit 0.1.0
    date: 2026-11-25
    R: R version 4.6.1 (2026-06-24)

    statcheck 1.6.1 - strings: 3527  unparsed by statcheck: 0 

    2 x 2 per group: rows = statcheck, columns = check_reported (planted and correct strings)

    t_paired
             check_reported
    statcheck consistent inconsistent
        error          1          189
        ok           410            0

    t_independent_student
             check_reported
    statcheck consistent inconsistent
        error          0          179
        ok           421            0

    t_independent_welch
             check_reported
    statcheck consistent inconsistent
        error         15          213
        ok           372            0

    t_one_sample
             check_reported
    statcheck consistent inconsistent
        error          2          188
        ok           410            0

    cor_pearson
             check_reported
    statcheck consistent inconsistent
        error          2          175
        ok           423            0

    cor_pearson_r
             check_reported
    statcheck consistent inconsistent
        error          4          170
        ok           353            0

    r(df), p strings passed as r, df, p (n implied from df): level 0 (not checkable) for 0 of 527 

    Correct papers only: check_reported inconsistent / statcheck error, of n
                     group   n check_reported statcheck
               cor_pearson 360              0         0
             cor_pearson_r 316              0         0
     t_independent_student 360              0         0
       t_independent_welch 360              0        12
              t_one_sample 360              0         0
                  t_paired 360              0         1

    Disagreements by category (statcheck error & check_reported consistent, or the reverse)
                   group   category Freq
             cor_pearson one-tailed    2
           cor_pearson_r one-tailed    4
     t_independent_welch one-tailed    1
            t_one_sample one-tailed    2
     t_independent_welch t-rounding    1
                t_paired t-rounding    1
     t_independent_welch   welch-df   13

    One example per category and group (text; truth/type; verdicts)
      [t-rounding | t_paired] t(2) = 0.00, p = .997  (correct/none) statcheck=error check_reported=consistent
      [welch-df | t_independent_welch] t(5.1) = -1.66, p = .155  (planted/p_digit) statcheck=error check_reported=consistent
      [one-tailed | t_independent_welch] t(3.5) = -2.63, p = .967  (planted/p_digit) statcheck=error check_reported=consistent
      [t-rounding | t_independent_welch] t(14.22) = 0.00, p = .998  (correct/none) statcheck=error check_reported=consistent
      [one-tailed | t_one_sample] t(7) = 5.00, p = .001  (planted/t_digit) statcheck=error check_reported=consistent
      [one-tailed | cor_pearson_r] r(2) = .95, p = .026  (planted/decision) statcheck=error check_reported=consistent
      [one-tailed | cor_pearson] t(2) = 22.42, p = .001  (planted/p_digit) statcheck=error check_reported=consistent

    check_reported flags, statcheck does not: 0  of which not oracle-detectable: 0 
    runtime (min): 0.4 
    PASS: check_reported false positives = 0, errors = 0, unexplained statcheck-only errors = 0, check-only flags not oracle-detectable = 0

## What is not validated here

Refusals (invalid input, and
[`appendix()`](https://dm-k1.github.io/reprokit/reference/appendix.md)
when summaries do not suffice, stop with class `reprokit_impossible`)
and each check rule’s behaviour on a known-bad and a known-good input
are covered by the package’s unit tests (`tests/testthat/`), not by
these scripts. The disclosure check is tested there too, for every
handler: a flagged case drops the
[`sufficient_stats()`](https://dm-k1.github.io/reprokit/reference/sufficient_stats.md)
verdict to `needs_raw_data`, a clean case is unaffected, zero counts are
not flagged, and the appendix names what was checked.
