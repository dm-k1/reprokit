# the paired appendix matches t.test()

    Code
      print(appendix(paired_stats(), choices = list(convention = "d_z")))
    Output
      ## Appendix: sufficient statistics for the paired-samples t-test
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      |  | drug1 | drug2 |
      | --- | --- | --- |
      | M | 0.75 | 2.33 |
      | SD | 1.79 | 2.00 |
      
      n = 10
      
      | Result | Value |
      | --- | --- |
      | t(9) | -4.06 |
      | p | .003 |
      | d, d_z (SD of the differences) | -1.28 |
      | 95% CI of the mean difference (drug1 - drug2) | [-2.46, -0.70] |
      
      ### B. Numbers APA does not require that the result needs
      
      | Number | Value | Why it is needed |
      | --- | --- | --- |
      | r between drug1 and drug2 | .7952 | APA does not require this; the SD of the differences depends on the correlation between conditions; per-condition M and SD do not determine it |
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Tails | two-sided |
      | Confidence level | 95% |
      | Effect-size convention | d_z (SD of the differences) (declaring it is a reprokit rule; APA does not require it) |
      | Direction of the difference | drug1 minus drug2 |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking the distribution and outliers of the differences
      - nonparametric or robust alternatives
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: sample size only (no groups or cells; no individual values stored). This lowers the risk of identifying a participant; it does not remove it.

# the one-sample appendix is stable

    Code
      print(appendix(s, choices = list(convention = "cohen")))
    Output
      ## Appendix: sufficient statistics for the one-sample t-test
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      | n | M | SD |
      | --- | --- | --- |
      | 20 | 1.54 | 2.02 |
      
      | Result | Value |
      | --- | --- |
      | t(19) | 2.30 |
      | p | .033 |
      | d, Cohen's d | 0.52 |
      | 95% CI of the mean (test value mu = 0.5) | [0.60, 2.48] |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Tails | two-sided |
      | Confidence level | 95% |
      | Effect-size convention | Cohen's d (declaring it is a reprokit rule; APA does not require it) |
      | Test value mu | 0.5 |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking the distribution and outliers
      - nonparametric or robust alternatives
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: sample size only (no groups or cells; no individual values stored). This lowers the risk of identifying a participant; it does not remove it.

# the independent-samples appendix is stable

    Code
      print(appendix(s, choices = list(convention = "pooled")))
    Output
      ## Appendix: sufficient statistics for the independent-samples t-test
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      |  | group 1 | group 2 |
      | --- | --- | --- |
      | n | 10 | 10 |
      | M | 0.75 | 2.33 |
      | SD | 1.79 | 2.00 |
      
      | Result | Value |
      | --- | --- |
      | t(17.78) | -1.86 |
      | p | .079 |
      | d, Cohen's d (pooled SD) | -0.83 |
      | 95% CI of the mean difference (group 1 - group 2) | [-3.37, 0.21] |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Tails | two-sided |
      | Confidence level | 95% |
      | Test | Welch's t |
      | Effect-size convention | Cohen's d (pooled SD) (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking the distribution and outliers within each group
      - nonparametric or robust alternatives
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: group sizes. This lowers the risk of identifying a participant; it does not remove it.

# the Pearson appendix is stable

    Code
      print(appendix(s))
    Output
      ## Appendix: sufficient statistics for the Pearson correlation test
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      |  | mpg | wt |
      | --- | --- | --- |
      | M | 20.09 | 3.22 |
      | SD | 6.03 | 0.98 |
      
      n = 32
      
      | Result | Value |
      | --- | --- |
      | r(30) | -.87 |
      | p | < .001 |
      | 95% CI of r | [-.93, -.74] |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Tails | two-sided |
      | Confidence level | 95% |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      M, SD per variable (r does not depend on them)
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking the scatterplot for outliers and non-linearity
      - Spearman, Kendall or robust alternatives
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: sample size only (no groups or cells; no individual values stored). This lowers the risk of identifying a participant; it does not remove it.

# the one-way appendix is stable

    Code
      print(appendix(s, choices = list(convention = "eta2")))
    Output
      ## Appendix: sufficient statistics for the one-way ANOVA
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      |  | ctrl | trt1 | trt2 |
      | --- | --- | --- | --- |
      | n | 10 | 10 | 10 |
      | M | 5.03 | 4.66 | 5.53 |
      | SD | 0.58 | 0.79 | 0.44 |
      
      | Result | Value |
      | --- | --- |
      | F(2, 27) | 4.85 |
      | p | .016 |
      | eta2, eta squared | .26 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Effect-size convention | eta squared (declaring it is a reprokit rule; APA does not require it) |
      | Variance assumption | equal variances (classical F test, not Welch's) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking the distribution and outliers within each group
      - checking equal variances beyond the group SDs
      - post-hoc comparisons that need the raw data or the error term of a model
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: group sizes. This lowers the risk of identifying a participant; it does not remove it.

# the chi-squared independence appendix is stable

    Code
      print(appendix(s))
    Output
      ## Appendix: sufficient statistics for the chi-squared test of independence
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      | Admit \ Gender | Male | Female |
      | --- | --- | --- |
      | Admitted | 1198 | 557 |
      | Rejected | 1493 | 1278 |
      
      | Result | Value |
      | --- | --- |
      | χ²(1) | 91.61 |
      | p | < .001 |
      | N | 4526 |
      | Cramér's V | .14 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Test | Yates' continuity correction |
      | Effect-size convention | Cramér's V (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking that the expected counts are large enough for the chi-squared approximation (expected counts below 5; reprokit does not test this)
      - exact, Fisher or Monte Carlo alternatives for sparse tables
      - any variable other than the ones in the table
      
      **Disclosure check.** No summary rests on fewer than 1 cases, and n is at least 2. Checked: cell counts (a count above 0 and below 1 is flagged; zeros are not). This lowers the risk of identifying a participant; it does not remove it.

# the chi-squared goodness-of-fit appendix is stable

    Code
      print(appendix(s))
    Output
      ## Appendix: sufficient statistics for the chi-squared goodness of fit
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      | cyl | 4 | 6 | 8 |
      | --- | --- | --- | --- |
      | n | 11 | 7 | 14 |
      
      | Result | Value |
      | --- | --- |
      | χ²(2) | 2.31 |
      | p | .315 |
      | N | 32 |
      
      ### B. Numbers APA does not require that the result needs
      
      | Number | Value | Why it is needed |
      | --- | --- | --- |
      | H0 proportions | 0.3333, 0.3333, 0.3333 | APA does not require this; the expected counts depend on them; not needed when they are equal |
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Effect-size convention | Cohen's w (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking that the expected counts are large enough for the chi-squared approximation (expected counts below 5; reprokit does not test this)
      - exact, Fisher or Monte Carlo alternatives for sparse tables
      - any variable other than the ones in the table
      
      **Disclosure check.** No summary rests on fewer than 1 cases, and n is at least 2. Checked: category counts (a count above 0 and below 1 is flagged; zeros are not). This lowers the risk of identifying a participant; it does not remove it.

# the factorial appendix is stable: balanced, then unbalanced with and without the SS type

    Code
      print(appendix(s, choices = list(convention = "partial_eta2")))
    Output
      ## Appendix: sufficient statistics for the factorial ANOVA, two factors
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      | supp | dose | n | M | SD |
      | --- | --- | --- | --- | --- |
      | OJ | 0.5 | 10 | 13.23 | 4.46 |
      | VC | 0.5 | 10 | 7.98 | 2.75 |
      | OJ | 1 | 10 | 22.70 | 3.91 |
      | VC | 1 | 10 | 16.77 | 2.52 |
      | OJ | 2 | 10 | 26.06 | 2.66 |
      | VC | 2 | 10 | 26.14 | 4.80 |
      
      | Effect | F(df1, df2) | p | partial eta squared |
      | --- | --- | --- | --- |
      | supp | F(1, 54) = 15.57 | < .001 | .22 |
      | dose | F(2, 54) = 92.00 | < .001 | .77 |
      | supp × dose | F(2, 54) = 4.11 | .022 | .13 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | SS type | not needed (balanced design: all types agree) |
      | Effect-size convention | partial eta squared (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - equal variances assumed (pooled error term)
      - checking the distribution and outliers within each cell
      - simple effects, post-hoc comparisons and contrasts that need the raw data or an error term other than the pooled one
      - covariates and any factor other than the two in the model
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: cell sizes. This lowers the risk of identifying a participant; it does not remove it.

---

    Code
      print(appendix(su))
    Output
      ## Appendix: sufficient statistics for the factorial ANOVA, two factors
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      | a | b | n | M | SD |
      | --- | --- | --- | --- | --- |
      | a1 | 1 | 8 | 9.95 | 1.45 |
      | a2 | 1 | 14 | 10.99 | 1.83 |
      | a1 | 2 | 20 | 11.33 | 2.03 |
      | a2 | 2 | 6 | 10.41 | 1.71 |
      | a1 | 3 | 12 | 12.30 | 2.26 |
      | a2 | 3 | 9 | 14.24 | 2.64 |
      
      | Effect | F(df1, df2) | p | effect size |
      | --- | --- | --- | --- |
      | a | not computed: declare the SS type (block C) | - | - |
      | b | not computed: declare the SS type (block C) | - | - |
      | a × b | F(2, 63) = 2.48 | .092 | not computed: declare the effect-size convention (block C) |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | SS type | not declared |
      | Effect-size convention | not declared (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - equal variances assumed (pooled error term)
      - checking the distribution and outliers within each cell
      - simple effects, post-hoc comparisons and contrasts that need the raw data or an error term other than the pooled one
      - covariates and any factor other than the two in the model
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: cell sizes. This lowers the risk of identifying a participant; it does not remove it.

---

    Code
      print(appendix(su, choices = list(ss_type = "II", convention = "partial_omega2")))
    Output
      ## Appendix: sufficient statistics for the factorial ANOVA, two factors
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      | a | b | n | M | SD |
      | --- | --- | --- | --- | --- |
      | a1 | 1 | 8 | 9.95 | 1.45 |
      | a2 | 1 | 14 | 10.99 | 1.83 |
      | a1 | 2 | 20 | 11.33 | 2.03 |
      | a2 | 2 | 6 | 10.41 | 1.71 |
      | a1 | 3 | 12 | 12.30 | 2.26 |
      | a2 | 3 | 9 | 14.24 | 2.64 |
      
      | Effect | F(df1, df2) | p | partial omega squared |
      | --- | --- | --- | --- |
      | a | F(1, 63) = 1.95 | .167 | .01 |
      | b | F(2, 63) = 9.64 | < .001 | .20 |
      | a × b | F(2, 63) = 2.48 | .092 | .04 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | SS type | type II |
      | Effect-size convention | partial omega squared (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - equal variances assumed (pooled error term)
      - checking the distribution and outliers within each cell
      - simple effects, post-hoc comparisons and contrasts that need the raw data or an error term other than the pooled one
      - covariates and any factor other than the two in the model
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: cell sizes. This lowers the risk of identifying a participant; it does not remove it.

# the regression-family appendices are stable

    Code
      print(appendix(sufficient_stats(lm(mpg ~ wt, mt), sharing_reason = sr)))
    Output
      ## Appendix: sufficient statistics for the simple regression
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      n = 32
      
      | Variable | M | SD |
      | --- | --- | --- |
      | wt | 3.22 | 0.98 |
      | mpg (outcome) | 20.09 | 6.03 |
      
      Correlations of the model columns and the outcome:
      
      |  | wt | mpg (outcome) |
      | --- | --- | --- |
      | wt | 1.00 | -.87 |
      | mpg (outcome) | -.87 | 1.00 |
      
      | Coefficient | b | SE | t | p |
      | --- | --- | --- | --- | --- |
      | (Intercept) | 37.29 | 1.88 | 19.86 | < .001 |
      | wt | -5.34 | 0.56 | -9.56 | < .001 |
      
      df = 30 for every coefficient. Each coefficient's confidence interval is b ± t(df) × SE at the level the paper states.
      
      | Model | Value |
      | --- | --- |
      | R² | .75 |
      | Adjusted R² | .74 |
      | F(1, 30) | 91.38 |
      | p (model) | < .001 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Confidence level | not declared |
      | Effect-size convention | not declared (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - residual diagnostics (normality, outliers, influence)
      - refitting with other predictors or transformations
      - heteroscedasticity-robust standard errors
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: factor levels (unweighted rows in the fit), counts of 0/1 columns and their pairwise tables. This lowers the risk of identifying a participant; it does not remove it.

---

    Code
      print(appendix(sufficient_stats(lm(mpg ~ wt, mt), sharing_reason = sr),
      choices = list(level = "0.95", convention = "r2")))
    Output
      ## Appendix: sufficient statistics for the simple regression
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      n = 32
      
      | Variable | M | SD |
      | --- | --- | --- |
      | wt | 3.22 | 0.98 |
      | mpg (outcome) | 20.09 | 6.03 |
      
      Correlations of the model columns and the outcome:
      
      |  | wt | mpg (outcome) |
      | --- | --- | --- |
      | wt | 1.00 | -.87 |
      | mpg (outcome) | -.87 | 1.00 |
      
      | Coefficient | b | SE | t | p |
      | --- | --- | --- | --- | --- |
      | (Intercept) | 37.29 | 1.88 | 19.86 | < .001 |
      | wt | -5.34 | 0.56 | -9.56 | < .001 |
      
      df = 30 for every coefficient. Each coefficient's confidence interval is b ± t(df) × SE at the level the paper states.
      
      | Model | Value |
      | --- | --- |
      | R² | .75 |
      | Adjusted R² | .74 |
      | F(1, 30) | 91.38 |
      | p (model) | < .001 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Confidence level | 95% |
      | Effect-size convention | R² (declaring it is a reprokit rule; APA does not require it) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - residual diagnostics (normality, outliers, influence)
      - refitting with other predictors or transformations
      - heteroscedasticity-robust standard errors
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: factor levels (unweighted rows in the fit), counts of 0/1 columns and their pairwise tables. This lowers the risk of identifying a participant; it does not remove it.

---

    Code
      print(appendix(sufficient_stats(lm(mpg ~ wt + hp + am, mt), sharing_reason = sr),
      choices = list(level = "0.99", convention = "adj_r2", coding = "treatment")))
    Output
      ## Appendix: sufficient statistics for the multiple regression
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      n = 32
      
      | Variable | M | SD |
      | --- | --- | --- |
      | wt | 3.22 | 0.98 |
      | hp | 146.69 | 68.56 |
      | am1 | 0.41 | 0.50 |
      | mpg (outcome) | 20.09 | 6.03 |
      
      Correlations of the model columns and the outcome:
      
      |  | wt | hp | am1 | mpg (outcome) |
      | --- | --- | --- | --- | --- |
      | wt | 1.00 | .66 | -.69 | -.87 |
      | hp | .66 | 1.00 | -.24 | -.78 |
      | am1 | -.69 | -.24 | 1.00 | .60 |
      | mpg (outcome) | -.87 | -.78 | .60 | 1.00 |
      
      | Coefficient | b | SE | t | p |
      | --- | --- | --- | --- | --- |
      | (Intercept) | 34.00 | 2.64 | 12.87 | < .001 |
      | wt | -2.88 | 0.90 | -3.18 | .004 |
      | hp | -0.04 | 0.01 | -3.90 | < .001 |
      | am1 | 2.08 | 1.38 | 1.51 | .141 |
      
      df = 28 for every coefficient. Each coefficient's confidence interval is b ± t(df) × SE at the level the paper states.
      
      | Model | Value |
      | --- | --- |
      | R² | .84 |
      | Adjusted R² | .82 |
      | F(3, 28) | 48.96 |
      | p (model) | < .001 |
      
      ### B. Numbers APA does not require that the result needs
      
      | Number | Value | Why it is needed |
      | --- | --- | --- |
      | the correlations | shown in block A | APA does not require this; coefficients depend on how the predictors overlap |
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Confidence level | 99% |
      | Effect-size convention | adjusted R² (declaring it is a reprokit rule; APA does not require it) |
      | Factor coding | am: contr.treatment |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - residual diagnostics (normality, outliers, influence)
      - refitting with other predictors or transformations
      - heteroscedasticity-robust standard errors
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: factor levels (unweighted rows in the fit), counts of 0/1 columns and their pairwise tables. This lowers the risk of identifying a participant; it does not remove it.

---

    Code
      print(appendix(sufficient_stats(lm(mpg ~ wt * hp, mt), sharing_reason = sr),
      choices = list(level = 0.9, convention = "r2", centering = "none", coding = "none")))
    Output
      ## Appendix: sufficient statistics for the moderation
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      n = 32
      
      | Variable | M | SD |
      | --- | --- | --- |
      | wt | 3.22 | 0.98 |
      | hp | 146.69 | 68.56 |
      | wt:hp | 514.74 | 341.94 |
      | mpg (outcome) | 20.09 | 6.03 |
      
      Correlations of the model columns and the outcome:
      
      |  | wt | hp | wt:hp | mpg (outcome) |
      | --- | --- | --- | --- | --- |
      | wt | 1.00 | .66 | .86 | -.87 |
      | hp | .66 | 1.00 | .93 | -.78 |
      | wt:hp | .86 | .93 | 1.00 | -.84 |
      | mpg (outcome) | -.87 | -.78 | -.84 | 1.00 |
      
      | Coefficient | b | SE | t | p |
      | --- | --- | --- | --- | --- |
      | (Intercept) | 49.81 | 3.61 | 13.82 | < .001 |
      | wt | -8.22 | 1.27 | -6.47 | < .001 |
      | hp | -0.12 | 0.02 | -4.86 | < .001 |
      | wt:hp | 0.03 | 0.01 | 3.75 | < .001 |
      
      df = 28 for every coefficient. Each coefficient's confidence interval is b ± t(df) × SE at the level the paper states.
      
      | Model | Value |
      | --- | --- |
      | R² | .88 |
      | Adjusted R² | .87 |
      | F(3, 28) | 71.66 |
      | p (model) | < .001 |
      
      ### B. Numbers APA does not require that the result needs
      
      | Number | Value | Why it is needed |
      | --- | --- | --- |
      | the correlations | shown in block A | APA does not require this; coefficients depend on how the predictors overlap |
      | the X·Z moments | shown in block A | APA does not require this; cannot be derived from X and Z summaries |
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Confidence level | 90% |
      | Effect-size convention | R² (declaring it is a reprokit rule; APA does not require it) |
      | Centering | as analysed |
      | Coding | no factor predictors |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - residual diagnostics (normality, outliers, influence)
      - refitting with other predictors or transformations
      - heteroscedasticity-robust standard errors
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: factor levels (unweighted rows in the fit), counts of 0/1 columns and their pairwise tables. This lowers the risk of identifying a participant; it does not remove it.

---

    Code
      print(appendix(sufficient_stats(mpg ~ wt, data = mt, test = "mediation",
      mediator = "hp", sharing_reason = sr), choices = list(level = "0.95")))
    Output
      ## Appendix: sufficient statistics for the simple mediation
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      n = 32
      
      | Variable | M | SD |
      | --- | --- | --- |
      | wt (X) | 3.22 | 0.98 |
      | hp (M) | 146.69 | 68.56 |
      | mpg (Y) | 20.09 | 6.03 |
      
      Correlations of X, M and Y:
      
      |  | wt | hp | mpg |
      | --- | --- | --- | --- |
      | wt | 1.00 | .66 | -.87 |
      | hp | .66 | 1.00 | -.78 |
      | mpg | -.87 | -.78 | 1.00 |
      
      | Path | b | SE | t | df | p |
      | --- | --- | --- | --- | --- | --- |
      | a (X to M) | 46.16 | 9.63 | 4.80 | 30 | < .001 |
      | b (M to Y, X held constant) | -0.03 | 0.01 | -3.52 | 29 | .001 |
      | c (X to Y) | -5.34 | 0.56 | -9.56 | 30 | < .001 |
      | c' (X to Y, M held constant) | -3.88 | 0.63 | -6.13 | 29 | < .001 |
      
      
      ### B. Numbers APA does not require that the result needs
      
      | Number | Value | Why it is needed |
      | --- | --- | --- |
      | the correlations | shown in block A | APA does not require this; the paths b and c′ depend on how X and M overlap; the means and SDs do not determine it |
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Confidence level | 95% |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      a·b and its CI (inferred by bootstrap, which resamples cases; summaries cannot)
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - the bootstrap (or Monte Carlo) confidence interval of the indirect effect a*b
      - residual diagnostics (normality, outliers, influence)
      - refitting with covariates, other mediators or transformations
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: sample size only (no groups or cells; no individual values stored). This lowers the risk of identifying a participant; it does not remove it.

# an undeclared effect-size convention is shown as not declared (one-way ANOVA)

    Code
      print(appendix(s))
    Output
      ## Appendix: sufficient statistics for the one-way ANOVA
      
      The APA Publication Manual (7th ed.) asks for "sufficient information to help readers fully understand the analyses conducted" (S3.7) and for data that "allow readers to confirm the basic reported analyses" (S6.43). When that set is extensive, "the information could be supplied in a supplementary data set or an appendix" (S3.7). This is that appendix.
      
      ### A. The numbers APA style asks for
      
      |  | ctrl | trt1 | trt2 |
      | --- | --- | --- | --- |
      | n | 10 | 10 | 10 |
      | M | 5.03 | 4.66 | 5.53 |
      | SD | 0.58 | 0.79 | 0.44 |
      
      | Result | Value |
      | --- | --- |
      | F(2, 27) | 4.85 |
      | p | .016 |
      | eta2, convention not declared | .26 |
      
      ### B. Numbers APA does not require that the result needs
      
      None: the APA numbers in block A determine the result.
      
      ### C. Analysis choices
      
      | Choice | Answer |
      | --- | --- |
      | Effect-size convention | not declared (declaring it is a reprokit rule; APA does not require it) |
      | Variance assumption | equal variances (classical F test, not Welch's) |
      
      ### D. APA numbers a reader cannot check from these statistics
      
      None: every number in block A follows from blocks A and B.
      
      ### Raw data
      
      **Why the raw data are not shared.** Consent did not cover public sharing.
      
      **What readers lose compared with the raw data:**
      
      - checking the distribution and outliers within each group
      - checking equal variances beyond the group SDs
      - post-hoc comparisons that need the raw data or the error term of a model
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. Checked: group sizes. This lowers the risk of identifying a participant; it does not remove it.

