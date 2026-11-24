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
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. This lowers the risk of identifying a participant; it does not remove it.

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
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. This lowers the risk of identifying a participant; it does not remove it.

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
      
      **Disclosure check.** No summary rests on fewer than 5 cases, and n is at least 10. This lowers the risk of identifying a participant; it does not remove it.

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

