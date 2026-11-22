# Round-trip contract (plan section 6): for every handler that claims
# exact reproduction, refit(kit(model)) must match the original within 1e-8.
# Rounding the stored statistics is a bug, and this tolerance catches it.

expect_reproduces <- function(original, refitted, tolerance = 1e-8) {
  act <- testthat::quasi_label(rlang::enquo(refitted), arg = "refitted")
  diff <- abs(unlist(original) - unlist(act$val))
  testthat::expect(
    length(original) == length(act$val) && all(diff <= tolerance),
    sprintf("%s differs from the original by up to %g (tolerance %g).",
            act$lab, max(diff), tolerance)
  )
  invisible(act$val)
}

# A registered handler that exists only for the duration of one test.
local_handler <- function(name, ..., env = parent.frame()) {
  register_handler(name, ...)
  withr::defer(unregister_handler(name), envir = env)
  invisible(name)
}

valid_kit_args <- function() {
  list(
    handler = "example",
    verdict = "summaries_suffice",
    verdict_reason = "Group means and SDs determine the t statistic.",
    stats = list(n = c(20, 22), mean = c(5.1, 4.3), sd = c(1.2, 1.4)),
    sharing_reason = "Consent form did not cover public sharing.",
    lost = c("outlier inspection", "alternative analyses")
  )
}
