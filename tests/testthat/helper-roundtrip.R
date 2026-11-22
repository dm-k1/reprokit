# Round-trip contract (plan section 6, ADR 0005): for every handler that claims
# exact reproduction, reproduce(sufficient_stats(model)) must match the original with a scaled
# difference |original - reproduced| / max(1, |original|) below 1e-8.
# Absolute for numbers up to 1, relative above (a large F carries floating-point
# noise in absolute terms). Rounding the stored statistics is a bug; this catches it.

expect_reproduces <- function(original, reproduced, tolerance = 1e-8) {
  act <- testthat::quasi_label(rlang::enquo(reproduced), arg = "reproduced")
  if (!identical(names(original), names(act$val))) {
    testthat::fail(sprintf("%s reports different quantities: %s vs %s.", act$lab,
                           paste(names(original), collapse = ","),
                           paste(names(act$val), collapse = ",")))
    return(invisible(act$val))
  }
  diff <- roundtrip_diff(original, act$val)
  testthat::expect(
    all(diff <= tolerance),
    sprintf(paste0("%s differs from the original by a scaled difference of up to %g ",
                   "(tolerance %g), at %s."),
            act$lab, max(diff), tolerance, names(diff)[which.max(diff)])
  )
  invisible(act$val)
}

# Scaled difference per element: |original - reproduced| / max(1, |original|).
# Equal values (including Inf CI bounds) give 0.
roundtrip_diff <- function(original, reproduced) {
  o <- unlist(original)
  r <- unlist(reproduced)
  ifelse(o == r, 0, abs(o - r) / pmax(1, abs(o)))
}

# A registered handler that exists only for the duration of one test.
local_handler <- function(name, ..., env = parent.frame()) {
  register_handler(name, ...)
  withr::defer(unregister_handler(name), envir = env)
  invisible(name)
}

valid_stats_args <- function() {
  list(
    handler = "example",
    verdict = "summaries_suffice",
    verdict_reason = "Group means and SDs determine the t statistic.",
    stats = list(n = c(20, 22), mean = c(5.1, 4.3), sd = c(1.2, 1.4)),
    sharing_reason = "Consent form did not cover public sharing.",
    lost = c("outlier inspection", "alternative analyses")
  )
}
