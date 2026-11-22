# Round-trip contract (plan section 6): for every handler that claims
# exact reproduction, refit(kit(model)) must match the original within 1e-8.
# Rounding the stored statistics is a bug, and this tolerance catches it.

expect_reproduces <- function(original, refitted, tolerance = 1e-8) {
  act <- testthat::quasi_label(rlang::enquo(refitted), arg = "refitted")
  if (!identical(names(original), names(act$val))) {
    testthat::fail(sprintf("%s reports different quantities: %s vs %s.", act$lab,
                           paste(names(original), collapse = ","),
                           paste(names(act$val), collapse = ",")))
    return(invisible(act$val))
  }
  diff <- roundtrip_diff(original, act$val)
  testthat::expect(
    all(diff <= tolerance),
    sprintf("%s differs from the original by up to %g (tolerance %g), at %s.",
            act$lab, max(diff), tolerance, names(diff)[which.max(diff)])
  )
  invisible(act$val)
}

# |original - refitted| per element; equal values (including Inf CI bounds) give 0.
roundtrip_diff <- function(original, refitted) {
  o <- unlist(original)
  r <- unlist(refitted)
  ifelse(o == r, 0, abs(o - r))
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
