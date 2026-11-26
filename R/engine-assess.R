# The assessment behind sufficient_stats(): which handler, can summaries suffice, and the
# summaries when they do. It never errors on the verdict (a handler that fails becomes a
# needs_raw_data verdict). Invalid input (a formula without `data` or with an unknown
# `test`, a result without `data`, a conflicting explicit choice) is an
# abort_input() error, raised by prepare_input() and below.
assess <- function(x, ..., threshold = 5) {
  prep <- prepare_input(x, list(...))
  refuse <- function(reason, handler = NULL) {
    list(verdict = "needs_raw_data", reason = reason,
         handler = if (is.null(handler)) NA_character_ else handler$name,
         publish = NA_character_,
         lost = if (is.null(handler)) "exact reproduction of this analysis" else handler$lost,
         summary = NULL, disclosure = list(threshold = threshold), prep = prep)
  }
  if (!is.null(prep$problem)) return(refuse(prep$problem))
  handler <- do.call(find_handler, c(list(prep$x), prep$dots))
  if (is.null(handler)) {
    if (inherits(x, "formula")) {
      abort_input(no_handler_reason(x))
    }
    return(refuse(no_handler_reason(x)))
  }
  # The behaviour rule (plan 5.4): the verdict path never crashes, so a failing handler is
  # reported as a verdict instead of an error.
  res <- tryCatch(
    assess_with(handler, prep, threshold),
    error = function(e) {
      list(verdict = "needs_raw_data",
           reason = paste0("The ", analysis_name(handler),
                           " could not be assessed: ", conditionMessage(e)))
    }
  )
  list(verdict = res$verdict, reason = res$reason, handler = handler$name,
       additions = res$additions, publish = handler$needs, lost = handler$lost,
       summary = res$summary,
       disclosure = c(res$disclosure, list(threshold = threshold)), prep = prep)
}

assess_with <- function(handler, prep, threshold) {
  call_with <- function(f, ...) do.call(f, c(list(prep$x), prep$dots, list(...)))
  res <- if (is.null(handler$triage)) {
    list(verdict = handler$verdict,
         reason = if (verdict_suffices(handler$verdict)) {
           paste0("Summary statistics suffice: ", handler$needs,
                  " determine the reported results exactly.")
         } else {
           paste0("The ", analysis_name(handler), " cannot be reproduced from summary ",
                  "statistics alone.")
         })
  } else {
    call_with(handler$triage, threshold = threshold)
  }
  if (!verdict_suffices(res$verdict) || is.null(handler$summarise)) return(res)
  summary <- call_with(handler$summarise)
  mismatch <- match_problem(handler, summary, prep)
  if (!is.null(mismatch)) {
    return(list(verdict = "needs_raw_data", reason = mismatch, summary = NULL))
  }
  # Decided per dataset: summaries of tiny samples or tiny groups are close to the raw data,
  # so they protect no one and the verdict drops. First the general minimum n (every
  # handler), then the handler's own check of its groups or cells.
  problems <- minimum_n_problem(summary$meta$n, threshold)
  disclosure <- list()
  if (length(problems) == 0L && !is.null(handler$disclosure)) {
    disclosure <- handler$disclosure(summary, threshold)
    problems <- disclosure$problems
  }
  if (length(problems) > 0L) {
    return(list(
      verdict = "needs_raw_data",
      reason = paste0(
        "The summaries would be close to the raw data, so they give no real privacy ",
        "gain, only less information: ", paste(problems, collapse = "; "), "."
      ),
      disclosure = c(disclosure, list(problems = problems)),
      summary = NULL
    ))
  }
  summary$meta$route <- prep$route
  res$summary <- summary
  res$disclosure <- disclosure
  # The tier follows from the handler's declared additions and, for the goodness-of-fit
  # test, from the summary itself (uniform H0 proportions are no addition): ADR 0032.
  res$verdict <- tier_from_additions(handler$additions, summary)
  res$additions <- addition_report(handler, summary)
  res
}

# General minimum sample size, for every handler: n >= 2 * threshold.
# With n close to the number of stored statistics, the summaries pin down the
# rows (n = 2: mean and SD give both values exactly).
minimum_n <- function(threshold) 2 * threshold

minimum_n_problem <- function(n, threshold) {
  if (is.null(n) || is.na(n)) {
    return("the sample size n could not be determined")
  }
  if (n < minimum_n(threshold)) {
    return(paste0("total n = ", n, " is below the minimum of ", minimum_n(threshold),
                  " (2 x threshold)"))
  }
  character()
}

# The disclosure hook of a handler without groups or cells (ADR 0023 point 5): it adds no
# problems, and says that the general minimum n is the whole check.
disclosure_sample_size_only <- function(summary, threshold) {
  list(
    passed = TRUE,
    checked = "sample size only (no groups or cells; no individual values stored)",
    problems = character()
  )
}

no_handler_reason <- function(x) {
  hint <- if (inherits(x, "formula")) {
    paste0(" For a formula, say which test with `test =`: ",
           paste0('"', formula_tests(), '"', collapse = ", "), ".")
  } else if (inherits(x, "htest")) {
    paste0(" Pass the result of t.test(), cor.test() or chisq.test(); a t.test or cor.test ",
           "result also needs `data`.")
  } else {
    ""
  }
  paste0(
    "reprokit does not cover ", paste(class(x), collapse = "/"),
    " objects, so it cannot show that summary statistics are enough.", hint
  )
}

# Tests a formula can be matched to, collected from the registered handlers.
formula_tests <- function() {
  unlist(lapply(handlers(), function(n) get_handler(n)$formula_tests), use.names = FALSE)
}

# Handlers are tried in alphabetical order, the narrow ones before a general
# `fallback` handler (the general `lm`), so the most specific match wins whatever the
# names are.
find_handler <- function(x, ...) {
  hs <- lapply(handlers(), get_handler)
  hs <- hs[order(vapply(hs, function(h) isTRUE(h$fallback), logical(1)))]
  for (h in hs) {
    if (isTRUE(tryCatch(h$matches(x, ...), error = function(e) FALSE))) return(h)
  }
  NULL
}
