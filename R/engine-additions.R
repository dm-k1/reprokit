# The additions of a verdict (ADR 0032): the quantities the stored summaries need beyond what
# APA style asks a paper to report, each with why it is needed, the exact identity that covers
# it (if any) and, for one that is not covered, the range the APA numbers imply for it.
# Computed from the handler's declarations, never per test.

# One entry per applicable addition: quantity, why, covered (the identity's words or NA),
# status ("covered", "implied" or "missing") and range (numeric length 2, or NULL). `s` is the
# summary (list(stats, meta)); with it, the range is computed from the author's data rounded
# at APA precision, by check_reported()'s machinery (derive_implied_inputs, implied_ranges).
addition_report <- function(handler, s = NULL) {
  applicable <- applicable_additions(handler$additions, s)
  lapply(stats::setNames(nm = names(applicable)), function(q) {
    a <- applicable[[q]]
    covered <- !is.null(a$covered)
    range <- if (!covered && !is.null(s) && !is.null(a$range)) {
      tryCatch(a$range(s), error = function(e) NULL)
    }
    list(quantity = q, why = a$why, covered = if (covered) a$covered else NA_character_,
         status = if (covered) "covered" else if (!is.null(range)) "implied" else "missing",
         range = range)
  })
}

# "between .60 and .64", without the leading zero when both ends are within -1 to 1.
format_addition_range <- function(range) {
  bounded <- all(abs(range) <= 1)
  both <- vapply(range, function(x) {
    out <- formatC(x, format = "f", digits = 2L)
    if (bounded) sub("^(-?)0\\.", "\\1.", out) else out
  }, character(1))
  paste0("between ", both[1L], " and ", both[2L])
}

# A quantity as a reader writes it in the narration.
addition_label <- function(handler, quantity) {
  display_quantities(handler, quantity)
}

# The sentence about one addition for the headline.
addition_sentence <- function(handler, a) {
  label <- addition_label(handler, a$quantity)
  why <- paste0(" (needed because ", sub("\\.$", "", a$why), ")")
  switch(a$status,
    covered = paste0(label, " follows from the APA numbers by an exact identity: ", a$covered, "."),
    present = paste0(label, " is reported, so the result can be reproduced exactly", why, "."),
    implied = paste0(label, " is implied ", format_addition_range(a$range),
                     " by the APA numbers; reporting it makes the result exact", why, "."),
    paste0(label, " is not implied by the APA numbers; reporting it makes the result exact",
           why, ".")
  )
}

# The narration's headline for a tier, one line per addition.
tier_lines <- function(handler, verdict, additions) {
  lines <- unlist(lapply(additions, function(a) paste0("  * ", addition_sentence(handler, a))),
                  use.names = FALSE)
  if (identical(verdict, "apa_suffices")) {
    c("The numbers APA style asks a paper to report suffice to reproduce this analysis exactly.",
      lines)
  } else {
    c(paste("The APA numbers alone do not reproduce this analysis exactly;",
            "these numbers beyond them are needed:"),
      lines)
  }
}
