# The appendix of a sufficient-statistics object (ADR 0010, ADR 0013 point 6).
# In one line: appendix = what APA asks for (A) + what APA leaves out but the
# result needs (B) + the choices behind the numbers (C) + what stays uncheckable
# (D), then why the raw data are not shared and what readers lose.
# Every block comes from the registry, so the appendix matches what the package
# does; only the author's answers (sharing reason, effect-size convention) are
# typed in.

#' Write the reporting appendix for a sufficient-statistics object
#'
#' The APA Publication Manual (seventh edition, section 3.7) says that when the sufficient set
#' of statistics is extensive, "the information could be supplied in a
#' supplementary data set or an appendix". `appendix()` writes that appendix as
#' markdown, in four blocks generated from reprokit's test definitions:
#'
#' * **A.** the numbers APA style asks for, with their values;
#' * **B.** the numbers APA does not require but the result needs, with their
#'   values and the reason;
#' * **C.** the analysis choices, with the author's answers;
#' * **D.** the APA numbers a reader cannot check from these statistics.
#'
#' It ends with the reason for not sharing the raw data, what readers lose, and
#' the disclosure check.
#'
#' @param x A fitted model or test result (what [sufficient_stats()] accepts: `lm`, `aov`,
#'   `chisq.test`, `t.test` / `cor.test` with `data`; two `lm` fits for a mediation, the
#'   second one in `...`), a formula with `data` and `test`, or an object made by
#'   [sufficient_stats()].
#' @param ... Passed to [sufficient_stats()] when `x` is not already its object (`data`,
#'   `test`, the second model of a mediation, the choices of a formula analysis).
#' @param sharing_reason Optional: why the raw data are not shared, in the author's
#'   words. reprokit does not suggest reasons. A missing or blank one gives a warning and
#'   the appendix line "not stated by the author". What
#'   readers lose is not an argument: reprokit states it, from the test (see
#'   `sufficient_stats(...)$lost`).
#' @param choices Named list of answers the object cannot know. `convention`:
#'   the effect-size convention, as a variant code (e.g. `"d_z"`, `"d_av"`).
#'   Declaring it is a reprokit rule, not an APA requirement. `ss_type`: for a
#'   factorial ANOVA, the type of sums of squares (`"III"`, `"II"` or `"I"`); only
#'   an unbalanced design needs it. Left out, the appendix uses type III and says
#'   it is reprokit's default, not the author's answer; checking a paper
#'   never assumes a type. `level`: for a regression or mediation, the
#'   confidence level of the paper's intervals (`"0.95"`, `"0.90"` or `"0.99"`);
#'   for a regression, `convention` is `"r2"` or `"adj_r2"` (the R-squared the paper
#'   reports). Choices recorded in a fitted result (tails, level, test variant, test value,
#'   continuity correction, H0 proportions) are read from it and marked "read from the
#'   result"; an explicit choice that conflicts with one is refused.
#' @param digits Decimals for the APA numbers in block A (p always gets 3).
#' @param digits_added Decimals for the added numbers in block B. They feed
#'   back into the result, so they get more decimals than APA's two; the object
#'   itself keeps full precision.
#' @param threshold Passed to [sufficient_stats()] when it is called here.
#' @param extra Names of further numbers to add after block D, or `"all"`. They are
#'   quantities of the test that APA style does not ask for and that the stored summaries
#'   determine: the R-squared family (`"r2"`, `"adj_r2"`, `"eta2"`, `"omega2"`,
#'   `"partial_eta2"`, `"partial_omega2"`), the F of each mediation model, or `"beta"` for
#'   the standardized coefficients of a regression. `coverage_table()` lists them per test
#'   (column `extras`). A name the test does not offer is an error that lists the ones it
#'   does. The default adds nothing, and the rest of the appendix does not change.
#' @return A character vector of markdown lines, class `reprokit_appendix`. Refuses with an
#'   error of class `reprokit_impossible` when summaries do not suffice.
#' @export
#' @examples
#' appendix(chisq.test(table(mtcars$am, mtcars$vs)),
#'          sharing_reason = "Consent did not cover public sharing.")
#'
#' # Add the standardized coefficients (and, for other tests, the R-squared family)
#' appendix(lm(mpg ~ wt + hp, data = mtcars), extra = "beta",
#'          sharing_reason = "Consent did not cover public sharing.")
#'
#' sleep_wide <- data.frame(drug1 = sleep$extra[sleep$group == 1],
#'                          drug2 = sleep$extra[sleep$group == 2])
#' appendix(Pair(drug1, drug2) ~ 1, data = sleep_wide, test = "paired",
#'          sharing_reason = "Consent did not cover public sharing.",
#'          choices = list(convention = "d_z"))
appendix <- function(x, ..., sharing_reason, choices = list(), digits = 2L,
                     digits_added = 4L, threshold = 5, extra = character()) {
  reason <- if (!missing(sharing_reason) && is.character(sharing_reason) &&
                  length(sharing_reason) == 1L && !is.na(sharing_reason) &&
                  nzchar(trimws(sharing_reason))) trimws(sharing_reason) else NA_character_
  if (is.na(reason)) {
    warning("No `sharing_reason` was given. The appendix says the author did not state one; ",
            "reprokit never writes a reason for you.", call. = FALSE)
  }
  if ("lost" %in% names(list(...))) {
    abort_input(
      "`lost` is not an argument of appendix(): reprokit states what readers lose, ",
      "from the test."
    )
  }
  s <- if (is_sufficient_stats(x)) x else sufficient_stats(x, ..., threshold = threshold)
  require_summaries(s, "write an appendix")
  choices <- appendix_choices(s, choices)
  h <- get_handler(s$handler)
  if (is.null(h$design)) {
    abort_input("No appendix for the ", analysis_name(h), " yet: its APA set is not declared.")
  }
  extra <- appendix_extra_names(h, extra)
  # Printed numbers come from the model when there is one; the guard refuses a fit whose own
  # numbers differ from the ones recomputed from its summaries (ADR 0031 point 5).
  reference <- if (is_sufficient_stats(x)) {
    NULL
  } else {
    appendix_model_reference(x, list(...), s$handler)
  }
  guard_model_source(reference, appendix_recomputed(h, s))
  with_model_source(reference, s$handler, appendix_lines(h, s, choices, extra, reason,
                                                         digits, digits_added))
}

# The recomputed values the model's numbers are held to: reproduce(), plus the per-model
# R squared and F of a mediation (read by its extras).
appendix_recomputed <- function(h, s) {
  r <- h$reproduce(s)
  if (identical(h$name, "mediation_simple")) {
    m <- mediation_model_values(s)
    r <- c(r, stats::setNames(c(m[, "r2"], m[, "f"]), c(paste0("r2:", 1:3), paste0("F:", 1:3))))
  }
  r
}

appendix_lines <- function(h, s, choices, extra, reason, digits, digits_added) {
  answers <- appendix_answers(h, s, choices)
  vals <- appendix_values(h, s, answers)
  labels <- appendix_cell_labels(h, s)
  lines <- c(
    paste0("## Appendix: sufficient statistics for the ", appendix_analysis_name(h)),
    "",
    paste("The APA Publication Manual (7th ed.) asks for \"sufficient information to help",
          "readers fully understand the analyses conducted\" (\u00a73.7) and for data that",
          "\"allow readers to confirm the basic reported analyses\" (\u00a76.43). When that",
          "set is extensive, \"the information could be supplied in a supplementary data set",
          "or an appendix\" (\u00a73.7). This is that appendix."),
    "",
    appendix_block_a(h, s, vals, answers, labels, digits),
    appendix_block_b(h, vals, labels, digits_added),
    appendix_block_c(h, s, answers, labels),
    appendix_block_d(h),
    appendix_block_extras(h, s, answers, extra, digits),
    appendix_sharing(s, reason)
  )
  structure(lines, class = "reprokit_appendix")
}

#' @export
print.reprokit_appendix <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}

# The extras to print (ADR 0031): `extra` is "all" or names the test offers (`h$extras`).
appendix_extra_names <- function(h, extra) {
  if (is.null(extra) || length(extra) == 0L) return(character())
  if (!is.character(extra) || anyNA(extra) || !all(nzchar(extra))) {
    abort_input("`extra` must be names of extra numbers, or \"all\".")
  }
  offered <- names(h$extras)
  if (identical(extra, "all")) return(offered)
  unknown <- setdiff(extra, offered)
  if (length(unknown) > 0L) {
    abort_input(
      "The ", analysis_name(h), " has no extra ", paste0("`", unknown, "`", collapse = ", "), ". ",
      if (length(offered) > 0L) {
        paste0("Valid extras: ", paste0("`", offered, "`", collapse = ", "), ", or \"all\".")
      } else {
        "It offers no extras."
      }
    )
  }
  unique(extra)
}

# An effect size as APA style writes it. The appendix is plain markdown, so no symbol is
# italicised (t, p, F, R squared and d are plain throughout); Greek letters are the symbols.
appendix_symbol <- function(code) {
  symbols <- c(eta2 = "\u03b7\u00b2", omega2 = "\u03c9\u00b2",
               partial_eta2 = "partial \u03b7\u00b2",
               partial_omega2 = "partial \u03c9\u00b2", r2 = "R\u00b2", adj_r2 = "adjusted R\u00b2",
               beta = "\u03b2 (standardized)")
  if (code %in% names(symbols)) symbols[[code]] else code
}

# The requested extras as a table after block D: each from the stored summaries, formatted as
# APA asks (no leading zero for a number that cannot exceed 1; adjusted R squared may be negative).
appendix_block_extras <- function(h, s, answers, extra, digits) {
  if (length(extra) == 0L) return(character())
  rows <- unlist(lapply(extra, function(q) {
    spec <- h$extras[[q]]
    value <- spec$value(s, answers, digits)
    if (is.null(value) || !all(is.finite(value))) {
      return(appendix_row(c(spec$label, "not determined by the stored summaries")))
    }
    labels <- if (is.null(names(value))) {
      spec$label
    } else if (grepl(")$", spec$label)) {
      paste0(spec$label, ", ", names(value))
    } else {
      paste0(spec$label, " (", names(value), ")")
    }
    vapply(seq_along(value), function(i) {
      appendix_row(c(labels[i], appendix_number(value[[i]], digits,
                                                leading_zero = !spec$bounded)))
    }, character(1), USE.NAMES = FALSE)
  }), use.names = FALSE)
  c("### Additional numbers", "",
    "Computed from the stored summaries at the author's request.", "",
    appendix_row(c("Number", "Value")), appendix_row(c("---", "---")), rows, "")
}

# The analysis name in running text: no parenthetical, first letter lower case
# (proper names such as Pearson keep their capital).
appendix_analysis_name <- function(h) {
  name <- sub(" \\(.*\\)$", "", h$analysis)
  if (grepl("^(Pearson|Spearman|Kendall)", name)) return(name)
  paste0(tolower(substr(name, 1L, 1L)), substring(name, 2L))
}

# The author's answer per variant kind, as a variant code (NA = not declared).
# Tails, level and test come from how the analysis was run; the effect-size
# convention only the author knows.
appendix_answers <- function(h, s, choices) {
  kinds <- unique(unlist(lapply(h$relations, function(rel) names(rel$variants))))
  convention_codes <- variant_codes(h, "convention")
  if (!is.null(choices$convention) && length(convention_codes) == 0L) {
    abort_input(
      "`", h$name, "` has no effect-size convention to declare; drop `choices$convention`."
    )
  }
  # A handler with one convention (V, w) has nothing for the author to choose.
  convention <- if (!is.null(choices$convention)) {
    choices$convention
  } else if (length(convention_codes) == 1L) {
    convention_codes
  } else {
    NA_character_
  }
  if (!is.na(convention) && !convention %in% convention_codes) {
    abort_input(
      "`choices$convention` must be one of: ",
      paste0("\"", convention_codes, "\"", collapse = ", "), "."
    )
  }
  test <- if (is.null(s$meta$test_variant)) s$meta$test else s$meta$test_variant
  ss_codes <- variant_codes(h, "test")
  if (has_ss_types(h)) {
    # Share side (ADR 0025): a declared SS type (or a stored one) counts; otherwise reprokit's
    # default, type III, is used and shown as such. The check side never assumes one.
    ss_defaulted <- is.null(choices$ss_type) && is.null(s$meta$ss_type)
    test <- if (!is.null(choices$ss_type)) choices$ss_type else
      if (!is.null(s$meta$ss_type)) s$meta$ss_type else ss_type_codes()[1L]
    if (!is.na(test) && !test %in% ss_codes) {
      abort_input(
        "`choices$ss_type` must be one of: ",
        paste0("\"", ss_codes, "\"", collapse = ", "), "."
      )
    }
  } else if (!is.null(choices$ss_type)) {
    abort_input("`", h$name, "` has no SS type to declare; drop `choices$ss_type`.")
  }
  # A regression's CI level is not an argument of the analysis: the author declares the
  # level the paper's intervals use (`choices$level`), like the convention.
  level <- if (!is.null(h$design$terms)) {
    appendix_level(choices$level)
  } else if (is.null(s$meta$conf_level)) {
    NULL
  } else {
    sprintf("%.2f", s$meta$conf_level)
  }
  if (is.null(h$design$terms) && !is.null(choices$level) &&
        is.null(attr(s$choices_read, "fn"))) {
    abort_input(
      "`", h$name, "` takes its confidence level from the analysis; drop `choices$level`."
    )
  }
  answers <- list(
    tail = s$meta$alternative,
    level = level,
    test = test,
    convention = convention
  )
  answers <- answers[intersect(names(answers), kinds)]
  attr(answers, "ss_defaulted") <- has_ss_types(h) && ss_defaulted
  answers
}

# The author's CI level as a variant code ("0.95"), NA when not declared.
appendix_level <- function(level) {
  if (is.null(level)) return(NA_character_)
  code <- if (is.numeric(level)) sprintf("%.2f", level) else level
  if (length(code) != 1L || !code %in% confidence_levels()) {
    abort_input(
      "`choices$level` must be one of: ",
      paste0("\"", confidence_levels(), "\"", collapse = ", "), " (or 0.90, 0.95, 0.99)."
    )
  }
  code
}

# Whether the handler's `test` variants are SS types (a factorial ANOVA).
has_ss_types <- function(h) {
  codes <- variant_codes(h, "test")
  length(codes) > 0L && all(codes %in% ss_type_codes())
}

# Every quantity's value: the stored descriptives, then what reproduce() returns
# (the round-trip-tested path), then any APA quantity still missing (the effect
# size) from a relation evaluated at the author's answers.
appendix_values <- function(h, s, answers) {
  stats <- s$stats
  names(stats)[names(stats) == "mean"] <- "m"
  vals <- stats[intersect(names(stats), quantity_names())]
  reproduced <- as.list(source_reproduce(s, h))
  for (q in setdiff(names(reproduced), names(vals))) vals[[q]] <- reproduced[[q]]
  for (q in intersect(names(s$meta), c("p0"))) vals[[q]] <- s$meta[[q]]
  args <- s$meta[intersect(h$test_args, names(s$meta))]
  relations <- relations_at(h, Filter(Negate(is.null), args))
  for (rel in relations) {
    if (!is.null(vals[[rel$output]]) || !all(rel$inputs %in% names(vals))) next
    variant <- appendix_variant(rel, answers)
    if (!is.na(variant)) vals[[rel$output]] <- rel$fn(vals, variant)
  }
  vals
}

# The variant of a relation the author's answers pick, or NA when the relation
# varies over a kind the author did not answer, or over more than one kind.
appendix_variant <- function(rel, answers) {
  if (length(rel$variants) == 0L) return("")
  if (length(rel$variants) > 1L) return(NA_character_)
  kind <- names(rel$variants)
  answer <- answers[[kind]]
  if (is.null(answer) || is.na(answer) || !answer %in% rel$variants[[kind]]) {
    return(NA_character_)
  }
  answer
}

# Names of the conditions (or variables), as the analysis wrote them.
appendix_cell_labels <- function(h, s) {
  if (is.null(h$cells) || !is.null(h$appendix_table)) return(character())
  labels <- names(s$stats$mean)
  if (is.null(labels)) {
    groups <- if (is.na(h$cells)) length(s$stats$mean) else h$cells
    labels <- as.character(seq_len(groups))
  }
  # Bare numeric levels ("1" - "2") read as arithmetic in the output, so name them.
  if (!anyNA(suppressWarnings(as.numeric(labels)))) labels <- paste("group", labels)
  labels
}

appendix_block_a <- function(h, s, vals, answers, labels, digits) {
  out <- c("### A. The numbers APA style asks for", "")
  if (!is.null(h$appendix_table)) {
    out <- c(out, h$appendix_table(s), "")
  } else if (identical(h$cells, 1L)) {
    out <- c(out, appendix_row(c("n", "M", "SD")), appendix_row(rep("---", 3L)),
             appendix_row(c(vals$n, appendix_number(vals$m, digits),
                            appendix_number(vals$sd, digits))), "")
  } else if (length(labels) > 0L) {
    rows <- c(if (length(vals$n) > 1L) appendix_row(c("n", vals$n)),
              appendix_row(c("M", appendix_number(vals$m, digits))),
              appendix_row(c("SD", appendix_number(vals$sd, digits))))
    out <- c(out, appendix_row(c("", labels)),
             appendix_row(rep("---", length(labels) + 1L)), rows, "")
    if (length(vals$n) == 1L) out <- c(out, paste0("n = ", vals$n), "")
  }
  if (!is.null(h$appendix_effects)) return(c(out, h$appendix_effects(s, answers, digits), ""))
  statistic <- h$design$statistic
  stat_label <- c(t = "t", f = "F", r = "r", chisq = "\u03c7\u00b2")[[statistic]]
  es <- h$design$effect_size
  es_row <- if (is.null(es) || identical(es, statistic)) NULL else if (is.null(vals[[es]])) {
    c(es, "not computed: declare the effect-size convention (block C)")
  } else {
    es_label <- if (is.na(answers$convention)) "convention not declared" else
      variant_label(answers$convention)
    c(if (es == "v") es_label else paste0(appendix_symbol(es), ", ", es_label),
      appendix_number(vals[[es]], digits, leading_zero = !es %in% c("eta2", "v")))
  }
  level <- if (is.null(s$meta$conf_level)) "" else
    paste0(variant_label(sprintf("%.2f", s$meta$conf_level)), " ")
  lz <- statistic != "r"
  ci <- if (isTRUE(h$design$ci)) {
    paste0("[", appendix_number(vals$ci_lower, digits, lz), ", ",
           appendix_number(vals$ci_upper, digits, lz), "]")
  }
  df_text <- if (identical(statistic, "f")) {
    paste0(appendix_df(vals$df1, digits), ", ", appendix_df(vals$df2, digits))
  } else {
    appendix_df(vals$df, digits)
  }
  c(out,
    appendix_row(c("Result", "Value")), appendix_row(c("---", "---")),
    appendix_row(c(paste0(stat_label, "(", df_text, ")"),
                   appendix_number(vals[[statistic]], digits, leading_zero = statistic != "r"))),
    appendix_row(c("p", appendix_p(vals$p))),
    if ("n_total" %in% h$apa) appendix_row(c("N", vals$n_total)),
    if (!is.null(es_row)) appendix_row(es_row),
    if (isTRUE(h$design$ci)) {
      appendix_row(c(paste0(level, "CI", appendix_ci_of(h, s, labels)), ci))
    },
    "")
}

# What the CI is an interval for. The registry does not say (it knows the bounds,
# not their target), so this is a small lookup by handler name; a handler without
# an entry gets no description rather than a wrong one.
appendix_ci_of <- function(h, s, labels) {
  switch(h$name,
         t_paired = paste0(" of the mean difference (", labels[1L], " \u2212 ", labels[2L], ")"),
         t_one_sample = {
           mu <- s$meta$mu
           paste0(" of the mean",
                  if (!is.null(mu) && mu != 0) paste0(" (test value mu = ", mu, ")"))
         },
         t_independent = paste0(" of the mean difference (", labels[1L], " \u2212 ",
                                labels[2L], ")"),
         cor_pearson = " of r",
         "")
}

appendix_block_b <- function(h, vals, labels, digits) {
  out <- c("### B. Numbers APA does not require that the result needs", "")
  missing <- missing_quantities(h)
  if (length(missing) == 0L) {
    return(c(out, "None: the APA numbers in block A determine the result.", ""))
  }
  reasons <- handler_missing_reasons(h)
  rows <- vapply(missing, function(q) {
    name <- if (q == "r" && length(labels) == 2L) {
      paste0("r between ", labels[1L], " and ", labels[2L])
    } else {
      display_quantities(h, q)
    }
    value <- if (is.null(vals[[q]])) {
      "shown in block A"
    } else {
      paste(appendix_number(vals[[q]], digits, leading_zero = q != "r"), collapse = ", ")
    }
    appendix_row(c(name, value,
                   paste0("APA does not require this; ", reasons[[q]])))
  }, character(1), USE.NAMES = FALSE)
  c(out, appendix_row(c("Number", "Value", "Why it is needed")),
    appendix_row(c("---", "---", "---")), rows, "")
}

appendix_block_c <- function(h, s, answers, labels) {
  kind_labels <- c(tail = "Tails", level = "Confidence level", test = "Test",
                   convention = "Effect-size convention")
  if (has_ss_types(h)) kind_labels[["test"]] <- "SS type"
  fn <- attr(s$choices_read, "fn")
  rows <- vapply(names(answers), function(kind) {
    answer <- answers[[kind]]
    text <- if (is.na(answer)) "not declared" else variant_label(answer)
    if (kind == "test" && kind_labels[["test"]] == "SS type") {
      if (isTRUE(s$meta$balanced)) {
        text <- "not needed (balanced design: all types agree)"
      } else if (isTRUE(attr(answers, "ss_defaulted"))) {
        text <- paste0(text, " (reprokit default, not stated by the author)")
      }
    }
    if (kind == "convention") {
      text <- paste0(text, " (declaring it is a reprokit rule; APA does not require it)")
    }
    if (!is.null(fn) && kind %in% c("tail", "level") ||
          !is.null(fn) && kind == "test" && !has_ss_types(h)) {
      text <- paste0(text, " (read from the ", fn, " result)")
    }
    appendix_row(c(kind_labels[[kind]], text))
  }, character(1), USE.NAMES = FALSE)
  extras <- vapply(h$choices, appendix_choice_row, character(1), s = s, labels = labels,
                   USE.NAMES = FALSE)
  none <- if (length(rows) + length(extras) == 0L) appendix_row(c("(none to declare)", ""))
  c("### C. Analysis choices", "",
    appendix_row(c("Choice", "Answer")), appendix_row(c("---", "---")),
    rows, extras, none, "")
}

# One declared choice as a table row. A choice written "name: answer" shows its
# answer, with the real labels when it is about a direction and there are two. A
# choice with no answer written in (the test value) gets its value from the
# analysis.
appendix_choice_row <- function(choice, s, labels) {
  parts <- strsplit(choice, ": ", fixed = TRUE)[[1L]]
  name <- parts[1L]
  answer <- parts[2L]
  if (grepl("direction of the difference", name, fixed = TRUE) && length(labels) == 2L) {
    answer <- paste(labels[1L], "minus", labels[2L])
  }
  if (is.na(answer) && grepl("test value", name, fixed = TRUE) && !is.null(s$meta$mu)) {
    fn <- attr(s$choices_read, "fn")
    from <- if (!is.null(fn)) paste0(" (read from the ", fn, " result)")
    return(appendix_row(c("Test value mu", paste0(s$meta$mu, from))))
  }
  if (is.na(answer) && grepl("coding", name, fixed = TRUE)) {
    # The contrasts the fit used, as stored; none when there is no factor predictor.
    answer <- if (length(s$meta$contrasts) > 0L) {
      paste0(names(s$meta$contrasts), ": ", unlist(s$meta$contrasts), collapse = "; ")
    } else {
      "no factor predictors"
    }
  }
  if (is.na(answer)) answer <- "as analysed"
  appendix_row(c(appendix_capitalise(name), answer))
}

appendix_block_d <- function(h) {
  unreached <- unreached_quantities(h)
  text <- if (length(unreached) == 0L) {
    "None: every number in block A follows from blocks A and B."
  } else {
    with_reasons(h, unreached, h$not_checkable)
  }
  c("### D. APA numbers a reader cannot check from these statistics", "", text, "")
}

# Explicit choices against the ones the result records: a conflict is refused.
appendix_choices <- function(s, choices) {
  if (!is.list(choices) ||
        (length(choices) > 0L && (is.null(names(choices)) || !all(nzchar(names(choices)))))) {
    abort_input("`choices` must be a named list.")
  }
  h <- get_handler(s$handler)
  valid <- unique(conventions(h$name)$choice)
  if (has_ss_types(h)) valid <- c(valid, "ss_type")
  if (!is.null(h$design$terms) || !is.null(s$meta$conf_level)) valid <- c(valid, "level")
  # A name reprokit knows but this test does not take gets its own, more specific
  # refusal below; only names reprokit has never heard of are refused here.
  unknown <- setdiff(names(choices), c("convention", "ss_type", "level"))
  if (length(unknown) > 0L) {
    abort_input(
      "Unknown choice ", paste0("`", unknown, "`", collapse = ", "), " for the ",
      analysis_name(h), ". ",
      if (length(valid) > 0L) {
        paste0("Valid choices: ", paste0("`", valid, "`", collapse = ", "), ".")
      } else {
        "It takes no choices."
      }
    )
  }
  fn <- attr(s$choices_read, "fn")
  level <- choices[["level"]]
  if (!is.null(level) && !is.null(s$meta$conf_level)) {
    given <- if (is.numeric(level)) level else suppressWarnings(as.numeric(level))
    if (is.na(given) || abs(given - s$meta$conf_level) > 1e-8) {
      abort_input(
        "`choices$level = ", format(level), "` conflicts with the analysis, which was run with ",
        "confidence level ", s$meta$conf_level,
        if (!is.null(fn)) paste0(" (read from the ", fn, " result)"), "."
      )
    }
  }
  choices
}

appendix_sharing <- function(s, reason) {
  threshold <- s$disclosure$threshold
  checked <- s$disclosure$checked
  c("### Raw data", "",
    if (is.na(reason)) "Reason for not sharing the raw data: not stated by the author." else
      paste0("Reason for not sharing the raw data: ", reason),
    "",
    "**What readers lose compared with the raw data:**", "",
    paste0("- ", s$lost), "",
    paste0("**Disclosure check.** No summary rests on fewer than ", threshold,
           " cases, and n is at least ", minimum_n(threshold), ". ",
           if (length(checked) > 0L) paste0("Checked: ", checked, ". "),
           "This lowers the risk of identifying a participant; it does not remove it."))
}

appendix_row <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")

appendix_capitalise <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))

# Fixed decimals; without the leading zero for numbers that cannot exceed 1
# (APA style), with a true minus sign.
appendix_number <- function(x, digits, leading_zero = TRUE) {
  out <- formatC(round(x, digits), format = "f", digits = digits)
  if (!leading_zero) out <- sub("^(-?)0\\.", "\\1.", out)
  # paste0, not sub(): sub() turns the replacement into "<U+2212>" in a C locale.
  negative <- startsWith(out, "-")
  out[negative] <- paste0("\u2212", substring(out[negative], 2L))
  out
}

# Degrees of freedom: whole numbers print bare, Welch's fractional df with `digits`.
appendix_df <- function(df, digits) {
  appendix_number(df, if (abs(df - round(df)) < 1e-8) 0L else digits)
}

appendix_p <- function(p) {
  if (p < 0.001) "< .001" else appendix_number(p, 3L, leading_zero = FALSE)
}
