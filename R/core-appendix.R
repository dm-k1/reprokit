# The appendix of a sufficient-statistics object (ADR 0010, ADR 0013 point 6).
# In one line: appendix = what APA asks for (A) + what APA leaves out but the
# result needs (B) + the choices behind the numbers (C) + what stays uncheckable
# (D), then why the raw data are not shared and what readers lose.
# Every block comes from the registry, so the appendix matches what the package
# does; only the author's answers (sharing reason, effect-size convention) are
# typed in.

#' Write the reporting appendix for a sufficient-statistics object
#'
#' The APA Publication Manual (7th ed., §3.7) says that when the sufficient set
#' of statistics is extensive, "the information could be supplied in a
#' supplementary data set or an appendix". `appendix()` writes that appendix as
#' markdown, in four blocks generated from the handler registry:
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
#' @param s A `reprokit_sufficient_stats` object from [sufficient_stats()].
#' @param choices Named list of answers the object cannot know. `convention`:
#'   the effect-size convention, as a variant code (e.g. `"d_z"`, `"d_av"`).
#'   Declaring it is a reprokit rule, not an APA requirement. `ss_type`: for a
#'   factorial ANOVA, the type of sums of squares (`"III"`, `"II"` or `"I"`); only
#'   an unbalanced design needs it.
#' @param digits Decimals for the APA numbers in block A (p always gets 3).
#' @param digits_added Decimals for the added numbers in block B. They feed
#'   back into the result, so they get more decimals than APA's two; the object
#'   itself keeps full precision.
#' @return A character vector of markdown lines, class `reprokit_appendix`.
#' @export
#' @examples
#' sleep_wide <- data.frame(drug1 = sleep$extra[sleep$group == 1],
#'                          drug2 = sleep$extra[sleep$group == 2])
#' s <- sufficient_stats(Pair(drug1, drug2) ~ 1, data = sleep_wide, test = "paired",
#'                       sharing_reason = "Consent did not cover public sharing.")
#' appendix(s, choices = list(convention = "d_z"))
appendix <- function(s, choices = list(), digits = 2L, digits_added = 4L) {
  if (!inherits(s, "reprokit_sufficient_stats")) {
    stop("`s` must be a reprokit_sufficient_stats object from sufficient_stats().",
         call. = FALSE)
  }
  h <- get_handler(s$handler)
  if (is.null(h$design)) {
    stop("No appendix for `", s$handler, "` yet: its APA set is not declared.", call. = FALSE)
  }
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
    appendix_sharing(s)
  )
  structure(lines, class = "reprokit_appendix")
}

#' @export
print.reprokit_appendix <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
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
  convention_codes <- unique(unlist(lapply(h$relations, function(rel) rel$variants$convention)))
  if (!is.null(choices$convention) && length(convention_codes) == 0L) {
    stop("`", h$name, "` has no effect-size convention to declare; drop `choices$convention`.",
         call. = FALSE)
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
    stop("`choices$convention` must be one of: ",
         paste0("\"", convention_codes, "\"", collapse = ", "), ".", call. = FALSE)
  }
  test <- if (is.null(s$meta$test_variant)) s$meta$test else s$meta$test_variant
  ss_codes <- unique(unlist(lapply(h$relations, function(rel) rel$variants$test)))
  if (has_ss_types(h)) {
    # The SS type is the author's to declare (never inferred); a stored one counts.
    test <- if (!is.null(choices$ss_type)) choices$ss_type else
      if (!is.null(s$meta$ss_type)) s$meta$ss_type else NA_character_
    if (!is.na(test) && !test %in% ss_codes) {
      stop("`choices$ss_type` must be one of: ",
           paste0("\"", ss_codes, "\"", collapse = ", "), ".", call. = FALSE)
    }
  } else if (!is.null(choices$ss_type)) {
    stop("`", h$name, "` has no SS type to declare; drop `choices$ss_type`.", call. = FALSE)
  }
  answers <- list(
    tail = s$meta$alternative,
    level = if (is.null(s$meta$conf_level)) NULL else sprintf("%.2f", s$meta$conf_level),
    test = test,
    convention = convention
  )
  answers[intersect(names(answers), kinds)]
}

# Whether the handler's `test` variants are SS types (a factorial ANOVA).
has_ss_types <- function(h) {
  codes <- unique(unlist(lapply(h$relations, function(rel) rel$variants$test)))
  length(codes) > 0L && all(codes %in% ss_type_codes())
}

# Every quantity's value: the stored descriptives, then what reproduce() returns
# (the round-trip-tested path), then any APA quantity still missing (the effect
# size) from a relation evaluated at the author's answers.
appendix_values <- function(h, s, answers) {
  stats <- s$stats
  names(stats)[names(stats) == "mean"] <- "m"
  vals <- stats[intersect(names(stats), quantity_names())]
  reproduced <- as.list(h$reproduce(s))
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
    es_label <- variant_label(answers$convention)
    c(if (es == "v") es_label else paste0(es, ", ", es_label),
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
  reasons <- missing_reasons()
  rows <- vapply(missing, function(q) {
    name <- if (q == "r" && length(labels) == 2L) {
      paste0("r between ", labels[1L], " and ", labels[2L])
    } else {
      display_quantities(h, q)
    }
    appendix_row(c(name, paste(appendix_number(vals[[q]], digits, leading_zero = q != "r"),
                               collapse = ", "),
                   paste0("APA does not require this; ", reasons[[q]])))
  }, character(1), USE.NAMES = FALSE)
  c(out, appendix_row(c("Number", "Value", "Why it is needed")),
    appendix_row(c("---", "---", "---")), rows, "")
}

appendix_block_c <- function(h, s, answers, labels) {
  kind_labels <- c(tail = "Tails", level = "Confidence level", test = "Test",
                   convention = "Effect-size convention")
  if (has_ss_types(h)) kind_labels[["test"]] <- "SS type"
  rows <- vapply(names(answers), function(kind) {
    answer <- answers[[kind]]
    text <- if (is.na(answer)) "not declared" else variant_label(answer)
    if (kind == "test" && kind_labels[["test"]] == "SS type" && isTRUE(s$meta$balanced)) {
      text <- "not needed (balanced design: all types agree)"
    }
    if (kind == "convention") {
      text <- paste0(text, " (declaring it is a reprokit rule; APA does not require it)")
    }
    appendix_row(c(kind_labels[[kind]], text))
  }, character(1), USE.NAMES = FALSE)
  extras <- vapply(h$choices, appendix_choice_row, character(1), s = s, labels = labels,
                   USE.NAMES = FALSE)
  c("### C. Analysis choices", "",
    appendix_row(c("Choice", "Answer")), appendix_row(c("---", "---")),
    rows, extras, "")
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
    return(appendix_row(c("Test value mu", s$meta$mu)))
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

appendix_sharing <- function(s) {
  threshold <- s$disclosure$threshold
  checked <- s$disclosure$checked
  c("### Raw data", "",
    paste0("**Why the raw data are not shared.** ", s$sharing_reason), "",
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
