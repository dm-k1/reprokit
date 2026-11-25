# Cross-check of check_reported() against statcheck (Nuijten et al.), the established tool for
# the same job on t(df) = ..., p = ... strings.
# Claim: on correct papers check_reported() flags nothing, and every APA result that statcheck
# calls an error but check_reported() calls consistent falls in a listed, explained category
# (the categories are tested for below; a case in none of them is "unexplained").
# Papers: 360 papers per group from the generators of check-designs.R and the planted-error types
# of check-planted-errors.R (seed 20261124 as there), NOT the same papers as that script: it draws
# 1000 per group, and the type rotation now also holds the transposed-digit and one-tailed-p
# errors, so the two draws differ. Correct and with one planted error, restricted to the numbers an APA string carries:
#   t(df) = t, p = p   for the paired, student, welch, one_sample and pearson tests
#   r(df) = r, p = p   for cor_pearson_r, passed as check_reported("pearson", r =, df =, p =):
#                      n is not reported, so check_reported implies it from df (n = df + 2) and
#                      the script counts any string left at level 0 (not checkable)
# Planted papers are those whose planted number is in the string (t, df, p; for cor_pearson_r
# r, df and p).
# statcheck is called as statcheck::statcheck(text, messages = FALSE), one string per element,
# and mapped back by its `source` index; a string it does not parse is "unparsed" and dropped.
# statcheck's verdict is its `error` column (the reported p does not match the p computed from
# the test statistic and df, after its own rounding correction); check_reported's verdict is
# "inconsistent" (`$consistent` FALSE). Both see the same string.
# Disagreement categories (each case is put in the first that applies; the tests are below):
#   below-.001    reported p is "< .001" and a two-sided p over the rounding intervals fits it:
#                 statcheck compares the computed p of the printed statistic, check_reported
#                 the interval (0, .001) over the statistic's rounding interval
#   t-rounding    two-sided p fits only through the rounding interval of the printed t (matters
#                 at |t| near 0, where the p of t = 0.00 is 1 but of t = 0.005 is not):
#                 statcheck takes the printed t as exact, check_reported as a +-.005 interval
#   r-rounding    the same for the printed r in r(df) strings
#   one-tailed    statcheck flags it; some one-tailed p fits (statcheck assumes two-sided unless
#                 the text says one-sided; check_reported tries two-sided, less and greater).
#                 It holds the planted "one_tailed" errors (a one-tailed p printed as two-tailed,
#                 expected) and any other planted paper that happens to fit a tail; the counts
#                 by planted type are printed so a real miss cannot hide in it, and the script
#                 fails when a type that moves the statistic or the df by a large amount
#                 (decision, df_wrong_n, large t_digit or r_digit) is more than 10% of it
#   welch-df      statcheck flags it; a fractional df (Welch) is in the string and the fit needs
#                 df inside its rounding interval, which statcheck takes as exact
#   check-only    check_reported flags it and statcheck does not (an error that statcheck's
#                 tolerance accepts, or a number statcheck does not read: a check_reported-
#                 only win, explained by the same rounding tolerances)
#   unexplained   none of the above
# The "t-rounding" and "one-tailed" categories are printed with their counts per group, truth
# (correct / planted) and planted type.
# PASS criterion: (1) check_reported() flags 0 correct papers; (2) every case statcheck flags
# as error but check_reported() calls consistent is in a category above, none unexplained;
# (3) every case check_reported() flags and statcheck does not is detectable on a grid oracle
# (t or r over its rounding interval x df, every tail; base R, no check_reported code), so
# check_reported is not accusing a paper statcheck accepts for no reason. A check_reported miss
# of something statcheck catches that no tolerance explains is "unexplained" and fails.
# Seed 20261124. Size: 360 correct + about 150 to 300 planted per group (one_tailed and transposed
# included), 5 groups plus the r(df) form of Pearson.
# Skipped, with a message, when statcheck is not installed (it is in Suggests, used only here).
# Run (from the package root):
#   Rscript -e 'devtools::load_all(quiet=TRUE); source("inst/validation/check-statcheck.R")'

source("inst/validation/record.R")
record_start("check-statcheck", seed = 20261124)

if (!requireNamespace("statcheck", quietly = TRUE)) {
  cat("SKIPPED: the 'statcheck' package is not installed",
      "(install.packages(\"statcheck\")); nothing was checked.\n")
} else {
  options(width = 200)
  n_papers <- 360L
  set.seed(20261124)
  t_start <- Sys.time()
  source("inst/validation/check-designs.R")
  cases <- lapply(planted_groups, draw_cases, n_papers = n_papers)

  strip0 <- function(s) sub("^(-?)0\\.", "\\1.", s)
  apa_p <- function(p) if (grepl("<", p, fixed = TRUE)) "p < .001" else paste0("p = ", strip0(p))
  apa_t <- function(paper) paste0("t(", paper$df, ") = ", paper$t, ", ", apa_p(paper$p))
  apa_r <- function(paper) paste0("r(", paper$df, ") = ", strip0(paper$r), ", ", apa_p(paper$p))

  # One row per APA string: group, truth (correct / planted), type, text, handler, keys, paper.
  rows <- list()
  add <- function(group, handler, test, truth, type, paper, form) {
    # statcheck cannot compute a p for |r| = 1 (it stops with an error): such a string is left out
    if (form == "r" && abs(as.numeric(paper$r)) >= 1) return(invisible())
    row <- list(group = group, handler = handler, test = test, truth = truth, type = type, paper = paper,
                form = form, text = if (form == "t") apa_t(paper) else apa_r(paper),
                keys = if (form == "t") c("t", "df", "p") else c("r", "df", "p"))
    rows[[length(rows) + 1L]] <<- row # nolint: assignment_linter
  }
  # statcheck reads t(df) and r(df) strings only: the groups of other handlers (the one-way
  # ANOVA draw, added to planted_groups after this script was written) have no such string.
  # They are drawn but left out, so the t and r draws stay the same as check-planted-errors.R.
  for (g in setdiff(names(cases), "anova_oneway")) {
    handler <- planted_groups[[g]]$handler
    test <- planted_groups[[g]]$test
    for (case in cases[[g]]) {
      add(g, handler, test, "correct", "none", case$paper, "t")
      if (case$planted$quantity %in% c("t", "df", "p")) {
        add(g, handler, test, "planted", case$type, case$planted$paper, "t")
      }
      if (g == "cor_pearson") {
        add("cor_pearson_r", handler, test, "correct", "none", case$paper, "r")
        if (case$planted$quantity %in% c("r", "df", "p")) {
          add("cor_pearson_r", handler, test, "planted", case$type, case$planted$paper, "r")
        }
      }
    }
  }

  texts <- vapply(rows, `[[`, "", "text")
  sc <- statcheck::statcheck(texts, messages = FALSE)
  sc_error <- rep(NA, length(rows))
  sc_error[as.integer(sc$source)] <- sc$error
  sc_computed <- rep(NA_real_, length(rows))
  sc_computed[as.integer(sc$source)] <- sc$computed_p

  check_one <- function(row) {
    res <- run_check(row$test, row$paper, row$keys)
    if (inherits(res, "error")) "error" else if (res$consistent) "consistent" else "inconsistent"
  }
  cr <- unlist(parallel::mclapply(rows, check_one, mc.cores = 6L))

  # Category tests, independent of check_reported() ---------------------------------------
  p_interval <- function(s) {
    if (grepl("<", s, fixed = TRUE)) return(c(0, 0.001))
    v <- as.numeric(s)
    c(max(v - 0.5 * 10^-n_decimals(s), 0), min(v + 0.5 * 10^-n_decimals(s), 1))
  }
  overlaps <- function(x, target) x[1L] <= target[2L] + 1e-9 && x[2L] >= target[1L] - 1e-9
  # p implied by t (or r) and df over the rounding intervals, per tail.
  implied_p <- function(row, exact_df, tails = c("two", "less", "greater")) {
    paper <- row$paper
    r_form <- row$form == "r"
    stat <- as.numeric(if (r_form) paper$r else paper$t)
    h <- 0.5 * 10^-n_decimals(if (r_form) paper$r else paper$t)
    sg <- seq(stat - h, stat + h, length.out = 101L)
    if (r_form) sg <- pmin(pmax(sg, -1), 1)
    dfv <- as.numeric(paper$df)
    dh <- if (exact_df) 0 else 0.5 * 10^-n_decimals(paper$df)
    dg <- if (dh == 0) dfv else seq(dfv - dh, dfv + dh, length.out = 11L)
    g <- expand.grid(s = sg, df = dg)
    tv <- if (r_form) g$s * sqrt(g$df) / sqrt(1 - g$s^2) else g$s
    lapply(setNames(tails, tails), function(tail) {
      p <- switch(tail, two = 2 * pt(-abs(tv), g$df), less = pt(tv, g$df),
                  greater = pt(tv, g$df, lower.tail = FALSE))
      range(p[is.finite(p)])
    })
  }
  fits <- function(row, exact_df, tails) {
    any(vapply(implied_p(row, exact_df, tails), overlaps, NA,
               target = p_interval(row$paper$p)))
  }
  classify <- function(i) {
    row <- rows[[i]]
    if (is.na(sc_error[i])) return("unparsed")
    s <- sc_error[i]
    c_flag <- cr[i] == "inconsistent"
    if (s && !c_flag) {
      if (fits(row, TRUE, "two")) {
        if (grepl("<", row$paper$p, fixed = TRUE)) return("below-.001")
        return(if (row$form == "r") "r-rounding" else "t-rounding")
      }
      if (fits(row, TRUE, c("less", "greater"))) return("one-tailed")
      if (n_decimals(row$paper$df) > 0L && fits(row, FALSE, c("two", "less", "greater"))) {
        return("welch-df")
      }
      return("unexplained")
    }
    if (!s && c_flag) return("check-only")
    "agree"
  }
  category <- vapply(seq_along(rows), classify, "")
  d <- data.frame(group = vapply(rows, `[[`, "", "group"), truth = vapply(rows, `[[`, "", "truth"),
                  type = vapply(rows, `[[`, "", "type"), text = texts,
                  statcheck = ifelse(is.na(sc_error), "unparsed", ifelse(sc_error, "error", "ok")),
                  check = cr, category = category, stringsAsFactors = FALSE)

  cat("statcheck", as.character(packageVersion("statcheck")), "- strings:", nrow(d),
      " unparsed by statcheck:", sum(d$statcheck == "unparsed"), "\n\n")
  cat("2 x 2 per group: rows = statcheck, columns = check_reported (planted and correct strings)\n")
  for (g in unique(d$group)) {
    cat("\n", g, "\n", sep = "")
    print(table(statcheck = d$statcheck[d$group == g & d$statcheck != "unparsed"],
                check_reported = d$check[d$group == g & d$statcheck != "unparsed"]))
  }
  raw_r <- Filter(function(row) row$form == "r", rows)
  raw_level <- unlist(parallel::mclapply(raw_r, function(row) {
    res <- run_check(row$test, row$paper, row$keys)
    if (inherits(res, "error")) NA_integer_ else res$level
  }, mc.cores = 6L))
  cat("\nr(df), p strings passed as r, df, p (n implied from df): level 0 (not checkable) for",
      sum(raw_level == 0L, na.rm = TRUE), "of", length(raw_r), "\n")
  cat("\nCorrect papers only: check_reported inconsistent / statcheck error, of n\n")
  corr <- d[d$truth == "correct", ]
  print(do.call(rbind, lapply(split(corr, corr$group), function(x) {
    data.frame(group = x$group[1L], n = nrow(x), check_reported = sum(x$check == "inconsistent"),
               statcheck = sum(x$statcheck == "error"))
  })), row.names = FALSE)

  cat("\nDisagreements by category (statcheck error & check_reported consistent, or the reverse)\n")
  dis <- d[!d$category %in% c("agree", "unparsed"), ]
  print(as.data.frame(table(group = dis$group, category = dis$category)) |>
          (\(x) x[x$Freq > 0, ])(), row.names = FALSE)
  cat("\nThe t-rounding and one-tailed categories by group, truth and planted type (a real miss",
      "hiding in a category would show as an unexpected type)\n")
  swallow <- dis[dis$category %in% c("t-rounding", "r-rounding", "one-tailed"), ]
  print(as.data.frame(table(category = swallow$category, group = swallow$group, truth = swallow$truth,
                            type = swallow$type)) |> (\(x) x[x$Freq > 0, ])(), row.names = FALSE)
  one_t <- swallow[swallow$category == "one-tailed", ]
  gross <- one_t$type %in% c("decision", "df_wrong_n")
  swallowed <- sum(gross)
  cat("planted one_tailed strings: statcheck error", sum(d$type == "one_tailed" & d$statcheck == "error"),
      "of", sum(d$type == "one_tailed" & d$statcheck != "unparsed"), "; check_reported inconsistent",
      sum(d$type == "one_tailed" & d$check == "inconsistent"), "\n")
  cat("one-tailed category: ", nrow(one_t), " cases, of which decision / df_wrong_n errors: ", swallowed,
      " (limit 10%: ", floor(0.1 * nrow(one_t)), ")\n", sep = "")
  cat("\nOne example per category and group (text; truth/type; verdicts)\n")
  seen <- character(0)
  for (i in seq_len(nrow(dis))) {
    key <- paste(dis$group[i], dis$category[i])
    if (key %in% seen) next
    seen <- c(seen, key)
    cat(sprintf("  [%s | %s] %s  (%s/%s) statcheck=%s check_reported=%s\n", dis$category[i],
                dis$group[i], dis$text[i], dis$truth[i], dis$type[i], dis$statcheck[i],
                dis$check[i]))
  }

  # check-only cases must be oracle-detectable (the same grid as check-planted-errors.R) -----
  tdfp_detectable <- function(row) {
    !fits(row, !(n_decimals(row$paper$df) > 0L), c("two", "less", "greater"))
  }
  check_only <- which(d$category == "check-only")
  unverified <- sum(!vapply(check_only, function(i) tdfp_detectable(rows[[i]]), NA))
  cat("\ncheck_reported flags, statcheck does not:", length(check_only),
      " of which not oracle-detectable:", unverified, "\n")

  errors <- sum(d$check == "error")
  false_positives <- sum(d$truth == "correct" & d$check == "inconsistent")
  unexplained <- sum(d$category == "unexplained")
  cat("runtime (min):", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "\n")
  pass <- errors == 0 && false_positives == 0 && unexplained == 0 && unverified == 0 &&
    swallowed <= floor(0.1 * nrow(one_t))
  cat(if (pass) "PASS" else "FAIL",
      ": check_reported false positives = ", false_positives, ", errors = ", errors,
      ", unexplained statcheck-only errors = ", unexplained,
      ", check-only flags not oracle-detectable = ", unverified, ", gross errors inside the one-tailed",
      " category = ", swallowed, "\n", sep = "")
}

# SKIPPED (statcheck missing) counts as FAIL for a release run: nothing was checked.
finish_validation(requireNamespace("statcheck", quietly = TRUE) && exists("pass") && isTRUE(pass))
