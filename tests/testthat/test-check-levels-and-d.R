# Confidence-level variants of every CI relation, the paired d conventions, and the
# readable variant labels (ADR 0014).

registered_checkers <- function() {
  Filter(function(n) !is.null(get_handler(n)$design), handlers())
}

ci_paper <- function(level, n = 30, m = c(5, 4.2), s = c(1, 1.1)) {
  se <- sqrt(sum((n - 1) * s^2) / (2 * n - 2) * 2 / n)
  ci <- diff(-m) * 0 + (m[1] - m[2]) + c(-1, 1) * stats::qt(1 - (1 - level) / 2, 2 * n - 2) * se
  two <- function(x) formatC(x, format = "f", digits = 2)
  list(n = c(n, n), m = two(m), sd = two(s), ci_lower = two(ci[1]), ci_upper = two(ci[2]))
}

fitting_levels <- function(checked) {
  levels <- checked$variants$level
  levels$variant[levels$fits]
}

test_that("every CI relation offers 90, 95 and 99 percent as level variants", {
  for (name in registered_checkers()) {
    for (rel in get_handler(name)$relations) {
      if (grepl("^ci_", rel$output)) {
        expect_identical(rel$variants$level, c("0.90", "0.95", "0.99"), info = rel$name)
        expect_true("level" %in% names(rel$variants), info = rel$name)
      }
    }
  }
})

test_that("a correctly rounded independent-samples CI fits its own level only", {
  for (level in c(0.90, 0.95, 0.99)) {
    paper <- ci_paper(level)
    paper$n <- as.character(paper$n)
    checked <- do.call(check_independent, c(paper))
    expect_true(checked$consistent, info = level)
    expect_true(sprintf("%.2f", level) %in% fitting_levels(checked), info = level)
  }
  paper <- ci_paper(0.90)
  paper$ci_upper <- fixed(as.numeric(paper$ci_upper) + 0.2, 2)
  checked <- do.call(check_independent,
                     c(lapply(paper, as.character)))
  expect_false(checked$consistent)
})

test_that("one-sample, paired and Pearson CIs accept 90, 95 and 99 percent", {
  x <- datasets::sleep$extra[datasets::sleep$group == 1]
  for (level in c(0.90, 0.95, 0.99)) {
    r <- stats::t.test(x, conf.level = level)
    checked <- check_reported(test = "one_sample", n = "10", m = fixed(mean(x), 2),
                              sd = fixed(stats::sd(x), 2),
                              ci_lower = fixed(r$conf.int[1], 2),
                              ci_upper = fixed(r$conf.int[2], 2))
    expect_true(checked$consistent, info = level)
    expect_true(sprintf("%.2f", level) %in% fitting_levels(checked))
    cr <- stats::cor.test(datasets::mtcars$mpg, datasets::mtcars$wt, conf.level = level)
    checked <- check_reported(test = "pearson", n = "32", r = fixed(cr$estimate, 3),
                              ci_lower = fixed(cr$conf.int[1], 3),
                              ci_upper = fixed(cr$conf.int[2], 3))
    expect_true(checked$consistent, info = level)
    expect_true(sprintf("%.2f", level) %in% fitting_levels(checked))
    paper <- paper_paired()
    first <- datasets::sleep$extra[datasets::sleep$group == 1]
    second <- datasets::sleep$extra[datasets::sleep$group == 2]
    pr <- stats::t.test(first, second, paired = TRUE, conf.level = level)
    paper$ci_lower <- fixed(pr$conf.int[1], 2)
    paper$ci_upper <- fixed(pr$conf.int[2], 2)
    checked <- run_check("paired", paper)
    expect_true(checked$consistent, info = level)
    expect_true(sprintf("%.2f", level) %in% fitting_levels(checked))
  }
})

test_that("paired d: d_z, d_rm and d_av, signed first minus second, never inconsistent", {
  paper <- paper_paired()
  first <- datasets::sleep$extra[datasets::sleep$group == 1]
  second <- datasets::sleep$extra[datasets::sleep$group == 2]
  sd_diff <- stats::sd(first - second)
  r <- stats::cor(first, second)
  d_z <- (mean(first) - mean(second)) / sd_diff
  conventions <- c(d_z = d_z, d_rm = d_z * sqrt(2 * (1 - r)),
                   d_av = (mean(first) - mean(second)) /
                     mean(c(stats::sd(first), stats::sd(second))))
  for (convention in names(conventions)) {
    paper$d <- fixed(conventions[[convention]], 2)
    checked <- run_check("paired", paper)
    expect_true(checked$consistent, info = convention)
    expect_equal(status_of(checked, "d"), "verified", info = convention)
    fitting <- checked$variants$convention
    expect_true(convention %in% fitting$variant[fitting$fits], info = convention)
  }
  # A d that no convention reproduces, and the wrong sign, are not reproducible.
  paper$d <- "0.90"
  expect_equal(status_of(run_check("paired", paper), "d"), "not reproducible")
  expect_true(run_check("paired", paper)$consistent)
  paper$d <- fixed(-conventions[["d_z"]], 2)
  expect_true(run_check("paired", paper)$consistent)
})

test_that("paired d is checkable without r through d_av, and r stays missing", {
  first <- datasets::sleep$extra[datasets::sleep$group == 1]
  second <- datasets::sleep$extra[datasets::sleep$group == 2]
  d_av <- (mean(first) - mean(second)) / mean(c(stats::sd(first), stats::sd(second)))
  checked <- check_reported(test = "paired", n = "10", m = fixed(c(mean(first), mean(second)), 2),
                            sd = fixed(c(stats::sd(first), stats::sd(second)), 2),
                            d = fixed(d_av, 2))
  expect_equal(status_of(checked, "d"), "verified")
  expect_true(checked$consistent)
  expect_equal(checked$missing$quantity, "r")
  # d alone says nothing about r: no implied range from a relation that cannot falsify.
  expect_null(checked$implied$r)
})

test_that("a shared output is available over the widest of its relations", {
  intervals <- list(n = list(lower = 1, upper = 2))
  rels <- list(relation("first", "n", "df", function(vals, variant) vals$n),
               relation("second", "n", "df", function(vals, variant) vals$n * 10),
               relation("later", "df", "t", function(vals, variant) vals$df))
  evaluations <- evaluate_relations(rels, intervals, list())
  later <- evaluations[evaluations$relation == "later", ]
  expect_equal(c(later$derived_lower, later$derived_upper), c(1, 20))
})

test_that("every variant of every registered relation has a readable label", {
  codes <- unique(unlist(lapply(registered_checkers(), function(name) {
    unlist(lapply(get_handler(name)$relations, `[[`, "variants"))
  })))
  expect_gt(length(codes), 10L)
  expect_no_error(variant_label(codes))
  expect_equal(variant_label("welch"), "Welch's t")
  expect_error(variant_label("no_such_variant"), "No label for variant: 'no_such_variant'")
})

test_that("print and choices use the labels, with none as the fallback", {
  paper <- paper_paired()
  out <- paste(utils::capture.output(print(run_check("paired", paper))), collapse = "\n")
  expect_match(out, "Confidence levels that fit: 95%", fixed = TRUE)
  expect_false(grepl("two.sided", out, fixed = TRUE))
  bad <- check_reported(test = "paired", n = "10", m = c("1.00", "1.10"), sd = c("1.00", "1.00"),
                        d = "0.90")
  expect_match(paste(utils::capture.output(print(bad)), collapse = "\n"),
               "conventions that fit: none", fixed = TRUE)
})

# Crossed variants: one choice per kind across all the relations that can falsify.

welch_paper <- function(level, digits = 2) {
  fixed <- function(x, digits) formatC(unname(x), format = "f", digits = digits)
  x <- datasets::sleep$extra[datasets::sleep$group == 1]
  y <- datasets::sleep$extra[datasets::sleep$group == 2]
  r <- stats::t.test(x, y, conf.level = level)
  list(n = as.character(c(length(x), length(y))),
       m = fixed(c(mean(x), mean(y)), digits), sd = fixed(c(stats::sd(x), stats::sd(y)), digits),
       ci_lower = fixed(r$conf.int[1], digits), ci_upper = fixed(r$conf.int[2], digits))
}

fitting_of <- function(checked, kind) {
  v <- checked$variants[[kind]]
  v$variant[v$fits]
}

test_that("a crossed relation is evaluated over every combination, with a column per kind", {
  rel <- relation("r", "n", "df", function(vals, variant) {
    vals$n + (variant[["test"]] == "welch") * 10 + as.numeric(variant[["level"]])
  }, variants = list(level = c("0.90", "0.95"), test = c("student", "welch")))
  expect_identical(names(rel$variants), intersect(relation_variant_kinds(), c("level", "test")))
  expect_length(variant_combinations(rel$variants), 4L)
  ev <- evaluate_relations(list(rel), list(n = list(lower = 1, upper = 1)), NULL)
  expect_equal(nrow(ev), 4L)
  expect_true(all(c("tail", "level", "test", "convention") %in% names(ev)))
  expect_true(all(is.na(ev$tail)) && all(is.na(ev$convention)))
  expect_setequal(paste(ev$level, ev$test), c("0.90 student", "0.90 welch", "0.95 student",
                                              "0.95 welch"))
  expect_true(all(ev$kind == paste(names(rel$variants), collapse = "+")))
  expect_identical(ev$variant, paste(ev$level, ev$test, sep = "+"))
  # A plain-string variant for one kind, a named vector for several.
  seen <- NULL
  one <- relation("one", "n", "df", function(vals, variant) {
    seen <<- variant
    vals$n
  }, variants = c("a", "b"), variant_kind = "test")
  relation_range(one, list(n = list(lower = 1, upper = 1)), "a")
  expect_identical(seen, "a")
})

test_that("an empty evaluation has the per-kind columns", {
  ev <- evaluate_relations(list(), list(), NULL)
  expect_true(all(relation_variant_kinds() %in% names(ev)))
  expect_equal(nrow(ev), 0L)
})

test_that("a correct 90 and 99 percent Welch CI each pass and report their level and test", {
  for (level in c(0.90, 0.99)) {
    checked <- do.call(check_independent, c(welch_paper(level)))
    expect_true(checked$consistent, info = as.character(level))
    expect_identical(fitting_of(checked, "level"), sprintf("%.2f", level),
                     info = as.character(level))
    expect_true("welch" %in% fitting_of(checked, "test"), info = as.character(level))
  }
})

test_that("bounds at different levels are inconsistent, though each fits some level", {
  lo <- welch_paper(0.90)
  hi <- welch_paper(0.99)
  paper <- lo
  paper$ci_upper <- hi$ci_upper
  checked <- do.call(check_independent, c(paper))
  expect_false(checked$consistent)
  expect_true(all(c("ci_lower", "ci_upper") %in%
                    checked$numbers$quantity[checked$numbers$status == "inconsistent"]))
  expect_length(fitting_of(checked, "level"), 0L)
})

test_that("a Student-only t with a Welch-only CI is inconsistent", {
  # Unequal groups with very different SDs so that Student and Welch disagree.
  n <- c(8, 40)
  m <- c(10, 8)
  s <- c(4, 1)
  welch_se <- sqrt(sum(s^2 / n))
  welch_df <- sum(s^2 / n)^2 / sum((s^2 / n)^2 / (n - 1))
  pooled_se <- sqrt(sum((n - 1) * s^2) / (sum(n) - 2) * sum(1 / n))
  ci <- (m[1] - m[2]) + c(-1, 1) * stats::qt(0.975, welch_df) * welch_se
  common <- list(n = as.character(n), m = fixed(m, 2), sd = fixed(s, 2),
                 ci_lower = fixed(ci[1], 2), ci_upper = fixed(ci[2], 2))
  welch_t <- fixed((m[1] - m[2]) / welch_se, 2)
  student_t <- fixed((m[1] - m[2]) / pooled_se, 2)
  expect_false(welch_t == student_t)
  both_welch <- do.call(check_independent,
                        c(common, list(t = welch_t)))
  expect_true(both_welch$consistent)
  mixed <- do.call(check_independent,
                   c(common, list(t = student_t)))
  expect_false(mixed$consistent)
  expect_length(fitting_of(mixed, "test"), 0L)
})

test_that("a user value restricts only its own kind", {
  paper <- welch_paper(0.90)
  both <- do.call(check_independent, c(paper,
                                    list(variants = c("welch", "0.90"))))
  expect_true(both$consistent)
  ci <- both$relations[grepl("^ci_", both$relations$relation), ]
  expect_setequal(ci$test, "welch")
  expect_setequal(ci$level, "0.90")
  only_level <- do.call(check_independent, c(paper,
                                          list(variants = "0.90")))
  ci <- only_level$relations[grepl("^ci_", only_level$relations$relation), ]
  expect_setequal(ci$test, c("student", "welch"))
  expect_setequal(ci$level, "0.90")
  wrong <- do.call(check_independent, c(paper,
                                     list(variants = "0.99")))
  expect_false(wrong$consistent)
  expect_error(do.call(check_independent, c(paper,
                                         list(variants = "no_such"))), "Unknown variant")
})

test_that("relation() validates crossed variant lists", {
  f <- function(vals, variant) vals$n
  expect_error(relation("r", "n", "df", f, variants = list(nope = "a")))
  expect_error(relation("r", "n", "df", f, variants = list(test = character())))
  expect_silent(relation("r", "n", "df", f, variants = list(test = "a", level = "0.95")))
})

test_that("a bound that fits under no choice does not drag the holding numbers along", {
  # Correct 95% Student CI with only the upper bound shifted: ci_upper fails on
  # every choice, but the lower bound fits, and keeps fitting at 95%.
  paper <- ci_paper(0.95)
  paper$n <- as.character(paper$n)
  correct <- do.call(check_independent, c(paper))
  expect_true(correct$consistent)

  # Far enough that no level fits (+0.2 lands on the 99% bound).
  paper$ci_upper <- fixed(as.numeric(paper$ci_upper) + 1, 2)
  checked <- do.call(check_independent, c(paper))
  expect_false(checked$consistent)
  expect_equal(status_of(checked, "ci_upper"), "inconsistent")
  expect_false(status_of(checked, "ci_lower") == "inconsistent")
  expect_true("0.95" %in% fitting_levels(checked))
})
