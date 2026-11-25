# Chi-squared of independence and chi-squared goodness of fit (ADR 0016).
# In one line: the table of counts (and, for goodness of fit, the H0 proportions)
# determines chi-squared, df, p and the effect size exactly; a count is a whole
# number, so chi-squared is one value, not a range.
# The table shape and the H0 proportions are test arguments of the relations
# (`dims`, `p0`), because a flat vector of counts does not say whether 12 cells
# are 2 x 6 or 3 x 4.

register_chisq_independence <- function() {
  register_handler(
    name = "chisq_independence",
    family = "Simple tests",
    analysis = "Chi-squared test of independence",
    verdict = "summaries_suffice",
    needs = "the contingency table of counts",
    catch = "declare whether the continuity correction was used (2 x 2 tables)",
    lost = chisq_lost(),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && is.character(test) && length(test) == 1L && test == "chisq"
    },
    summarise = chisq_independence_summarise,
    reproduce = chisq_independence_reproduce,
    reported = chisq_independence_reported,
    disclosure = chisq_disclosure,
    formula_tests = "chisq",
    design = apa_design(statistic = "chisq", effect_size = "v", cells = NA_integer_, ci = FALSE,
                        descriptives = "n", extra = "n_total"),
    sufficient = "n",
    per_cell = "n",
    domains = list(n = c(0, Inf)),
    labels = c(n = "the contingency table"),
    relations = chisq_independence_relations,
    appendix_table = chisq_appendix_table,
    example = function() {
      d <- as.data.frame(margin.table(base_data("UCBAdmissions"), c(1, 2)))
      list(x = ~ Admit + Gender, args = list(data = d[rep(seq_len(nrow(d)), d$Freq), 1:2],
                                             test = "chisq"))
    }
  )
}

register_chisq_gof <- function() {
  register_handler(
    name = "chisq_gof",
    family = "Simple tests",
    analysis = "Chi-squared goodness of fit",
    verdict = "summaries_suffice",
    needs = "the observed counts and the H0 proportions",
    catch = "report the H0 proportions unless they are equal",
    lost = chisq_lost(),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && is.character(test) && length(test) == 1L && test == "gof"
    },
    summarise = chisq_gof_summarise,
    reproduce = chisq_gof_reproduce,
    reported = chisq_gof_reported,
    disclosure = chisq_disclosure,
    formula_tests = "gof",
    design = apa_design(statistic = "chisq", effect_size = NULL, cells = NA_integer_, ci = FALSE,
                        descriptives = "n", extra = "n_total"),
    sufficient = c("n", "p0"),
    per_cell = "n",
    # Cohen's w is not bounded by 1 (sqrt(k - 1) for a uniform H0 with every count in one
    # category), unlike Cramer's V.
    domains = list(n = c(0, Inf), v = c(0, Inf)),
    labels = c(n = "the observed counts", v = "w"),
    relations = chisq_gof_relations,
    appendix_table = chisq_appendix_table,
    example = function() {
      list(x = ~ cyl, args = list(data = data.frame(cyl = base_data("mtcars")$cyl), test = "gof"))
    }
  )
}

chisq_lost <- function() {
  c(paste("checking that the expected counts are large enough for the chi-squared",
          "approximation (expected counts below 5; reprokit does not test this)"),
    "exact, Fisher or Monte Carlo alternatives for sparse tables",
    "any variable other than the ones in the table")
}

# A cell (independence) or category (goodness of fit) that is small but not empty could
# identify a participant; an empty one says only that nobody falls there, and zeros are
# not flagged in v1 (ADR 0023 point 3).
chisq_disclosure <- function(summary, threshold) {
  n <- summary$stats$n
  small <- which(n > 0 & n < threshold)
  labels <- summary$meta$cell_labels
  unit <- if (length(summary$meta$dims) == 1L) "category" else "cell"
  list(
    passed = length(small) == 0L,
    checked = paste0(unit, " counts (a count above 0 and below ", threshold,
                     " is flagged; zeros are not)"),
    problems = if (length(small)) {
      paste0(unit, " ", labels[small], " has n = ", n[small], " < ", threshold)
    } else {
      character()
    }
  )
}

# Shape and H0 proportions -------------------------------------------------------

check_table_dims <- function(dims, kind) {
  ok <- is.numeric(dims) && all(dims == round(dims)) && all(dims >= 2)
  if (kind == "independence") {
    if (!ok || length(dims) != 2L) {
      stop("`dims` must be c(rows, columns), two whole numbers of at least 2.", call. = FALSE)
    }
  } else if (!ok || length(dims) != 1L) {
    stop("`dims` must be the number of categories, one whole number of at least 2.",
         call. = FALSE)
  }
  invisible(dims)
}

# p0: k proportions that sum to 1 (within 1e-8); an input error otherwise.
check_p0 <- function(p0, k = NULL) {
  if (!is.numeric(p0) || anyNA(p0) || any(p0 < 0)) {
    stop("`p0` must be non-negative numbers, one proportion per category.", call. = FALSE)
  }
  if (!is.null(k) && length(p0) != k) {
    stop("`p0` must have one proportion per category: length ", k, ", but got ", length(p0),
         ".", call. = FALSE)
  }
  if (abs(sum(p0) - 1) > 1e-8) {
    stop("`p0` must sum to 1 (within 1e-8), but sums to ", format(sum(p0)), ".", call. = FALSE)
  }
  invisible(p0)
}

table_matrix <- function(n, dims) {
  if (length(n) != prod(dims)) {
    stop("`n` has ", length(n), " counts, but a table of ", dims[1L], " x ", dims[2L],
         " has ", prod(dims), ".", call. = FALSE)
  }
  matrix(n, nrow = dims[1L], ncol = dims[2L])
}

# Pearson chi-squared of a table, or Yates' as chisq.test() computes it
# (|O - E| - min(0.5, |O - E|) over the whole table). NaN when an expected count is
# 0 (an empty row or column): not evaluable.
chisq_table_stat <- function(n, dims, yates = FALSE) {
  tab <- table_matrix(n, dims)
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  if (!all(is.finite(expected)) || any(expected == 0)) return(NaN)
  gap <- abs(tab - expected)
  if (yates) gap <- gap - min(0.5, gap)
  sum(gap^2 / expected)
}

chisq_gof_stat <- function(n, p0 = NULL, k = length(n)) {
  if (length(n) != k) {
    stop("`n` has ", length(n), " counts, but `dims` says ", k, " categories.", call. = FALSE)
  }
  p <- if (is.null(p0)) rep(1 / k, k) else p0
  expected <- sum(n) * p
  if (!all(is.finite(expected)) || any(expected == 0)) return(NaN)
  sum((n - expected)^2 / expected)
}

# Cramer's V (w for goodness of fit) from a Pearson chi-squared.
effect_from_chisq <- function(chisq, total, scale) sqrt(chisq / (total * scale))

independence_effect <- function(chisq, total, dims) {
  effect_from_chisq(chisq, total, min(dims) - 1)
}

# Relations ---------------------------------------------------------------------

# `dims = NULL` (the registry's default) has the 2 x 2 variants and no shape: the
# shape-dependent relations are then not evaluable. Larger tables have Pearson only.
chisq_independence_relations <- function(dims = NULL) {
  if (!is.null(dims)) check_table_dims(dims, "independence")
  with_variants <- is.null(dims) || all(dims == 2)
  tests <- if (with_variants) c("pearson", "yates")
  shaped <- function(fn) {
    function(vals, variant) if (is.null(dims)) NaN else fn(vals, variant)
  }
  stat <- function(vals, variant) {
    chisq_table_stat(vals$n, dims, identical(variant, "yates"))
  }
  list(
    relation("n_total_from_n", "n", "n_total", function(vals, variant) sum(vals$n)),
    relation("chisq_from_table", "n", "chisq", shaped(stat),
             variants = tests, variant_kind = "test"),
    relation("df_from_dims", "chisq", "df", shaped(function(vals, variant) prod(dims - 1))),
    relation("p_from_table", "n", "p", shaped(function(vals, variant) {
      stats::pchisq(stat(vals, variant), prod(dims - 1), lower.tail = FALSE)
    }), variants = tests, variant_kind = "test"),
    relation_p_from_statistic("chisq"),
    relation("v_from_table", "n", "v", shaped(function(vals, variant) {
      independence_effect(chisq_table_stat(vals$n, dims), sum(vals$n), dims)
    }), variants = "v", variant_kind = "convention", falsifies = FALSE),
    relation("v_from_chisq", c("chisq", "n_total"), "v", shaped(function(vals, variant) {
      independence_effect(vals$chisq, vals$n_total, dims)
    }), variants = "v", variant_kind = "convention", falsifies = FALSE)
  )
}

chisq_gof_relations <- function(dims = NULL, p0 = NULL) {
  if (!is.null(dims)) check_table_dims(dims, "gof")
  if (!is.null(p0)) check_p0(p0, dims)
  shaped <- function(fn) function(vals, variant) if (is.null(dims)) NaN else fn(vals, variant)
  stat <- function(vals) chisq_gof_stat(vals$n, p0, if (is.null(dims)) length(vals$n) else dims)
  list(
    relation("n_total_from_n", "n", "n_total", function(vals, variant) sum(vals$n)),
    relation("chisq_from_counts", "n", "chisq", function(vals, variant) stat(vals)),
    relation("df_from_dims", "chisq", "df", shaped(function(vals, variant) dims - 1)),
    relation_p_from_statistic("chisq"),
    relation("w_from_counts", "n", "v", function(vals, variant) {
      effect_from_chisq(stat(vals), sum(vals$n), 1)
    }, variants = "w", variant_kind = "convention", falsifies = FALSE),
    relation("w_from_chisq", c("chisq", "n_total"), "v", function(vals, variant) {
      effect_from_chisq(vals$chisq, vals$n_total, 1)
    }, variants = "w", variant_kind = "convention", falsifies = FALSE)
  )
}

# Share side -------------------------------------------------------------------

chisq_independence_frame <- function(x, data) {
  mf <- stats::model.frame(x, data)
  if (ncol(mf) != 2L) stop("The formula must be `~ a + b`.", call. = FALSE)
  table(factor(mf[[1L]]), factor(mf[[2L]]), dnn = names(mf))
}

chisq_independence_summarise <- function(x, data, test, correct = TRUE, ...) {
  tab <- chisq_independence_frame(x, data)
  dims <- dim(tab)
  if (any(dims < 2L)) {
    stop("Both variables need at least 2 categories; got ", dims[1L], " and ", dims[2L], ".",
         call. = FALSE)
  }
  yates <- isTRUE(correct) && all(dims == 2L)
  list(
    stats = list(n = as.vector(tab)),
    meta = list(
      n = sum(tab), test = test, dims = dims, correct = isTRUE(correct),
      test_variant = if (yates) "yates" else "pearson",
      row_name = names(dimnames(tab))[1L], col_name = names(dimnames(tab))[2L],
      row_labels = rownames(tab), col_labels = colnames(tab),
      cell_labels = paste0("(", rep(rownames(tab), times = dims[2L]), ", ",
                           rep(colnames(tab), each = dims[1L]), ")")
    )
  )
}

chisq_independence_reproduce <- function(s) {
  n <- s$stats$n
  dims <- s$meta$dims
  chisq <- chisq_table_stat(n, dims, isTRUE(s$meta$correct) && all(dims == 2L))
  df <- prod(dims - 1)
  c(chisq = chisq, df = df, p = stats::pchisq(chisq, df, lower.tail = FALSE),
    n_total = sum(n), v = independence_effect(chisq_table_stat(n, dims), sum(n), dims))
}

chisq_independence_reported <- function(x, data, test, correct = TRUE, ...) {
  tab <- chisq_independence_frame(x, data)
  r <- suppressWarnings(stats::chisq.test(tab, correct = correct))
  pearson <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic
  c(chisq = unname(r$statistic), df = unname(r$parameter), p = r$p.value,
    n_total = sum(tab), v = unname(sqrt(pearson / (sum(tab) * (min(dim(tab)) - 1)))))
}

chisq_gof_table <- function(x, data) {
  mf <- stats::model.frame(x, data)
  if (ncol(mf) != 1L) stop("The formula must be `~ a`.", call. = FALSE)
  f <- mf[[1L]]
  table(if (is.factor(f)) f else factor(f), dnn = names(mf))
}

chisq_gof_summarise <- function(x, data, test, p0 = NULL, ...) {
  tab <- chisq_gof_table(x, data)
  k <- length(tab)
  if (k < 2L) stop("The variable needs at least 2 categories, not ", k, ".", call. = FALSE)
  if (is.null(p0)) p0 <- rep(1 / k, k)
  check_p0(p0, k)
  if (any(p0 <= 0)) stop("`p0` must be positive for every category.", call. = FALSE)
  list(
    stats = list(n = as.vector(tab), p0 = as.numeric(p0)),
    meta = list(n = sum(tab), test = test, dims = k, p0 = as.numeric(p0),
                var_name = names(dimnames(tab)), labels = names(tab),
                cell_labels = paste0("'", names(tab), "'"))
  )
}

chisq_gof_reproduce <- function(s) {
  n <- s$stats$n
  p0 <- s$meta$p0
  chisq <- chisq_gof_stat(n, p0)
  df <- length(n) - 1
  c(chisq = chisq, df = df, p = stats::pchisq(chisq, df, lower.tail = FALSE),
    n_total = sum(n), v = effect_from_chisq(chisq, sum(n), 1))
}

chisq_gof_reported <- function(x, data, test, p0 = NULL, ...) {
  tab <- chisq_gof_table(x, data)
  if (is.null(p0)) p0 <- rep(1 / length(tab), length(tab))
  r <- suppressWarnings(stats::chisq.test(tab, p = p0))
  c(chisq = unname(r$statistic), df = unname(r$parameter), p = r$p.value,
    n_total = sum(tab), v = unname(sqrt(r$statistic / sum(tab))))
}

# The counts table for block A of the appendix.
chisq_appendix_table <- function(s) {
  n <- s$stats$n
  if (length(s$meta$dims) == 2L) {
    dims <- s$meta$dims
    tab <- matrix(n, nrow = dims[1L], ncol = dims[2L])
    c(appendix_row(c(paste0(s$meta$row_name, " \\ ", s$meta$col_name), s$meta$col_labels)),
      appendix_row(rep("---", dims[2L] + 1L)),
      vapply(seq_len(dims[1L]), function(i) {
        appendix_row(c(s$meta$row_labels[i], tab[i, ]))
      }, character(1)))
  } else {
    c(appendix_row(c(s$meta$var_name, s$meta$labels)),
      appendix_row(rep("---", length(n) + 1L)),
      appendix_row(c("n", n)))
  }
}
