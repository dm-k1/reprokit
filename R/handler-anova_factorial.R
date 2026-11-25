# Factorial between-subjects ANOVA, two factors (classical F tests, equal variances assumed).
# In one line: per-cell n, mean and SD determine F, df1, df2, p and partial eta squared of
# each of the three effects (A, B, A x B) exactly; the SS type matters only when the cells
# differ in size (ADR 0017).

register_anova_factorial <- function() {
  register_handler(
    name = "anova_factorial",
    family = "Simple tests",
    analysis = "Factorial ANOVA, two factors (between-subjects)",
    verdict = "summaries_suffice",
    needs = "per-cell n, mean and SD",
    catch = paste0("declare the SS type (unbalanced designs) and partial eta squared or ",
                   "partial omega squared"),
    lost = c("equal variances assumed (pooled error term)",
             "checking the distribution and outliers within each cell",
             paste("simple effects, post-hoc comparisons and contrasts that need the raw data",
                   "or an error term other than the pooled one"),
             "covariates and any factor other than the two in the model"),
    matches = function(x, test = NULL, ...) {
      inherits(x, "formula") && is.character(test) && length(test) == 1L && test == "factorial"
    },
    summarise = anova_factorial_summarise,
    reproduce = anova_factorial_reproduce,
    reported = anova_factorial_reported,
    disclosure = function(summary, threshold) {
      n <- summary$stats$n
      small <- which(n < threshold)
      list(
        passed = length(small) == 0L,
        checked = "cell sizes",
        problems = if (length(small)) {
          paste0("cell ", summary$meta$cell_labels[small], " has n = ", n[small], " < ", threshold)
        } else {
          character()
        }
      )
    },
    formula_tests = "factorial",
    design = apa_design(statistic = "f", effect_size = "eta2", cells = NA_integer_, ci = FALSE),
    sufficient = c("n", "m", "sd"),
    per_cell = c("n", "m", "sd"),
    # Partial omega squared can be negative, unlike partial eta squared.
    domains = list(eta2 = c(-1, 1)),
    labels = c(eta2 = "partial eta2"),
    relations = anova_factorial_relations,
    appendix_table = anova_factorial_appendix_table,
    appendix_effects = anova_factorial_appx_effects,
    example = function() {
      d <- base_data("ToothGrowth")
      d$dose <- factor(d$dose)
      list(x = len ~ supp * dose, args = list(data = d, test = "factorial"))
    }
  )
}

# Shape and effects ------------------------------------------------------------

check_factorial_dims <- function(dims) {
  if (!(is.numeric(dims) && length(dims) == 2L && all(is.finite(dims)) &&
          all(dims == round(dims)) && all(dims >= 2))) {
    abort_input("`dims` must be c(levels of A, levels of B), two whole numbers of at least 2.")
  }
  invisible(dims)
}

factorial_df1 <- function(effect, dims) {
  switch(effect, A = dims[1L] - 1, B = dims[2L] - 1, AB = (dims[1L] - 1) * (dims[2L] - 1))
}

factorial_effect_words <- function(effect) {
  switch(effect,
         A = "A (the factor in the rows of the cell matrices)",
         B = "B (the factor in the columns)",
         AB = "AB (the interaction)")
}

# Sum-coded columns over the cells, in R's column-major order (A varies fastest).
factorial_contrasts <- function(dims) {
  a <- dims[1L]
  b <- dims[2L]
  ca <- stats::contr.sum(a)[rep(seq_len(a), times = b), , drop = FALSE]
  cb <- stats::contr.sum(b)[rep(seq_len(b), each = a), , drop = FALSE]
  ab <- do.call(cbind, lapply(seq_len(ncol(cb)), function(j) ca * cb[, j]))
  list(one = matrix(1, a * b, 1L), A = ca, B = cb, AB = ab)
}

# The columns an effect is adjusted for (ADR 0017 point 5): type III adjusts for every
# other term, type II for the terms that do not contain it, type I (the effect entered
# first) for the intercept only. The interaction is last in every type.
factorial_adjust <- function(effect, type) {
  other <- c(A = "B", B = "A")[effect]
  switch(effect,
         AB = c("one", "A", "B"),
         switch(type,
                III = c("one", other, "AB"),
                II = c("one", other),
                I = "one"))
}

# L, one row per df of the effect: the effect's hypothesis L mu = 0 on the cell means,
# whose sum of squares is (L mu)' (L D^-1 L')^-1 (L mu) with D = diag(n). L = Z' W, where
# Z is the effect's coded columns after removing what the adjusted columns explain
# under the cell sizes W = D. NULL when the cell sizes leave nothing to test.
factorial_l <- function(effect, dims, n, type) {
  cm <- factorial_contrasts(dims)
  e <- cm[[effect]]
  x <- do.call(cbind, cm[factorial_adjust(effect, type)])
  xtw <- t(x * n)
  z <- tryCatch(e - x %*% solve(xtw %*% x, xtw %*% e), error = function(cond) NULL)
  if (is.null(z) || !all(is.finite(z))) return(NULL)
  l <- t(z * n)
  if (max(abs(l)) < 1e-12) return(NULL)
  l
}

# Sum of squares of an effect from the cell sizes and means.
factorial_ss <- function(effect, dims, n, m, type) {
  l <- factorial_l(effect, dims, n, type)
  if (is.null(l)) return(NaN)
  lm_ <- l %*% m
  drop(crossprod(lm_, solve(l %*% (t(l) / n), lm_)))
}

factorial_sse <- function(n, sd) sum((n - 1) * sd^2)

# F of one effect from the per-cell n, mean and SD. NaN when the shape is unknown or does
# not fit the cells (not evaluable).
factorial_f <- function(effect, dims, n, m, sd, type = "III") {
  cells <- prod(dims)
  if (length(n) != cells || length(m) != cells || length(sd) != cells) return(NaN)
  df2 <- sum(n) - cells
  (factorial_ss(effect, dims, n, m, type) / factorial_df1(effect, dims)) /
    (factorial_sse(n, sd) / df2)
}

factorial_ready <- function(effect, dims) !is.null(effect) && !is.null(dims)

# Exact range of F over the box of n, m and sd, or an unbounded range (not evaluable).
# n must be exact (a count), as it is when a paper reports the cell sizes.
# * df1 = 1: SS = (L mu)^2 / (L D^-1 L'), L mu is linear in the means, so its range is
#   exact by interval arithmetic; the minimum of the square is 0 if the range holds 0.
# * balanced main effect: SS is the one-way SSb of the marginal means, whose intervals are
#   exact and independent (the means of disjoint cells).
# * anything else (the interaction with df1 > 1, an unbalanced effect with df1 > 1): no exact
#   range; the relation is skipped, so those effects are checked for coherence only.
factorial_f_range <- function(effect, dims) {
  unbounded <- c(lower = NA_real_, upper = NA_real_)
  function(intervals, variant) {
    if (!factorial_ready(effect, dims)) return(unbounded)
    cells <- prod(dims)
    n <- interval_bounds(intervals$n)
    m <- interval_bounds(intervals$m)
    sd <- interval_bounds(intervals$sd)
    if (any(lengths(list(n$lower, m$lower, sd$lower)) != cells) || any(n$lower != n$upper) ||
          any(n$lower < 2)) {
      return(unbounded)
    }
    counts <- n$lower
    df2 <- sum(counts) - cells
    df1 <- factorial_df1(effect, dims)
    type <- if (is.null(variant)) "III" else variant
    sse_high <- factorial_sse(counts, sd$upper)
    sse_low <- factorial_sse(counts, pmax(sd$lower, 0))
    if (df2 < 1 || sse_high <= 0) return(unbounded)
    if (df1 == 1) {
      l <- factorial_l(effect, dims, counts, type)
      if (is.null(l)) return(unbounded)
      l <- l[1L, ]
      scale <- sum(l^2 / counts)
      reach <- c(sum(pmin(l * m$lower, l * m$upper)), sum(pmax(l * m$lower, l * m$upper)))
      ss_min <- if (reach[1L] <= 0 && reach[2L] >= 0) 0 else min(reach^2) / scale
      ss_max <- max(reach^2) / scale
    } else if (effect != "AB" && length(unique(counts)) == 1L && dims[1L] * 0 == 0) {
      along <- if (effect == "A") 1L else 2L
      levels <- dims[along]
      if (levels > 12L) return(unbounded)
      margin <- function(x) {
        mat <- matrix(x, dims[1L], dims[2L])
        if (along == 1L) rowMeans(mat) else colMeans(mat)
      }
      weights <- rep(counts[1L] * dims[3L - along], levels)
      ss_min <- oneway_ssb_min(weights, margin(m$lower), margin(m$upper))
      ss_max <- oneway_ssb_max(weights, margin(m$lower), margin(m$upper))
    } else {
      return(unbounded)
    }
    c(lower = if (ss_min == 0) 0 else ss_min / df1 / (sse_high / df2),
      upper = ss_max / df1 / (sse_low / df2))
  }
}

# p from n, mean and SD at one SS type: monotone in F, so its range is the F range mapped.
factorial_p_range <- function(effect, dims) {
  f_range <- factorial_f_range(effect, dims)
  function(intervals, variant) {
    f <- f_range(intervals, variant)
    if (!all(is.finite(f))) return(c(lower = NA_real_, upper = NA_real_))
    n <- interval_bounds(intervals$n)
    df1 <- factorial_df1(effect, dims)
    df2 <- sum(n$lower) - prod(dims)
    c(lower = stats::pf(f[["upper"]], df1, df2, lower.tail = FALSE),
      upper = stats::pf(f[["lower"]], df1, df2, lower.tail = FALSE))
  }
}

# Relations ----------------------------------------------------------------------

# `effect = NULL, dims = NULL` (the registry's default) has the SS-type variants and no
# shape: the shape-dependent relations are then not evaluable. The SS type is a `test`
# variant of F and p for a main effect of a design that is not known to be balanced
# (`balanced`, set by check_reported() from the reported n); the interaction has one value
# in every type, and so does every effect of a balanced design (ADR 0017 point 5).
#
# F and p both vary over the SS type, so shared_choice() makes them agree on one. p has its
# own relation from the descriptives for that reason. The effect size is a convention
# (partial eta squared F df1 / (F df1 + df2); partial omega squared df1 (F - 1) /
# (df1 (F - 1) + N), N = df2 + number of cells) that cannot falsify. The two are separate
# relations so that a missing shape, which partial omega squared needs for N, leaves partial
# eta squared checkable.
anova_factorial_relations <- function(effect = NULL, dims = NULL, balanced = NULL) {
  if (!is.null(effect) && !(is.character(effect) && length(effect) == 1L &&
                              effect %in% c("A", "B", "AB"))) {
    abort_input("`effect` must be one of \"A\", \"B\" or \"AB\".")
  }
  if (!is.null(dims)) check_factorial_dims(dims)
  ready <- factorial_ready(effect, dims)
  with_types <- !isTRUE(balanced) && !identical(effect, "AB")
  variants <- if (with_types) list(test = ss_type_codes())
  type_of <- function(variant) if (is.null(variant)) "III" else variant
  f_value <- function(vals, variant) {
    if (!ready) return(NaN)
    factorial_f(effect, dims, vals$n, vals$m, vals$sd, type_of(variant))
  }
  list(
    relation("df1_from_dims", "f", "df1", function(vals, variant) {
      if (ready) factorial_df1(effect, dims) else NaN
    }),
    relation("df2_from_n", "n", "df2", function(vals, variant) sum(vals$n) - length(vals$n)),
    relation("f_from_descriptives", c("n", "m", "sd"), "f", f_value, variants = variants,
             variant_kind = "test", range = factorial_f_range(effect, dims)),
    relation("p_from_descriptives", c("n", "m", "sd"), "p", function(vals, variant) {
      if (!ready) return(NaN)
      stats::pf(f_value(vals, variant), factorial_df1(effect, dims),
                sum(vals$n) - length(vals$n), lower.tail = FALSE)
    }, variants = variants, variant_kind = "test", range = factorial_p_range(effect, dims)),
    relation_p_from_statistic("F"),
    relation("eta2_from_f", c("f", "df1", "df2"), "eta2", function(vals, variant) {
      explained <- vals$f * vals$df1
      explained / (explained + vals$df2)
    }, variants = "partial_eta2", variant_kind = "convention", falsifies = FALSE),
    relation("eta2_omega_from_f", c("f", "df1", "df2"), "eta2", function(vals, variant) {
      if (is.null(dims)) return(NaN)
      gain <- vals$df1 * (vals$f - 1)
      gain / (gain + vals$df2 + prod(dims))
    }, variants = "partial_omega2", variant_kind = "convention", falsifies = FALSE)
  )
}

# Share side -----------------------------------------------------------------------

# Everything an effect's row reports, from n, mean and SD of the cells.
factorial_effects <- function(n, m, sd, dims, type = "III") {
  cells <- prod(dims)
  df2 <- sum(n) - cells
  sse <- factorial_sse(n, sd)
  out <- lapply(stats::setNames(nm = c("A", "B", "AB")), function(effect) {
    df1 <- factorial_df1(effect, dims)
    ss <- factorial_ss(effect, dims, n, m, type)
    f <- (ss / df1) / (sse / df2)
    list(f = f, df1 = df1, p = stats::pf(f, df1, df2, lower.tail = FALSE),
         eta2 = ss / (ss + sse))
  })
  list(effects = out, df2 = df2)
}

# The named vector reproduce() and reported() both return.
factorial_vector <- function(effects, df2) {
  values <- unlist(lapply(names(effects), function(effect) {
    e <- effects[[effect]]
    stats::setNames(c(e$f, e$df1, e$p, e$eta2),
                    paste0(c("f", "df1", "p", "eta2"), "_", effect))
  }))
  c(values, df2 = df2)
}

anova_factorial_summarise <- function(x, data, test, ss_type = NULL, ...) {
  if (!is.null(ss_type) && !(is.character(ss_type) && length(ss_type) == 1L &&
                               ss_type %in% ss_type_codes())) {
    abort_input(
      "`ss_type` must be one of ", paste0("\"", ss_type_codes(), "\"", collapse = ", "),
      "."
    )
  }
  mf <- factorial_frame(x, data)
  y <- mf[[1L]]
  if (!is.numeric(y)) abort_input("The outcome must be numeric.")
  a <- mf[[2L]]
  b <- mf[[3L]]
  tab <- table(a, b)
  dims <- dim(tab)
  if (any(dims < 2L)) {
    abort_input("Both factors need at least 2 levels; got ", dims[1L], " and ", dims[2L], ".")
  }
  if (any(tab == 0L)) {
    abort_input(
      "Every cell needs observations; the cell (",
      paste(c(rownames(tab)[which(tab == 0L, arr.ind = TRUE)[1L, 1L]],
              colnames(tab)[which(tab == 0L, arr.ind = TRUE)[1L, 2L]]), collapse = ", "),
      ") is empty."
    )
  }
  if (any(tab < 2L)) {
    abort_input(
      "Every cell needs at least 2 observations (its SD must be defined); some cells have fewer."
    )
  }
  cell_mean <- tapply(y, list(a, b), mean)
  cell_sd <- tapply(y, list(a, b), stats::sd)
  labels <- paste0("(", rep(rownames(tab), times = dims[2L]), ", ",
                   rep(colnames(tab), each = dims[1L]), ")")
  list(
    stats = list(n = as.vector(tab), mean = as.vector(cell_mean), sd = as.vector(cell_sd)),
    meta = list(
      n = length(y), test = test, dims = dims,
      balanced = length(unique(as.vector(tab))) == 1L,
      ss_type = ss_type,
      factor_names = names(mf)[2:3],
      level_labels = list(rownames(tab), colnames(tab)),
      cell_labels = labels
    )
  )
}

# The model frame of `y ~ a * b`, the two predictors as factors.
factorial_frame <- function(x, data) {
  mf <- stats::model.frame(x, data)
  labels <- attr(stats::terms(x), "term.labels")
  expected_labels <- c(names(mf)[2:3], paste(names(mf)[2:3], collapse = ":"))
  if (ncol(mf) != 3L || !setequal(labels, expected_labels)) {
    abort_input("The formula must be `outcome ~ a * b`: two factors and their interaction.")
  }
  mf[[2L]] <- factor(mf[[2L]])
  mf[[3L]] <- factor(mf[[3L]])
  mf
}

anova_factorial_reproduce <- function(s) {
  type <- if (is.null(s$meta$ss_type)) "III" else s$meta$ss_type
  e <- factorial_effects(s$stats$n, s$stats$mean, s$stats$sd, s$meta$dims, type)
  factorial_vector(e$effects, e$df2)
}

# Base R only (ADR 0017 point 8): type I from anova() with the factor entered first, type
# II from drop1() on the additive model, type III from drop1() with sum contrasts. The F
# tests use the error term of the full model in every type.
anova_factorial_reported <- function(x, data, test, ss_type = NULL, ...) {
  type <- if (is.null(ss_type)) "III" else ss_type
  mf <- factorial_frame(x, data)
  d <- data.frame(y = mf[[1L]], a = mf[[2L]], b = mf[[3L]])
  sums <- function(fit, rows) unname(stats::anova(fit)[rows, "Sum Sq"])
  full <- stats::lm(y ~ a * b, d)
  ss <- switch(
    type,
    I = c(A = sums(full, "a"), B = sums(stats::lm(y ~ b * a, d), "b"), AB = sums(full, "a:b")),
    II = {
      dropped <- stats::drop1(stats::lm(y ~ a + b, d), scope = ~ a + b)
      c(A = dropped["a", "Sum of Sq"], B = dropped["b", "Sum of Sq"], AB = sums(full, "a:b"))
    },
    III = {
      coded <- stats::lm(y ~ a * b, d, contrasts = list(a = "contr.sum", b = "contr.sum"))
      dropped <- stats::drop1(coded, scope = ~ a + b + a:b)
      c(A = dropped["a", "Sum of Sq"], B = dropped["b", "Sum of Sq"],
        AB = dropped["a:b", "Sum of Sq"])
    }
  )
  df2 <- stats::df.residual(full)
  sse <- stats::deviance(full)
  dims <- c(nlevels(d$a), nlevels(d$b))
  effects <- lapply(stats::setNames(nm = names(ss)), function(effect) {
    df1 <- factorial_df1(effect, dims)
    f <- (ss[[effect]] / df1) / (sse / df2)
    list(f = f, df1 = df1, p = stats::pf(f, df1, df2, lower.tail = FALSE),
         eta2 = ss[[effect]] / (ss[[effect]] + sse))
  })
  factorial_vector(effects, df2)
}

# Appendix -------------------------------------------------------------------------

# One row per cell: the levels, then n, M and SD.
anova_factorial_appendix_table <- function(s) {
  dims <- s$meta$dims
  names_ <- s$meta$factor_names
  levels_ <- s$meta$level_labels
  digits <- 2L
  c(appendix_row(c(names_, "n", "M", "SD")),
    appendix_row(rep("---", 5L)),
    vapply(seq_along(s$stats$n), function(i) {
      appendix_row(c(levels_[[1L]][(i - 1L) %% dims[1L] + 1L],
                     levels_[[2L]][(i - 1L) %/% dims[1L] + 1L],
                     s$stats$n[i], appendix_number(s$stats$mean[i], digits),
                     appendix_number(s$stats$sd[i], digits)))
    }, character(1)))
}

# The three F tests with their df, p and the declared effect size. A main effect of an
# unbalanced design needs the declared SS type; without it the row says so instead of
# guessing (the interaction does not depend on it).
anova_factorial_appx_effects <- function(s, answers, digits) {
  type <- answers$test
  balanced <- isTRUE(s$meta$balanced)
  names_ <- s$meta$factor_names
  dims <- s$meta$dims
  convention <- answers$convention
  convention_text <- if (is.na(convention)) "effect size" else variant_label(convention)
  header <- c(appendix_row(c("Effect", "F(df1, df2)", "p", convention_text)),
              appendix_row(rep("---", 4L)))
  at <- function(effect_type) {
    e <- factorial_effects(s$stats$n, s$stats$mean, s$stats$sd, dims, effect_type)
    list(effects = e$effects, df2 = e$df2)
  }
  rows <- vapply(c("A", "B", "AB"), function(effect) {
    label <- switch(effect, A = names_[1L], B = names_[2L],
                    AB = paste0(names_[1L], " \u00d7 ", names_[2L]))
    needs_type <- effect != "AB" && !balanced && is.na(type)
    if (needs_type) {
      return(appendix_row(c(label, "not computed: declare the SS type (block C)", "-", "-")))
    }
    res <- at(if (is.na(type)) "III" else type)
    e <- res$effects[[effect]]
    size <- if (is.na(convention)) {
      "not computed: declare the effect-size convention (block C)"
    } else if (convention == "partial_eta2") {
      appendix_number(e$eta2, digits, leading_zero = FALSE)
    } else {
      gain <- e$df1 * (e$f - 1)
      appendix_number(gain / (gain + res$df2 + prod(dims)), digits, leading_zero = FALSE)
    }
    appendix_row(c(label, paste0("F(", appendix_df(e$df1, digits), ", ",
                                 appendix_df(res$df2, digits), ") = ",
                                 appendix_number(e$f, digits)),
                   appendix_p(e$p), size))
  }, character(1), USE.NAMES = FALSE)
  c(header, rows)
}
