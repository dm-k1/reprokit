# Printed numbers come from the model (ADR 0031 point 5).
#
# When appendix() is given a model or a test result (not a sufficient_stats object), the
# numbers it prints are the ones the model itself holds (summary(lm), the aov table, the
# htest fields). Every one of them is also recomputed from the stored summaries with
# reproduce(); a value that differs by more than the round-trip tolerance (scaled difference,
# 1e-8) means the fit is not what its data says, and the appendix is refused.
#
# The reference values live in an internal environment for the duration of one appendix()
# call; source_reproduce() is reproduce() with the model's values laid over it.

.appendix_source <- new.env(parent = emptyenv())

# reproduce(s) (or the handler's own reproduce for a summaries object), with the model's
# values substituted for the keys the model holds.
source_reproduce <- function(s, h = get_handler(s$handler)) {
  r <- h$reproduce(s)
  ref <- .appendix_source$reference
  if (is.null(ref) || !identical(.appendix_source$handler, s$handler)) return(r)
  keys <- intersect(names(ref), names(r))
  r[keys] <- ref[keys]
  r
}

# Run `code` with the model reference in place, and take it away afterwards.
with_model_source <- function(reference, handler, code) {
  .appendix_source$reference <- reference
  .appendix_source$handler <- handler
  on.exit({
    .appendix_source$reference <- NULL
    .appendix_source$handler <- NULL
  }, add = TRUE)
  force(code)
}

# The model's own numbers as a named numeric vector in the key space of reproduce(); NULL
# when x holds none (a formula, or a route whose numbers cannot be read off without
# refitting). Never an error: a model whose values cannot be read is simply not a source.
appendix_model_reference <- function(x, dots, handler) {
  tryCatch(model_reference(x, dots, handler), error = function(e) NULL)
}

model_reference <- function(x, dots, handler) {
  if (inherits(x, "formula")) return(NULL)
  if (inherits(x, "aov")) {
    table <- summary(x)[[1L]]
    if (!identical(handler, "anova_oneway") || nrow(table) != 2L) return(NULL)
    ss <- table[["Sum Sq"]]
    return(c(f = table[["F value"]][1L], df1 = table[["Df"]][1L], df2 = table[["Df"]][2L],
             p = table[["Pr(>F)"]][1L], eta2 = ss[1L] / sum(ss)))
  }
  if (inherits(x, "glm")) return(NULL)
  if (identical(class(x), "lm")) {
    second <- Filter(function(d) identical(class(d), "lm"), dots)
    if (length(second) == 1L) return(mediation_model_reference(list(x, second[[1L]])))
    return(lm_reported(x))
  }
  if (inherits(x, "htest")) return(htest_reference(x))
  NULL
}

htest_reference <- function(x) {
  stat <- names(x$statistic)
  p <- unname(x$p.value)
  if (identical(stat, "t")) {
    out <- c(t = unname(x$statistic), df = unname(x$parameter), p = p)
    if (identical(names(x$estimate), "cor")) out <- c(r = unname(x$estimate), out)
    return(out)
  }
  if (identical(stat, "X-squared")) {
    return(c(chisq = unname(x$statistic), df = unname(x$parameter), p = p))
  }
  NULL
}

# Two lm fits of a simple mediation: a, b and c' read off the fits; c (X to Y) and its model
# have no source model among the two, so they are refitted from the model frames. The
# per-model R squared and F go in as `r2:<k>` and `F:<k>` (k = 1 a-model, 2 b/c'-model,
# 3 c-model), which mediation_model_values() consults.
mediation_model_reference <- function(fits) {
  width <- vapply(fits, function(f) ncol(f$model), numeric(1))
  if (!setequal(width, c(2, 3))) return(NULL)
  med <- fits[[which(width == 2)]]
  outc <- fits[[which(width == 3)]]
  mn <- names(med$model)[1L]
  xn <- names(med$model)[2L]
  yn <- names(outc$model)[1L]
  d <- data.frame(outc$model[[xn]], outc$model[[mn]], outc$model[[yn]], check.names = FALSE)
  names(d) <- c(xn, mn, yn)
  y_on_x <- lm_reported(stats::lm(stats::reformulate(paste0("`", xn, "`"),
                                                     paste0("`", yn, "`")), data = d))
  a <- lm_reported(med)
  b <- lm_reported(outc)
  out <- mediation_gather(a, y_on_x, b, xn, mn)
  models <- list(a, b, y_on_x)
  per_model <- unlist(lapply(seq_along(models), function(k) {
    stats::setNames(c(models[[k]][["r.squared"]], models[[k]][["F"]]),
                    paste0(c("r2:", "F:"), k))
  }))
  c(out, per_model)
}

# The scaled differences between the model's values and the recomputed ones, keyed alike.
# Refuse (abort_impossible) when any exceeds the tolerance.
guard_model_source <- function(reference, recomputed) {
  keys <- intersect(names(reference), names(recomputed))
  if (length(keys) == 0L) return(invisible(NULL))
  differences <- scaled_differences(reference[keys], recomputed[keys])
  if (all(differences < match_tolerance)) return(invisible(NULL))
  worst <- names(differences)[which.max(differences)]
  abort_impossible(
    "write an appendix for this model",
    paste0(
      "the model's own value of ", worst, " (", format(reference[[worst]], digits = 10),
      ") differs from the one recomputed from its summaries (",
      format(recomputed[[worst]], digits = 10), "). The fit does not follow from its ",
      "data, so printing either number would misreport it."
    ),
    alternatives = "Refit the model from the data, then call appendix() again."
  )
}
