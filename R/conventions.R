#' List the reporting conventions each handler accepts
#'
#' Some results depend on a choice the statistics cannot show: which effect size
#' was computed (`choices$convention` in [appendix()]) and, for an unbalanced
#' factorial ANOVA, the type of sums of squares (`choices$ss_type`, also
#' `ss_type` in [sufficient_stats()]). `conventions()` lists the codes reprokit
#' accepts for them. It reads the handler registry, the same place [appendix()]
#' validates against, so the list cannot drift from the package.
#'
#' `default` is `TRUE` for the code reprokit uses when the argument is left out.
#' Only the sums of squares type has one among several codes (type III). An
#' effect-size convention with several codes has no default: leaving it out
#' means "not declared", and the appendix says so. A handler with a single code
#' (V, w) uses it.
#'
#' @param handler A handler name from [handlers()], or `NULL` for all handlers
#'   that accept a convention choice.
#' @return A data frame with one row per accepted code and columns `handler`,
#'   `choice` (`"convention"` or `"ss_type"`, the name used in `choices`),
#'   `code` (the exact string reprokit accepts), `default` (logical) and `label`
#'   (the short label used in the appendix).
#' @export
#' @examples
#' conventions("t_paired")
#' subset(conventions(), choice == "ss_type")
conventions <- function(handler = NULL) {
  if (!is.null(handler) &&
        !(is.character(handler) && length(handler) == 1L && handler %in% handlers())) {
    abort_impossible(
      "list conventions for an unknown handler",
      paste0("`handler` must be one of: ", paste0("\"", handlers(), "\"", collapse = ", "), ".")
    )
  }
  rows <- lapply(if (is.null(handler)) handlers() else handler, function(name) {
    h <- get_handler(name)
    one <- function(choice, codes, default) {
      if (length(codes) == 0L) return(NULL)
      data.frame(handler = name, choice = choice, code = codes, default = codes %in% default,
                 label = unname(variant_labels()[codes]), stringsAsFactors = FALSE)
    }
    convention <- variant_codes(h, "convention")
    ss_type <- if (has_ss_types(h)) variant_codes(h, "test")
    rbind(one("convention", convention, if (length(convention) == 1L) convention),
          one("ss_type", ss_type, ss_type_codes()[1L]))
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(handler = character(), choice = character(), code = character(),
                      default = logical(), label = character(), stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}
