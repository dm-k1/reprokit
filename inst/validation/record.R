# Records what a validation script prints, for the validation vignette (plan 8: the evidence
# has to outlive the conversation that produced it). Sourced by each script.
#   record_start("roundtrip-simulation", seed = 20261122)  # at the top, after the header
#   record_stop()                                            # at the end
# The printed output goes to the console as usual and, with a header of the seed, the package
# version, the date and the R version, to inst/validation/results/<script>.txt. Run from the
# package root with devtools::load_all() (the scripts need internal functions anyway).
record_start <- function(script, seed) {
  dir <- file.path("inst", "validation", "results")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, paste0(script, ".txt"))
  version <- tryCatch(as.character(utils::packageVersion("reprokit")), error = function(e) NA)
  writeLines(c(paste0("script: inst/validation/", script, ".R"),
               paste0("seed: ", paste(seed, collapse = ", ")),
               paste0("package: reprokit ", version),
               paste0("date: ", format(Sys.Date())),
               paste0("R: ", R.version.string),
               ""), path)
  sink(path, append = TRUE, split = TRUE)
  invisible(path)
}

record_stop <- function() {
  while (sink.number() > 0L) sink()
  invisible()
}

# Ends a validation script: closes the record and, when run with Rscript (not interactively),
# exits with status 0 on PASS and 1 on FAIL so a release script can stop on a failure.
# Sourced interactively it only returns `pass`.
finish_validation <- function(pass) {
  record_stop()
  if (!interactive()) quit(save = "no", status = if (isTRUE(pass)) 0L else 1L)
  invisible(pass)
}
