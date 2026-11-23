# ADR 0002: the app runs in the browser via webR, so the package must stay
# dependency-light. Heavy packages (lme4, lavaan, ...) go in Suggests.
test_that("Imports stay within base R packages", {
  desc <- system.file("DESCRIPTION", package = "reprokit")
  skip_if(desc == "", "package not installed")
  imports <- read.dcf(desc, fields = "Imports")[1, 1]
  pkgs <- if (is.na(imports)) character() else trimws(sub("\\(.*", "", strsplit(imports, ",")[[1]]))
  expect_true(all(pkgs %in% c("stats", "utils", "methods")),
              info = paste("Imports:", paste(pkgs, collapse = ", ")))
})

test_that("the R code uses only base R packages through ::", {
  # Under covr or R CMD check the tests can run next to the installed package, whose R/ holds
  # only the lazy-load database. No .R files there means no sources to scan, not a pass.
  r_dir <- testthat::test_path("..", "..", "R")
  sources <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  skip_if(length(sources) == 0L, "package sources not available")
  used <- unlist(lapply(sources, function(path) {
    parsed <- utils::getParseData(parse(path, keep.source = TRUE))
    parsed$text[parsed$token == "SYMBOL_PACKAGE"]
  }))
  expect_true(length(used) > 0L, info = "the scan found no pkg:: calls, so it is not scanning")
  expect_true(all(used %in% c("stats", "utils", "methods")),
              info = paste("used:", paste(sort(unique(used)), collapse = ", ")))
})
