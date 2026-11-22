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
