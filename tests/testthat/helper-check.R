# Helpers shared by the check_reported() tests.
fixed <- function(x, digits) formatC(unname(x), format = "f", digits = digits)

paper_paired <- function() {
  d <- datasets::sleep
  first <- d$extra[d$group == 1]
  second <- d$extra[d$group == 2]
  result <- stats::t.test(first, second, paired = TRUE)
  list(n = as.character(length(first)),
       m = fixed(c(mean(first), mean(second)), 2),
       sd = fixed(c(sd(first), sd(second)), 2),
       r = fixed(stats::cor(first, second), 3),
       t = fixed(result$statistic, 2), df = as.character(result$parameter),
       p = fixed(result$p.value, 3),
       ci_lower = fixed(result$conf.int[1], 2), ci_upper = fixed(result$conf.int[2], 2),
       true_r = stats::cor(first, second))
}

paper_pearson <- function() {
  result <- stats::cor.test(datasets::mtcars$mpg, datasets::mtcars$wt)
  list(n = "32", r = fixed(result$estimate, 3), df = as.character(result$parameter),
       p = "< .001",
       ci_lower = fixed(result$conf.int[1], 3), ci_upper = fixed(result$conf.int[2], 3))
}

run_check <- function(handler, paper, drop = character()) {
  paper <- paper[setdiff(setdiff(names(paper), "true_r"), drop)]
  do.call(check_reported, c(list(handler = handler), paper))
}

status_of <- function(checked, quantity) {
  checked$numbers$status[checked$numbers$quantity == quantity]
}
