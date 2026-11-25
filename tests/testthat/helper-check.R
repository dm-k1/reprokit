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

run_check <- function(test, paper, drop = character()) {
  paper <- paper[setdiff(setdiff(names(paper), "true_r"), drop)]
  do.call(check_reported, c(list(test = test), paper))
}

status_of <- function(checked, quantity) {
  checked$numbers$status[checked$numbers$quantity == quantity]
}

# A 2 x 3 between-subjects design with unequal cell sizes, built so the three SS types give
# different F and p for the main effects (seed found by search; fixed here).
factorial_unbalanced <- function() {
  set.seed(71)
  n <- c(8, 14, 20, 6, 12, 9)
  d <- data.frame(a = factor(rep(rep(c("a1", "a2"), times = 3), n)),
                  b = factor(rep(rep(1:3, each = 2), n)))
  d$y <- rnorm(nrow(d), 10 + 0.8 * (d$a == "a2") + 0.6 * as.integer(d$b), 2)
  d
}

# What a paper reports for one effect: cell matrices at 2 decimals, F at 2, p at 3, partial
# eta squared at 2, at the SS type the paper used.
paper_factorial <- function(d, effect, type = "III", digits = 2L) {
  dims <- c(nlevels(d$a), nlevels(d$b))
  r <- anova_factorial_reported(y ~ a * b, d, "factorial", ss_type = type)
  cells <- function(f) matrix(f, dims[1L], dims[2L])
  list(n = cells(as.character(as.vector(table(d$a, d$b)))),
       m = cells(fixed(tapply(d$y, list(d$a, d$b), mean), digits)),
       sd = cells(fixed(tapply(d$y, list(d$a, d$b), sd), digits)),
       f = fixed(r[[paste0("f_", effect)]], 2),
       df1 = as.character(r[[paste0("df1_", effect)]]), df2 = as.character(r[["df2"]]),
       p = fixed(r[[paste0("p_", effect)]], 3),
       eta2 = fixed(r[[paste0("eta2_", effect)]], 2),
       effect = effect, dims = dims)
}

check_factorial <- function(...) check_reported(test = "factorial", ...)
check_paired <- function(...) check_reported(test = "paired", ...)
check_pearson <- function(...) check_reported(test = "pearson", ...)
# Student and Welch together: the range engine without fixing the test, so the variants
# kind lists both (check_reported(test = "student") / ("welch") would fix it).
check_independent <- function(...) check_handler("t_independent", ...)
