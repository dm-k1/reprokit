.onLoad <- function(libname, pkgname) {
  register_builtin_handlers()
}

# One line per handler. Examples are functions, so loading the package never
# touches data or the random number generator.
register_builtin_handlers <- function() {
  register_t_independent()
  register_t_one_sample()
  register_t_paired()
  register_cor_pearson()
  register_anova_oneway()
  register_lm()
}

# A dataset from the datasets package, without adding it to Imports
# (Imports stay stats/utils/methods, see test-dependencies.R).
base_data <- function(name) {
  env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "datasets", envir = env)
  env[[name]]
}
