# Reproducible parallel map for the validation scripts (sourced, not run).
# The result of par_run() does not depend on the number of cores: every task gets its own
# L'Ecuyer-CMRG stream (parallel::nextRNGStream from one seed, in task order), set inside
# the worker before the task runs, and mclapply() only decides which core runs which task.
# Tasks must therefore be defined (and ordered) independently of the core count: a chunk of
# 15 designs of a group is a task, not "the share of one core". Cores: the environment
# variable REPROKIT_VALIDATION_CORES, else all but one (at most 8). With 1 core the tasks run
# in the calling process, one after the other, with the same streams and the same results.
par_cores <- function() {
  env <- suppressWarnings(as.integer(Sys.getenv("REPROKIT_VALIDATION_CORES", "")))
  if (!is.na(env) && env >= 1L) return(env)
  max(1L, min(8L, parallel::detectCores() - 1L))
}

par_streams <- function(n, seed) {
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  s <- get(".Random.seed", globalenv())
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- s
    s <- parallel::nextRNGStream(s)
  }
  out
}

# Runs fn(task) for each element of `tasks`; returns the list of results in task order.
par_run <- function(tasks, fn, seed, cores = par_cores()) {
  streams <- par_streams(length(tasks), seed)
  out <- parallel::mclapply(seq_along(tasks), function(i) {
    assign(".Random.seed", streams[[i]], globalenv())
    fn(tasks[[i]])
  }, mc.cores = cores, mc.preschedule = FALSE, mc.set.seed = FALSE)
  failed <- vapply(out, inherits, logical(1), what = "try-error")
  if (any(failed)) stop("task ", which(failed)[1L], " failed: ", as.character(out[[which(failed)[1L]]]))
  out
}
