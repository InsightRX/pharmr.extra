#' Resolve and validate a requested number of worker processes
#'
#' @param n_cores requested number of cores.
#'
#' @returns a positive integer, capped at the number of cores detected on the
#' machine.
#' @noRd
resolve_n_cores <- function(n_cores) {
  n <- suppressWarnings(as.numeric(n_cores))
  ## Bound above by the integer range as well: `as.integer()` returns NA for
  ## `Inf` or anything past `.Machine$integer.max`, which would then blow up in
  ## the `n == 1L` comparison below instead of reporting the real problem.
  if(length(n) != 1 || is.na(n) || n < 1 || n != round(n) ||
     n > .Machine$integer.max) {
    cli::cli_abort("`n_cores` must be a positive integer (<= {(.Machine$integer.max)}).")
  }
  n <- as.integer(n)
  if(n == 1L) return(n)
  avail <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if(!is.na(avail) && n > avail) {
    cli::cli_warn(c(
      "`n_cores` ({n}) exceeds the {avail} core{?s} detected; using {avail}."
    ))
    n <- as.integer(avail)
  }
  n
}

#' Apply a function over elements, optionally on parallel worker processes
#'
#' Sequential when `n_cores == 1` (or there is nothing to gain), otherwise
#' runs `FUN` on a PSOCK cluster with load balancing. PSOCK rather than
#' `parallel::mclapply()` (fork): the calling process holds an embedded Python
#' interpreter (reticulate) and rxode2 spawns OpenMP threads, neither of which
#' is safe to fork. Workers are fresh R processes that must therefore never
#' touch Python.
#'
#' Results are returned in the order of `X` regardless of how work was
#' scheduled, so callers can rely on positional indexing.
#'
#' Starting the workers is best-effort: bringing a cluster up is a separate
#' failure mode from the work itself, and it is one this package has seen fail
#' intermittently (#134 — a worker's `loadNamespace()` failing inside rxode2's
#' `.onLoad`). Because it happens before `FUN` is ever called it is outside
#' whatever error handling the caller wrapped `FUN` in, so left unguarded it
#' takes down the entire run rather than costing one item. Rather than let a
#' transient startup problem throw away the work, a failed attempt is retried
#' once and then falls back to running everything sequentially in this process:
#' slower, but the same results.
#'
#' @param X vector/list to iterate over.
#' @param FUN function applied to each element. Must be self-contained enough
#' to survive serialisation to a worker (no Python/reticulate objects, no
#' open connections). Must also be safe to call again from scratch: a parallel
#' attempt that fails part-way is redone sequentially.
#' @param n_cores number of worker processes; `1` runs sequentially in-process.
#'
#' @returns list of results, one per element of `X`, in the order of `X`.
#' @noRd
parallel_lapply <- function(X, FUN, n_cores = 1L) {
  if(n_cores <= 1L || length(X) < 2L) {
    return(lapply(X, FUN))
  }
  n_workers <- min(n_cores, length(X))

  attempt <- function() {
    cl <- parallel::makePSOCKcluster(n_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    init <- worker_init_args()
    parallel::clusterCall(cl, worker_init_fn(), init$path, init$dev,
                          worker_threads(n_workers))
    parallel::parLapplyLB(cl, X, FUN)
  }

  for(try_n in 1:2) {
    res <- tryCatch(attempt(), error = function(e) e)
    if(!inherits(res, "condition")) return(res)
    if(try_n == 1L) next
    cli::cli_warn(c(
      "Could not run on {n_workers} worker process{?es}; running sequentially.",
      x = conditionMessage(res),
      i = "The results are the same either way, only slower."
    ))
  }
  lapply(X, FUN)
}

#' The function each worker runs before any work is handed to it
#'
#' A factory returning a `baseenv()`-parented closure, on purpose: a closure
#' carrying this package's namespace makes the worker load the *installed*
#' pharmr.extra just to unserialise it, which fails when the package was never
#' installed and silently runs stale code when it was. This one loads nothing
#' implicitly, so it gets to decide which copy the worker uses.
#'
#' @returns a function of `(path, dev, threads)`.
#' @noRd
worker_init_fn <- function() {
  init_fn <- function(path, dev, threads) {
    if(isTRUE(dev)) {
      pkgload::load_all(path, quiet = TRUE, helpers = FALSE,
                        attach_testthat = FALSE)
    } else {
      loadNamespace("pharmr.extra")
    }
    ## Each worker is a fresh R process in which rxode2 sizes its OpenMP thread
    ## pool to every core it detects, so `n_workers` workers would each spawn a
    ## machine's worth of solver threads and spend the run fighting each other
    ## for cores. Give every worker an equal, non-overlapping share instead.
    if(requireNamespace("rxode2", quietly = TRUE)) {
      try(rxode2::setRxThreads(threads), silent = TRUE)
    }
    invisible(NULL)
  }
  environment(init_fn) <- baseenv()
  init_fn
}

#' Solver threads to allow each worker process
#'
#' Splits the machine's cores evenly over the workers, so the total number of
#' rxode2/OpenMP threads stays around the core count rather than
#' `n_workers` times it. At least `1` per worker.
#'
#' @param n_workers number of worker processes about to be started.
#'
#' @returns a positive integer.
#' @noRd
worker_threads <- function(n_workers) {
  avail <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if(is.na(avail)) return(1L)
  max(1L, as.integer(avail %/% n_workers))
}

#' Arguments describing how workers should load this package
#'
#' Under `pkgload::load_all()` (i.e. during development and `devtools::test()`)
#' the package is not installed in a form a worker can `loadNamespace()`, or
#' worse, an older *installed* copy would be picked up silently. Detect that
#' case in the parent and hand the workers the source path instead.
#'
#' @returns list with `path` (package source or install path) and `dev` (is
#' this a `load_all()` session).
#' @noRd
worker_init_args <- function() {
  dev <- FALSE
  if(requireNamespace("pkgload", quietly = TRUE)) {
    dev <- isTRUE(pkgload::is_dev_package("pharmr.extra"))
  }
  list(path = getNamespaceInfo("pharmr.extra", "path"), dev = dev)
}

#' Run one replicate, capturing its warnings and turning errors into a drop
#'
#' Worker processes have no console, so warnings and messages raised there
#' would be lost; capture them and return them for the parent to re-emit. An
#' error is returned as a value rather than propagated so a single bad
#' replicate does not kill the whole run.
#'
#' Used for the sequential path too, so both paths behave identically.
#'
#' @param index replicate index (1-based).
#' @param fn function of no arguments performing the replicate.
#'
#' @returns list with `index`, `result` (a data.frame, or an error condition)
#' and `warnings` (list of warning conditions).
#' @noRd
run_captured <- function(index, fn) {
  warns <- list()
  res <- withCallingHandlers(
    tryCatch(fn(), error = function(e) e),
    warning = function(w) {
      ## The whole condition, not just its message: re-emitting it in the
      ## parent should keep any custom class an upstream `tryCatch()` matches on.
      warns[[length(warns) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(index = index, result = res, warnings = warns)
}

#' Re-emit warnings a replicate raised, labelled with the replicate index
#'
#' @param index replicate index (1-based).
#' @param warnings list of captured warning conditions.
#' @param label what the index counts, used as the warning prefix. The NWPRI
#' uncertainty engine splits the draws over chunks rather than replicates, so
#' its warnings should not claim to be about replicate `k`.
#'
#' @returns `NULL`, invisibly.
#' @noRd
emit_replicate_warnings <- function(index, warnings,
                                    label = "Uncertainty replicate") {
  for(w in warnings) {
    msg <- conditionMessage(w)
    ## Preserve any class the original warning carried beyond the base ones, so
    ## callers handling a specific condition class still see it after the trip
    ## through a worker.
    extra <- setdiff(
      class(w),
      c("simpleWarning", "rlang_warning", "warning", "condition")
    )
    cli::cli_warn(
      "{label} {index}: {msg}",
      class = if(length(extra) > 0) extra else NULL
    )
  }
  invisible(NULL)
}

#' Run a `cli` progress-bar call without letting it fail the caller
#'
#' `cli` builds progress bars on top of its status-bar stack, so an unbalanced
#' `cli::cli_process_done()` in any code that runs while a bar is open pops the
#' bar's own entry off that stack. `cli` then indexes the emptied stack and
#' throws `subscript out of bounds` on the bar's next update or on its
#' teardown — after the real work has already finished. A progress bar is
#' cosmetic, so swallow its errors rather than lose a completed run to one.
#' See issue #137.
#'
#' @param expr a `cli` progress-bar call, evaluated lazily inside the handler.
#'
#' @returns the value of `expr`, or `NULL` (invisibly) if it errored.
#' @noRd
progress_try <- function(expr) {
  tryCatch(expr, error = function(e) invisible(NULL))
}
