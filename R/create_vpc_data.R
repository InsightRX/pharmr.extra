#' Run a simulation based on supplied parameters estimates,
#' and combine into proper format for VPC
#'
#' This rewrite of `create_vpc_data()` removes the Pharmpy/pharmr dependency
#' from every step that runs before `run_nlme()`. The only Pharmpy touch is
#' reading `model$code` as a string when a Pharmpy model is supplied. All
#' subsequent manipulation of the NONMEM code (parameter updates, record
#' removal/insertion, $EST -> $SIM conversion) and the input dataset
#' (sanitisation of string-typed columns) is done in pure R, so the
#' Python/R <-> reticulate serialisation cannot reject a value that NONMEM
#' itself would accept. `run_nlme()` is still used to actually invoke nmfe,
#' and post-processing of `obs`/`sim` is identical to the original.
#'
#' @param fit fit object from `pharmr::run_modelfit()`. Optional; alternative
#'   to `model` + `parameters`.
#' @param model Either a Pharmpy model object (its `$code` is read), a
#'   character vector of NONMEM model code, or a path to a `.mod` file.
#' @param data Path to the NONMEM-ready CSV. Optional; if missing, the
#'   function reads `$DATA` from the model code.
#' @param parameters Named list of parameter inits, e.g.
#'   `list(THETA_1 = 0.23, OMEGA_1_1 = 0.097, SIGMA_1_1 = 1)`. Optional.
#' @param keep_columns character vector of column names in the original
#'   dataset to keep in the output.
#' @param n number of simulation iterations.
#' @param id run id used as folder name. Defaults to a random name.
#' @param verbose verbose output?
#' @param use_pharmpy retained for backward compatibility; controls whether
#'   `PRED`/`TAD` are transferred from obs to sim during post-processing.
#' @param fix_input_heuristic If TRUE (default), detect the common
#'   `pharmr::set_dataset()` side-effect that rewrites `$INPUT` to the CSV
#'   headers, and rebind `TIME` -> `TAFD` and (for log-transform-both-sides
#'   models) `DV` -> `LNDV`. Set to FALSE to leave `$INPUT` untouched.
#'
#' @returns list with `obs` and `sim` data frames.
#'
#' @export
create_vpc_data <- function(
  fit = NULL,
  model = NULL,
  data = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE,
  id = NULL,
  use_pharmpy = TRUE,
  fix_input_heuristic = TRUE
) {

  ## ---- Resolve model & dispatch nlmixr branch via the original code path ----
  caller_supplied_model <- !is.null(model)
  if(is.null(model)) {
    model <- attr(fit, "final_model") %||% attr(fit, "model")
    if(is.null(model)) {
      cli::cli_abort("Either a `fit` object with a model attached, or a `model` argument is required.")
    }
  }
  if(inherits(model, "pharmpy.model.model.Model") &&
     identical(get_tool_from_model(model), "nlmixr")) {
    return(create_vpc_data_nlmixr(
      fit = fit,
      model = if(caller_supplied_model) model else NULL,
      parameters = parameters,
      keep_columns = keep_columns,
      n = n,
      verbose = verbose
    ))
  }

  ## ---- Get model code as a plain string (only Pharmpy touch) ----
  model_code <- get_model_code_pure(model)

  ## ---- Resolve parameters (from fit if not supplied) ----
  if(is.null(parameters) && !is.null(fit) && !is.null(fit$parameter_estimates)) {
    if(verbose) message("Using parameters from `fit` object")
    parameters <- as.list(fit$parameter_estimates)
  }

  ## ---- Resolve data path ----
  if(is.null(data)) {
    data <- extract_data_path_from_code(model_code)
  }
  if(!is.character(data) || !file.exists(data)) {
    cli::cli_abort("`data` is required and must be a path to a CSV file (got: {data}).")
  }

  ## ---- Sanitise dataset for NONMEM ----
  ##
  ## - Trailing columns not declared in $INPUT are dropped entirely (the
  ##   `*_UNIT` / `UNIQUE_ID` strings in many real-world datasets).
  ## - Columns declared in $INPUT that are non-numeric are coerced to numeric
  ##   (values that don't parse become 0). These positions are typically
  ##   DROP'd in $INPUT anyway, so 0 has no semantic effect.
  ## - NAs (incl. the NONMEM "." convention) become 0.
  ##
  ## The result is a temp CSV with the same column ORDER as the input, so the
  ## $INPUT positional binding is preserved.
  clean_csv <- sanitize_csv_for_nonmem(data, model_code, verbose = verbose)

  ## ---- Optional: undo the $INPUT rewrite that pharmr::set_dataset does ----
  if(isTRUE(fix_input_heuristic)) {
    model_code <- fix_input_after_set_dataset(model_code, clean_csv, verbose = verbose)
  }

  ## ---- Apply parameter values to $THETA/$OMEGA/$SIGMA inits ----
  if(!is.null(parameters)) {
    if(verbose) message("Applying supplied parameter values to model code")
    model_code <- apply_parameters_to_code(model_code, parameters)
  }

  ## ---- Strip $COVARIANCE and existing $TABLE, then add the VPC sdtab ----
  vpc_vars <- c("ID", "TIME", "PRED", "DV", "EVID", "MDV", keep_columns)
  vpc_vars <- unique(vpc_vars)
  model_code <- remove_record(model_code, "COVARIANCE")
  model_code <- remove_record(model_code, "COV")  ## abbreviated form
  model_code <- remove_record(model_code, "TABLE")
  model_code <- add_table_record(model_code, vpc_vars, file = "sdtab")

  ## Point the model's $DATA at our sanitised CSV so Pharmpy (inside
  ## run_nlme) reads numeric data only.
  model_code <- change_nonmem_dataset(model_code, clean_csv)

  ## ---- Build eval and sim variants ----
  eval_code <- set_estimation_maxeval_zero(model_code)
  sim_code  <- convert_estimation_to_simulation(model_code, n = n)

  ## ---- Run via run_nlme ----
  tmp_path <- file.path(
    tempdir(),
    paste0("simulation_", irxutils::random_string(5))
  )
  dir.create(tmp_path, showWarnings = FALSE, recursive = TRUE)
  if(is.null(id)) id <- "tmp"

  if(verbose) cli::cli_alert_info("Running input model evaluation for VPC")
  eval_mod <- write_temp_mod(eval_code)
  eval_res <- run_nlme(
    model = eval_mod,
    data = clean_csv,
    path = tmp_path,
    force = TRUE,
    id = id,
    save_final = FALSE,
    verbose = verbose
  )
  obs <- attr(eval_res, "tables")[[1]]
  if(is.null(obs)) {
    cli::cli_abort("Evaluation step produced no tables. Check NONMEM output in {tmp_path}.")
  }

  if(verbose) cli::cli_alert_info("Running {n} simulations for VPC")
  sim_mod <- write_temp_mod(sim_code)
  sim_res <- run_nlme(
    model = sim_mod,
    data = clean_csv,
    path = tmp_path,
    force = TRUE,
    id = id,
    save_final = FALSE,
    verbose = verbose
  )
  sim <- attr(sim_res, "tables")[[1]]
  if(is.null(sim)) {
    cli::cli_abort("Simulation step produced no tables. Check NONMEM output in {tmp_path}.")
  }

  ## ---- Post-process: unchanged from the original create_vpc_data ----
  if(verbose) cli::cli_alert_info("Preparing simulated output data for plotting")

  if(is.null(obs$TAD)) {
    obs <- obs |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(last_dose_time = dplyr::if_else(.data$EVID == 1, .data$TIME, NA)) |>
      tidyr::fill("last_dose_time", .direction = "downup") |>
      dplyr::mutate(TAD = .data$TIME - .data$last_dose_time) |>
      dplyr::select(-"last_dose_time")
  }

  len_obs <- nrow(obs)
  len_sim <- nrow(sim)
  if((len_sim %% len_obs) != 0) {
    cli::cli_abort("The simulated dataset length is not a multiple of the length of the original dataset. Please check model and simulation settings.")
  }
  if(use_pharmpy) {
    transfer <- c("ID", "TIME", "PRED", "TAD")
    for(col in transfer) {
      if(!is.null(obs[[col]])) {
        sim[[col]] <- obs[[col]]
      } else {
        cli::cli_alert_warning("Column {col} not found in original dataset.")
      }
    }
  }
  for(col in keep_columns) {
    if(!is.null(obs[[col]])) {
      sim[[col]] <- obs[[col]]
    }
  }

  list(obs = obs, sim = sim)
}

## =====================================================================
## Pure-R helpers (no Pharmpy/pharmr)
## =====================================================================

#' Read NONMEM code from a Pharmpy model, file path, or string.
#'
#' Reading `model$code` is the only Pharmpy attribute access in the new
#' `create_vpc_data()`. It's a plain string, so no dataset serialisation.
#'
#' @noRd
get_model_code_pure <- function(model) {
  if(inherits(model, "pharmpy.model.model.Model")) {
    return(model$code)
  }
  if(is.character(model)) {
    if(length(model) == 1 && file.exists(model)) {
      return(paste(readLines(model, warn = FALSE), collapse = "\n"))
    }
    return(paste(model, collapse = "\n"))
  }
  cli::cli_abort("Don't know how to extract NONMEM code from a {class(model)[1]}.")
}

#' Pull the dataset path out of $DATA, or NA if not resolvable.
#' @noRd
extract_data_path_from_code <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  data_line <- grep("^\\s*\\$DATA\\b", lines, value = TRUE, ignore.case = TRUE)
  if(!length(data_line)) return(NA_character_)
  body <- sub("^\\s*\\$DATA\\s*", "", data_line[1], ignore.case = TRUE)
  ## first token before any IGNORE=/ACCEPT=/etc.
  tok <- strsplit(trimws(body), "\\s+")[[1]][1]
  tok
}

#' Read a NONMEM CSV in pure R, drop trailing string columns past $INPUT,
#' numerify any remaining string columns and NAs, write to a temp file.
#'
#' @noRd
sanitize_csv_for_nonmem <- function(csv_path, model_code, verbose = FALSE) {
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                        na.strings = c("", "NA", "."))
  n_input <- count_input_columns(model_code)
  if(is.na(n_input) || n_input < 1) n_input <- ncol(df)

  ## Drop any string columns past the $INPUT range entirely.
  is_char <- vapply(df, function(x) !is.numeric(x), logical(1))
  trailing <- which(is_char & seq_along(df) > n_input)
  if(length(trailing)) {
    if(verbose) {
      cli::cli_alert_info("Dropping trailing non-numeric columns past $INPUT: {names(df)[trailing]}")
    }
    df <- df[, -trailing, drop = FALSE]
    is_char <- vapply(df, function(x) !is.numeric(x), logical(1))
  }

  ## Numerify string columns within $INPUT (DROP'd positions usually).
  for(i in which(is_char)) {
    x <- suppressWarnings(as.numeric(df[[i]]))
    x[is.na(x)] <- 0
    df[[i]] <- x
  }
  ## Fill any remaining numeric NAs with 0.
  for(i in seq_along(df)) {
    if(any(is.na(df[[i]]))) df[[i]][is.na(df[[i]])] <- 0
  }

  out <- tempfile(pattern = "vpc_data_", fileext = ".csv")
  utils::write.csv(df, out, row.names = FALSE, quote = FALSE)
  out
}

#' Count the number of columns declared in $INPUT (handles multi-line blocks).
#' @noRd
count_input_columns <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  in_block <- FALSE
  tokens <- character(0)
  for(line in lines) {
    body <- sub(";.*$", "", line)
    if(grepl("^\\s*\\$INPUT\\b", body, ignore.case = TRUE)) {
      in_block <- TRUE
      body <- sub("^\\s*\\$INPUT\\s*", "", body, ignore.case = TRUE)
    } else if(in_block && grepl("^\\s*\\$[A-Za-z]", body)) {
      break
    }
    if(in_block) {
      toks <- strsplit(trimws(body), "\\s+")[[1]]
      tokens <- c(tokens, toks[nzchar(toks)])
    }
  }
  length(tokens)
}

#' Apply heuristics to undo the $INPUT rewrite done by `pharmr::set_dataset()`.
#'
#' set_dataset() replaces the model's positional $INPUT with the dataframe
#' headers. For datasets where the CSV header doesn't already match what the
#' model code references (e.g. CSV has `TAFD` while the model uses `TIME` for
#' the PK time variable, or CSV stores log-conc in `LNDV` while the model
#' uses `DV` and does `IPRE = LOG(F)`), the rewritten $INPUT misbinds the
#' columns and the model breaks silently.
#'
#' We detect two patterns and patch:
#'   1. Both `TIME` and `TAFD` named in $INPUT -> swap (TIME -> CLOCK=DROP,
#'      TAFD -> TIME). The "TIME" column in such datasets is usually a clock
#'      string anyway, and the numeric time-since-first-dose is in `TAFD`.
#'   2. Both `DV` and `LNDV` named in $INPUT and the model is LTBS (uses
#'      `LOG(F)`) -> swap (DV -> CONC=DROP, LNDV -> DV) so the model's DV
#'      reference picks up the log-transformed values.
#'
#' @noRd
fix_input_after_set_dataset <- function(code, csv_path, verbose = FALSE) {
  tokens <- get_input_tokens(code)
  if(!length(tokens)) return(code)
  names_no_drop <- sub("=.*$", "", tokens)

  rename <- c()
  if("TIME" %in% names_no_drop && "TAFD" %in% names_no_drop) {
    rename <- c(rename, "TIME" = "CLOCK=DROP", "TAFD" = "TIME")
  }
  is_ltbs <- grepl("=\\s*LOG\\s*\\(", code, ignore.case = TRUE) &&
             grepl("\\bIPRE", code, ignore.case = TRUE)
  if(is_ltbs && "DV" %in% names_no_drop && "LNDV" %in% names_no_drop) {
    rename <- c(rename, "DV" = "CONC=DROP", "LNDV" = "DV")
  }

  if(!length(rename)) return(code)
  if(verbose) {
    cli::cli_alert_info("Patching $INPUT to undo set_dataset() column rename: {names(rename)} -> {unname(rename)}")
  }

  ## Apply renames in $INPUT, careful to only touch the $INPUT block.
  rewrite_input_tokens(code, function(toks) {
    nms <- sub("=.*$", "", toks)
    for(i in seq_along(toks)) {
      if(nms[i] %in% names(rename)) {
        toks[i] <- unname(rename[nms[i]])
      }
    }
    toks
  })
}

#' Pull $INPUT tokens out of code as a character vector (handles multi-line).
#' @noRd
get_input_tokens <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  in_block <- FALSE
  tokens <- character(0)
  for(line in lines) {
    body <- sub(";.*$", "", line)
    if(grepl("^\\s*\\$INPUT\\b", body, ignore.case = TRUE)) {
      in_block <- TRUE
      body <- sub("^\\s*\\$INPUT\\s*", "", body, ignore.case = TRUE)
    } else if(in_block && grepl("^\\s*\\$[A-Za-z]", body)) {
      break
    }
    if(in_block) {
      toks <- strsplit(trimws(body), "\\s+")[[1]]
      tokens <- c(tokens, toks[nzchar(toks)])
    }
  }
  tokens
}

#' Replace the $INPUT block by passing its tokens through a transform.
#' Preserves rest of the code verbatim.
#' @noRd
rewrite_input_tokens <- function(code, transform) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  in_block <- FALSE
  start <- end <- NA_integer_
  for(i in seq_along(lines)) {
    body <- sub(";.*$", "", lines[i])
    if(grepl("^\\s*\\$INPUT\\b", body, ignore.case = TRUE)) {
      in_block <- TRUE
      start <- i
    } else if(in_block && grepl("^\\s*\\$[A-Za-z]", body)) {
      end <- i - 1
      break
    }
  }
  if(is.na(start)) return(code)
  if(is.na(end)) end <- length(lines)
  tokens <- get_input_tokens(code)
  new_tokens <- transform(tokens)
  ## Re-emit as one $INPUT line per ~10 tokens to mimic NONMEM style.
  chunks <- split(new_tokens, ceiling(seq_along(new_tokens) / 10))
  new_block <- c(
    paste("$INPUT", paste(chunks[[1]], collapse = " ")),
    vapply(chunks[-1], function(c) paste("       ", paste(c, collapse = " ")),
           character(1))
  )
  c(lines[seq_len(start - 1)], new_block, lines[(end + 1):length(lines)]) |>
    paste(collapse = "\n")
}

#' Remove all NONMEM records of a given name (e.g. $TABLE, $COVARIANCE).
#' Each "record" runs from `$NAME ...` up to (but not including) the next
#' `$<KEYWORD>` or end of file.
#' @noRd
remove_record <- function(code, record_name) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  keep <- rep(TRUE, length(lines))
  in_record <- FALSE
  pat <- paste0("^\\s*\\$", record_name, "\\b")
  for(i in seq_along(lines)) {
    body <- sub(";.*$", "", lines[i])
    starts <- grepl(pat, body, ignore.case = TRUE)
    starts_other <- grepl("^\\s*\\$[A-Za-z]", body) && !starts
    if(starts) {
      in_record <- TRUE
      keep[i] <- FALSE
    } else if(in_record) {
      if(starts_other) {
        in_record <- FALSE
        ## fall through, keep this line
      } else {
        keep[i] <- FALSE
      }
    }
  }
  paste(lines[keep], collapse = "\n")
}

#' Add a $TABLE record at the end of the model code.
#' @noRd
add_table_record <- function(code, variables, file = "sdtab") {
  table_line <- paste(
    "$TABLE",
    paste(variables, collapse = " "),
    paste0("NOAPPEND NOPRINT FILE=", file)
  )
  paste0(sub("\\s+$", "", code), "\n", table_line, "\n")
}

#' Force MAXEVAL=0 on any existing $ESTIMATION record (or add one).
#' @noRd
set_estimation_maxeval_zero <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  pat <- "^\\s*\\$EST(IMATION)?\\b"
  est_idx <- grep(pat, lines, ignore.case = TRUE)
  if(!length(est_idx)) {
    return(paste0(sub("\\s+$", "", code), "\n$ESTIMATION METHOD=COND INTER MAXEVAL=0\n"))
  }
  for(i in est_idx) {
    body <- lines[i]
    if(grepl("\\bMAXEVAL\\s*=\\s*[0-9]+", body, ignore.case = TRUE)) {
      body <- sub("\\bMAXEVAL\\s*=\\s*[0-9]+", "MAXEVAL=0", body, ignore.case = TRUE)
    } else {
      body <- sub("(\\$EST(IMATION)?\\b)", "\\1 MAXEVAL=0", body, ignore.case = TRUE)
    }
    lines[i] <- body
  }
  paste(lines, collapse = "\n")
}

#' Replace $ESTIMATION with $SIMULATION ONLYSIM NSUB=n.
#' @noRd
convert_estimation_to_simulation <- function(code, n) {
  code <- remove_record(code, "ESTIMATION")
  code <- remove_record(code, "EST")
  seed <- 20260528
  sim_line <- sprintf("$SIMULATION (%d) ONLYSIM NSUB=%d", seed, n)
  paste0(sub("\\s+$", "", code), "\n", sim_line, "\n")
}

#' Write NONMEM model code to a temp .mod file.
#' @noRd
write_temp_mod <- function(code) {
  p <- tempfile(pattern = "vpc_mod_", fileext = ".mod")
  writeLines(code, p)
  p
}

#' Update parameter init values in $THETA / $OMEGA / $SIGMA blocks.
#'
#' `parameters` is the named list returned by `model$parameters$inits`
#' (e.g. `THETA_1 = 0.226989`, `CLAGE1 = -0.001`, `OMEGA_1_1 = 0.097`,
#' `SIGMA_1_1 = 1`). The mapping back to positions is positional:
#' the first non-OMEGA/non-SIGMA value is THETA #1, the second is #2, etc.
#' OMEGA/SIGMA values are matched in declaration order against the values
#' that appear in the respective blocks.
#'
#' This is a pragmatic, regex-driven implementation and may not handle every
#' NONMEM syntactic corner (e.g. `2 x 0.1` shorthand). For the common forms
#' `(low, init, up)`, `(low, init)`, `(init)`, and bare `init` it works.
#' @noRd
apply_parameters_to_code <- function(code, parameters) {
  if(!length(parameters)) return(code)

  is_omega <- grepl("^OMEGA(_|$)", names(parameters))
  is_sigma <- grepl("^SIGMA(_|$)", names(parameters))
  theta_vals <- unlist(parameters[!(is_omega | is_sigma)], use.names = FALSE)
  omega_vals <- unlist(parameters[is_omega], use.names = FALSE)
  sigma_vals <- unlist(parameters[is_sigma], use.names = FALSE)

  code <- rewrite_block_values(code, "THETA", theta_vals, init_in_paren = TRUE)
  code <- rewrite_block_values(code, "OMEGA", omega_vals, init_in_paren = FALSE)
  code <- rewrite_block_values(code, "SIGMA", sigma_vals, init_in_paren = FALSE)
  code
}

#' Walk a $THETA/$OMEGA/$SIGMA block and replace each "init" value, in
#' declaration order, with the next value from `values`. For $THETA, the
#' init is the 2nd element of a `(low, init, up)` tuple (or 1st of `(init)`,
#' or the bare value if no parens). For $OMEGA/$SIGMA, every numeric token is
#' an init.
#' @noRd
rewrite_block_values <- function(code, block_name, values, init_in_paren) {
  if(!length(values)) return(code)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  in_block <- FALSE
  q <- as.list(values)  ## queue
  pat_start <- paste0("^\\s*\\$", block_name, "\\b")

  for(i in seq_along(lines)) {
    body0 <- lines[i]
    comment_pos <- regexpr(";", body0)
    if(comment_pos > 0) {
      body <- substring(body0, 1, comment_pos - 1)
      comment <- substring(body0, comment_pos)
    } else {
      body <- body0; comment <- ""
    }

    starts <- grepl(pat_start, body, ignore.case = TRUE)
    other_record <- !starts && grepl("^\\s*\\$[A-Za-z]", body)

    if(starts) {
      in_block <- TRUE
      ## Strip keyword and anything that's a NON-init token (BLOCK(n), DIAG)
      ## before scanning for inits on the same line.
      head_match <- regmatches(body, regexpr("^\\s*\\$[A-Za-z]+\\s*", body))
      tail <- substring(body, nchar(head_match) + 1)
      ## Drop e.g. BLOCK(2), DIAGONAL(n) -- not init values
      tail_stripped <- gsub("\\bBLOCK\\s*\\([^)]*\\)|\\bDIAG(ONAL)?\\s*\\([^)]*\\)",
                            "", tail, ignore.case = TRUE)
      if(grepl("[0-9]", tail_stripped)) {
        new_tail <- replace_inits_in_text(tail_stripped, q, init_in_paren)
        tail_done <- new_tail$text
        q <- new_tail$queue
        ## Re-insert: just splice the new tail back; we lose any BLOCK token
        ## that was on this line, but BLOCK declarations are usually on
        ## their own line in practice.
        body <- paste0(head_match, tail_done)
      }
      lines[i] <- paste0(body, comment)
      next
    }

    if(in_block && other_record) {
      in_block <- FALSE
      next
    }

    if(in_block && grepl("[0-9]", body)) {
      body_stripped <- gsub("\\bBLOCK\\s*\\([^)]*\\)|\\bDIAG(ONAL)?\\s*\\([^)]*\\)",
                            "", body, ignore.case = TRUE)
      new_body <- replace_inits_in_text(body_stripped, q, init_in_paren)
      lines[i] <- paste0(new_body$text, comment)
      q <- new_body$queue
    } else {
      lines[i] <- body0
    }

    if(!length(q)) {
      ## queue exhausted; stop trying to replace
      ## but leave the rest of the block unchanged
    }
  }
  paste(lines, collapse = "\n")
}

#' Within a block-of-text fragment, replace each init value (left-to-right)
#' with the next value from `queue`. Returns the modified text and the
#' shortened queue.
#' @noRd
replace_inits_in_text <- function(text, queue, init_in_paren) {
  if(!length(queue) || !nzchar(text)) return(list(text = text, queue = queue))

  fmt <- function(v) {
    if(is.na(v) || !is.finite(v)) return("0")
    format(v, scientific = FALSE, trim = TRUE)
  }

  if(init_in_paren) {
    ## Replace each paren expression's "init" position; bare numbers replaced
    ## individually (rare for $THETA but allowed).
    out <- ""
    pos <- 1
    nchars <- nchar(text)
    while(pos <= nchars && length(queue)) {
      remaining <- substring(text, pos, nchars)
      paren_m <- regexpr("\\([^)]*\\)", remaining)
      bare_m <- regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", remaining)
      if(paren_m == -1 && bare_m == -1) break

      take_paren <- paren_m != -1 && (bare_m == -1 || paren_m <= bare_m)
      if(take_paren) {
        rel <- as.integer(paren_m)
        len <- attr(paren_m, "match.length")
        ## copy text before paren
        out <- paste0(out, substring(remaining, 1, rel - 1))
        paren_text <- substring(remaining, rel, rel + len - 1)
        inner <- substring(paren_text, 2, nchar(paren_text) - 1)
        parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
        parts <- trimws(parts)
        ## init index: 1 if singleton, else 2
        init_i <- if(length(parts) == 1) 1 else 2
        if(init_i <= length(parts)) {
          new_val <- fmt(queue[[1]]); queue <- queue[-1]
          parts[init_i] <- sub(
            "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
            new_val, parts[init_i], perl = TRUE)
        }
        out <- paste0(out, "(", paste(parts, collapse = ","), ")")
        pos <- pos + rel + len - 1
      } else {
        rel <- as.integer(bare_m)
        len <- attr(bare_m, "match.length")
        out <- paste0(out, substring(remaining, 1, rel - 1))
        new_val <- fmt(queue[[1]]); queue <- queue[-1]
        out <- paste0(out, new_val)
        pos <- pos + rel + len - 1
      }
    }
    ## append the rest of text untouched
    out <- paste0(out, substring(text, pos, nchars))
    return(list(text = out, queue = queue))
  } else {
    ## $OMEGA/$SIGMA: every numeric token is an init.
    pos <- 1
    out <- ""
    nchars <- nchar(text)
    while(pos <= nchars && length(queue)) {
      remaining <- substring(text, pos, nchars)
      m <- regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", remaining)
      if(m == -1) break
      rel <- as.integer(m); len <- attr(m, "match.length")
      out <- paste0(out, substring(remaining, 1, rel - 1))
      new_val <- fmt(queue[[1]]); queue <- queue[-1]
      out <- paste0(out, new_val)
      pos <- pos + rel + len - 1
    }
    out <- paste0(out, substring(text, pos, nchars))
    return(list(text = out, queue = queue))
  }
}

#' Build VPC obs/sim data for an nlmixr-format model
#'
#' Mirrors [create_vpc_data()] but stays in nlmixr2-land: the observation
#' dataset is taken from the model's data, and `n` simulation iterations
#' are produced via [run_sim()] (which dispatches to rxode2's `rxSolve`
#' for nlmixr2 models).
#'
#' @noRd
create_vpc_data_nlmixr <- function(
  fit = NULL,
  model = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE
) {
  if(is.null(model)) {
    if(is.null(fit)) cli::cli_abort("Need either `fit` or `model`.")
    model <- attr(fit, "final_model")
    if(is.null(model)) model <- attr(fit, "model")
  }

  ## Update estimates if supplied (or, when a fit is given, take them from
  ## the fit). For nlmixr2 simulations the model already carries the final
  ## estimates if `attr(fit, 'final_model')` is used.
  if(!is.null(parameters)) {
    model <- pharmr::set_initial_estimates(model, inits = parameters)
  } else if(!is.null(fit) && !is.null(fit$parameter_estimates) &&
            is.null(attr(fit, "final_model"))) {
    model <- pharmr::set_initial_estimates(model, inits = as.list(fit$parameter_estimates))
  }

  ## Prefer an explicitly-attached input dataset (set by run_nlme_nlmixr
  ## when `data` was supplied) over `model$dataset`, which may not have
  ## been refreshed.
  data <- as.data.frame(attr(model, "original_data") %||% model$dataset)

  ## Build obs from the model dataset. Compute TAD on the full event log
  ## (dose rows are needed to derive last_dose_time), then restrict to
  ## observation rows so the row-set matches what rxSolve returns and
  ## downstream VPC alignment between obs and sim is preserved.
  obs <- data
  if(!"EVID" %in% names(obs)) obs$EVID <- 0L
  if(!"MDV" %in% names(obs)) obs$MDV <- ifelse(obs$EVID == 0, 0L, 1L)
  if(is.null(obs$TAD)) {
    obs <- obs |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(last_dose_time = dplyr::if_else(.data$EVID == 1, .data$TIME, NA_real_)) |>
      tidyr::fill("last_dose_time", .direction = "downup") |>
      dplyr::mutate(TAD = .data$TIME - .data$last_dose_time) |>
      dplyr::select(-"last_dose_time") |>
      dplyr::ungroup() |>
      as.data.frame()
  }
  obs <- obs[obs$MDV == 0, , drop = FALSE]

  ## Run n simulations against the same dataset; reuse run_sim() so the
  ## engine dispatch lives in one place.
  if(verbose) cli::cli_alert_info("Running {n} simulations for VPC")
  sim <- run_sim_nlmixr(
    fit = fit,
    data = NULL,        # use model dataset
    model = model,
    n_iterations = n,
    verbose = FALSE
  )

  ## Optional column carry-over from obs to sim
  for(col in keep_columns) {
    if(col %in% names(obs)) {
      ## sim has multiple iterations per obs row; left_join on (ID, TIME)
      sim <- sim |>
        dplyr::left_join(
          obs[, c("ID", "TIME", col), drop = FALSE] |> unique(),
          by = c("ID", "TIME"),
          suffix = c("", ".obs")
        )
      ## prefer existing column on sim; otherwise rename .obs back
      sim_col <- paste0(col, ".obs")
      if(sim_col %in% names(sim) && !col %in% names(sim)) {
        names(sim)[names(sim) == sim_col] <- col
      }
    }
  }

  list(obs = obs, sim = sim)
}
