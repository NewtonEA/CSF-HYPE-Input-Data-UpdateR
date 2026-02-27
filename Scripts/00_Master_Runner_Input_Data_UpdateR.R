# ============================================================
# CSF-HYPE Input Data UpdateR
# Master pipeline runner 
# 
#  Ordered, resumable execution of update scripts 
#  Sourcing this script will run the update pipeline, includes
# validation checks and will stop if any required inputs are missing
# with error handling and logging
# ============================================================

# load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here,
               tibble,
               digest)

# ============================================================
# Configuration
# ============================================================

CONFIG <- list(
  status_dir = here("status_tracking"),
  log_file = here("pipeline_run.log"),
  force_rerun = FALSE  # Set TRUE to ignore success markers
)

# ============================================================
# Logging 
# ============================================================

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s\n", timestamp, level, msg)
  
  cat(log_entry)
  cat(log_entry, file = CONFIG$log_file, append = TRUE)
  flush.console()
}

# ============================================================
# Success tracking
# ============================================================

success_file <- function(script_name) {
  # Ensure status directory exists before returning path
  dir.create(CONFIG$status_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(CONFIG$status_dir, paste0(script_name, "_SUCCESS.rds"))
}

script_has_success <- function(script_name, required_inputs = NULL) {
  
  if (CONFIG$force_rerun) return(FALSE)
  
  sf <- success_file(script_name)
  if (!file.exists(sf)) return(FALSE)
  
  if (is.null(required_inputs)) return(TRUE)
  
  info <- readRDS(sf)
  old_hashes <- info$input_hashes
  new_hashes <- hash_inputs(required_inputs)
  
  if (is.null(old_hashes)) return(TRUE)
  
  !isFALSE(identical(old_hashes, new_hashes)) # scripts will re-run if inputs have changed
}


hash_inputs <- function(paths) {
  if (is.null(paths)) return(NULL)
  
  paths <- normalizePath(paths, mustWork = FALSE)
  
  tibble::tibble(
    path = paths,
    exists = file.exists(paths),
    hash = ifelse(
      file.exists(paths),
      vapply(paths, digest::digest, character(1), algo = "md5"),
      NA_character_
    )
  )
}

mark_success <- function(script_name, outputs, required_inputs, runtime_secs) {
  
  success_data <- list(
    script = script_name,
    completed_at = Sys.time(),
    runtime_secs = runtime_secs,
    outputs_created = outputs,
    input_hashes = hash_inputs(required_inputs),
    r_version = R.version.string,
    session_hash = digest::digest(sessionInfo(), algo = "md5")
  )
  
  saveRDS(success_data, success_file(script_name))
}


get_success_info <- function(script_name) {
  sf <- success_file(script_name)
  if (file.exists(sf)) {
    return(readRDS(sf))
  }
  return(NULL)
}

# ============================================================
# Script execution
# ============================================================
run_script <- function(script_path, script_file, required_inputs = NULL) {
  

  # Log start of script execution for runtime tracking
  log_message(paste("Running", script_file))
  start_time <- Sys.time()
  

  # Validate that all required input files exist before execution
  # prevents partial runs / downstream failures
  if (!is.null(required_inputs)) {
    missing_inputs <- required_inputs[!file.exists(required_inputs)]
    if (length(missing_inputs) > 0) {
      stop(
        "Missing required inputs for ", script_file, ":\n",
        paste("  -", missing_inputs, collapse = "\n")
      )
    }
  }
  # Ensure the script itself exists before attempting to source it
  if (!file.exists(script_path)) {
    stop("Script not found: ", script_path)
  }
  # Create an isolated execution environment for the script
  env <- new.env(parent = globalenv())
  

  # Source the script with error handling
  # On failure:
  #   - log the error message
  #   - log the traceback for debugging
  #   - stop the pipeline immediately

  tryCatch(
    source(script_path, local = env),
    error = function(e) {
      log_message(paste("Failed:", script_file, "-", e$message), "ERROR")
      log_message(paste(capture.output(traceback()), collapse = "\n"), "ERROR")
      stop(e)
    }
  )
  

  # Enforce pipeline contract:
  # every script must declare its outputs explicitly via
  # `declared_outputs`

  if (!exists("declared_outputs", envir = env)) {
    stop(script_file, " did not define 'declared_outputs' variable")
  }
    # Validate that all declared outputs were actually created
  outputs <- env$declared_outputs
  missing <- outputs[!file.exists(outputs)]
  
  if (length(missing) > 0) {
    stop(
      script_file, " finished but missing declared outputs:\n",
      paste("  -", missing, collapse = "\n")
    )
  }
  
  runtime_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  

  # Persist success marker including:
  #   - outputs created
  #   - input hashes (for change detection)
  #   - runtime
  #   - environment/session metadata

  mark_success(
    script_name = script_file,
    outputs = outputs,
    required_inputs = required_inputs,
    runtime_secs = runtime_secs
  )
  

  # Log successful completion with output count and runtime

  log_message(
    paste("Completed", script_file, "-", length(outputs),
          "outputs |", round(runtime_secs, 1), "secs")
  )
  

  # Return outputs invisibly (useful for interactive use or testing)

  invisible(outputs)
}

# ============================================================
# Pipeline definition
# ============================================================

pipeline <- tibble::tribble(
  ~order, ~script_file, ~script_path, ~required_inputs, 
  
  1, "1_Update_Forcing_data.R", 
  "Scripts/1_Update_Forcing_data.R", 
  NULL, #required inputs are defined in individual scripts, but could be added here
  
  2, "2_Extract_Flow_data.R", 
  "Scripts/2_Extract_Flow_data.R",
  NULL,  
  
  3, "3_Extract_consents.R", 
  "Scripts/3_Extract_consents.R",
  NULL,
  
  4, "4_Extract&Update_observations.R", 
  "Scripts/4_Extract&Update_observations.R",
  NULL,
  
  5, "5_Prepare_consents.R", 
  "Scripts/5_Prepare_consents.R",
  NULL,
  
  6, "6_Collate_EDM_returns.R", 
  "Scripts/6_Collate_EDM_returns.R",
  NULL,
  
  7, "7_Create_Intermittents_monthly_series.R", 
  "Scripts/7_Create_Intermittents_monthly_series.R",
  NULL,
  
  8, "8_Create_Abstractions_monthly_series.R", 
  "Scripts/8_Create_Abstractions_monthly_series.R",
  NULL,
  
  9, "9_Create_CattleInStreams_monthly_series.R", 
  "Scripts/9_Create_CattleInStreams_monthly_series.R",
  NULL,
  
  10, "10_Make_Discharge_PSData_master.R", 
  "Scripts/10_Make_PSData_master.R",
  NULL, 
  
  11, "11_Prepare_STW_flow_data.R", 
  "Scripts/11_Prepare_STW_flow_data.R",
  NULL, 
  
  12, "12_Create_Quantitative_monthly_series.R", 
  "Scripts/12_Create_Quantitative_monthly_series.R",
  NULL,
  
  13, "13_Make_final_PointSource_Inputs.R", 
  "Scripts/13_Make_final_PointSource_Inputs.R",
  NULL, 
  
  14, "14_Create_Qobs_input.R", 
  "Scripts/14_Create_Qobs_input.R",
  NULL, 
  
  15, "15_Create_Xobs_input.R", 
  "Scripts/15_Create_Xobs_input.R",
  NULL
  
)

# ============================================================
# Pipeline execution
# ============================================================

run_pipeline <- function(start_from = 1, stop_at = nrow(pipeline)) {
  
  log_message("=== Starting CSF-HYPE input data update pipeline ===")
  log_message(paste("Running steps", start_from, "to", stop_at))
  
  start_time <- Sys.time()
  completed <- 0
  skipped <- 0
  
  for (i in start_from:stop_at) {
    
    row <- pipeline[i, ]
    req_inputs <- row$required_inputs[[1]]
    
    if (script_has_success(row$script_file, req_inputs)) {
      
      info <- get_success_info(row$script_file)
      log_message(
        paste0(
          "Skipping ", row$script_file,
          " (completed ",
          format(info$completed_at, "%Y-%m-%d %H:%M"),
          ", inputs unchanged)"
        ),
        "SKIP"
      )
      
      skipped <- skipped + 1
      
    } else {
      
      tryCatch({
        run_script(
          script_path = here(row$script_path),
          script_file = row$script_file,
          required_inputs = req_inputs
        )
        completed <- completed + 1
        
      }, error = function(e) {
        log_message(paste("Pipeline failed at step", i), "ERROR")
        log_message(paste("Error:", e$message), "ERROR")
        stop("Pipeline execution stopped")
      })
    }
  }
  
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  
  log_message("=== Pipeline complete ===")
  log_message(paste(
    "Completed:", completed,
    "| Skipped:", skipped,
    "| Time:", round(elapsed, 2), "mins"
  ))
  
  invisible(list(
    completed = completed,
    skipped = skipped,
    elapsed = elapsed
  ))
}

# ============================================================
# Utility functions
# ============================================================

clear_success_markers <- function(script_pattern = NULL) {
  
  success_files <- list.files(
    CONFIG$status_dir,
    pattern = "_SUCCESS\\.rds$",
    full.names = TRUE
  )
  
  if (!is.null(script_pattern)) {
    success_files <- grep(script_pattern, success_files, value = TRUE)
  }
  
  if (length(success_files) > 0) {
    file.remove(success_files)
    log_message(paste("Removed", length(success_files), "success markers"))
  } else {
    log_message("No success markers to remove")
  }
}

print_pipeline_status <- function() {
  cat("\n=== Pipeline Status ===\n\n")
  
  for (i in seq_len(nrow(pipeline))) {
    row <- pipeline[i, ]
    status <- if (script_has_success(row$script_file)) "✓ DONE" else "○ PENDING"
    
    info <- get_success_info(row$script_file)
    time_info <- if (!is.null(info)) {
      paste0(" (", format(info$completed_at, "%Y-%m-%d %H:%M"), ")")
    } else {
      ""
    }
    
    cat(sprintf("%2d. %s %s%s\n", i, status, row$script_file, time_info))
  }
  
  cat("\n")
}

# ============================================================
# Execute pipeline (runs when sourced)
# ============================================================

# Print current status
print_pipeline_status()

# Run pipeline
result <- run_pipeline()

#clear markers for next run

on.exit(clear_success_markers(), add = TRUE)

log_message("CSF-HYPE input data update complete!")