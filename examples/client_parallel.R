#!/usr/bin/env Rscript

# Parallel R Client for Flood Data Plotter API
# Processes directory of JSON files using process_single_file in parallel

# Load required packages
suppressPackageStartupMessages({
  library(future)
  library(furrr)
  library(purrr)
})

# Suppress harmless future connection warnings
options(future.rng.onMisuse = "ignore")
suppressWarnings({
  options(future.supportsMulticore.unstable = "quiet")
})

# Source common utilities and single client
source(file.path(dirname(sys.frame(1)$ofile), "client_utils.R"))
source(file.path(dirname(sys.frame(1)$ofile), "client.R"))

# Configuration
DEFAULT_WORKERS <- 3

#' Process a single file with error handling for parallel execution
#' @param json_file_path Path to JSON file
#' @param output_dir Output directory
#' @param job_id Job identifier for logging
#' @return Result list with success status and details
process_file_parallel <- function(json_file_path, output_dir, job_id) {
  
  start_time <- Sys.time()
  
  tryCatch({
    cat("[", job_id, "] Processing:", basename(json_file_path), "\n")
    
    # Use the same process_single_file function but capture output
    result <- process_single_file(json_file_path, output_dir)
    
    duration <- as.numeric(Sys.time() - start_time, units = "secs")
    
    return(list(
      job_id = job_id,
      input_file = json_file_path,
      success = TRUE,
      html_path = result$html_path,
      png_path = if (!is.na(result$png_path)) result$png_path else NA,
      duration = duration,
      error = NA
    ))
    
  }, error = function(e) {
    duration <- as.numeric(Sys.time() - start_time, units = "secs")
    
    # Warning instead of stopping
    warning("[", job_id, "] ⚠️  Failed to process ", basename(json_file_path), ": ", e$message)
    
    return(list(
      job_id = job_id,
      input_file = json_file_path,
      success = FALSE,
      html_path = NA,
      png_path = NA,
      duration = duration,
      error = as.character(e$message)
    ))
  })
}

#' Process directory of JSON files in parallel
#' @param json_dir Directory containing JSON files
#' @param output_dir Output directory (optional)
#' @param max_workers Number of parallel workers (default: 3)
process_directory_parallel <- function(json_dir, output_dir = NULL, max_workers = DEFAULT_WORKERS) {
  
  wkdir <- get_working_directory()
  
  # Set default output directory
  if (is.null(output_dir)) {
    output_dir <- fs::path(wkdir, "test-output", "parallel-results")
  }
  
  cat("Parallel Flood Data Plotter Client\n")
  cat("Input directory:", json_dir, "\n")
  cat("Output directory:", output_dir, "\n")
  cat("Workers:", max_workers, "\n\n")
  
  # Find JSON files
  if (!fs::dir_exists(json_dir)) {
    stop("❌ Input directory not found: ", json_dir)
  }
  
  json_files <- fs::dir_ls(json_dir, regexp = "\\.json$", type = "file")
  
  if (length(json_files) == 0) {
    stop("❌ No JSON files found in: ", json_dir)
  }
  
  cat("Found", length(json_files), "JSON files:\n")
  for (f in json_files) {
    cat("   -", fs::path_file(f), "\n")
  }
  cat("\n")
  
  # Create output directory
  fs::dir_create(output_dir, recurse = TRUE)
  
  # Setup parallel processing
  plan(multisession, workers = max_workers)
  on.exit(plan(sequential), add = TRUE)
  
  cat("Starting parallel processing...\n\n")
  start_time <- Sys.time()
  
  # Generate job IDs
  job_ids <- paste0("job_", sprintf("%03d", seq_along(json_files)))
  
  # Process files in parallel (suppress connection warnings)
  results <- suppressWarnings({
    future_map2(json_files, job_ids, function(json_file, job_id) {
      process_file_parallel(json_file, output_dir, job_id)
    }, .options = furrr_options(seed = TRUE))
  })
  
  total_time <- as.numeric(Sys.time() - start_time, units = "secs")
  
  # Summarize results
  successful <- sum(map_lgl(results, ~ .x$success))
  failed <- length(results) - successful
  
  # Print summary
  cat("\n📊 PARALLEL EXECUTION SUMMARY\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("Total time:     ", round(total_time, 2), "seconds\n")
  cat("Total files:    ", length(json_files), "\n")
  cat("✅ Successful:  ", successful, "\n")
  cat("⚠️  Failed:      ", failed, "\n")
  
  if (successful > 0) {
    successful_results <- results[map_lgl(results, ~ .x$success)]
    avg_duration <- mean(map_dbl(successful_results, ~ .x$duration))
    cat("Avg duration:   ", round(avg_duration, 2), "seconds\n")
  }
  
  cat("Workers used:   ", max_workers, "\n")
  
  # Report failures with warnings (non-blocking)
  if (failed > 0) {
    cat("\n⚠️  FAILED FILES (warnings issued):\n")
    failed_results <- results[!map_lgl(results, ~ .x$success)]
    for (result in failed_results) {
      cat("   -", result$job_id, "(", fs::path_file(result$input_file), "):", result$error, "\n")
    }
  }
  
  # Report successful files
  if (successful > 0) {
    cat("\n✅ SUCCESSFUL FILES:\n")
    successful_results <- results[map_lgl(results, ~ .x$success)]
    for (result in successful_results) {
      cat("   -", result$job_id, "(", fs::path_file(result$input_file), "): HTML + PNG\n")
    }
  }
  
  cat("\nParallel processing complete!\n")
  cat("Results in:", output_dir, "\n")
  
  return(results)
}

#' Interactive helper function for testing
run_parallel_example <- function(max_workers = DEFAULT_WORKERS) {
  wkdir <- get_working_directory()
  json_dir <- file.path(gsub("examples", "server", wkdir), "src", "example-jsons")
  
  if (!fs::dir_exists(json_dir)) {
    cat("❌ Example JSON directory not found:", json_dir, "\n")
    return(FALSE)
  }
  
  cat("Running parallel example with directory:", json_dir, "\n")
  cat("Using", max_workers, "workers\n")
  
  results <- process_directory_parallel(json_dir, max_workers = max_workers)
  
  # Return summary
  successful <- sum(map_lgl(results, ~ .x$success))
  total <- length(results)
  
  cat("\nQuick Summary: ", successful, "/", total, " files successful\n")
  
  return(results)
}

# Main execution when run as script
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Default: use example JSON directory
    wkdir <- get_working_directory()
    json_dir <- file.path(gsub("examples", "server", wkdir), "src", "example-jsons")
    
    if (!fs::dir_exists(json_dir)) {
      cat("❌ Default JSON directory not found:", json_dir, "\n")
      cat("Usage: Rscript client_parallel.R [json_directory] [output_directory] [max_workers]\n")
      quit(status = 1)
    }
    
    cat("Using default JSON directory:", json_dir, "\n")
    process_directory_parallel(json_dir)
    
  } else if (length(args) == 1) {
    # JSON directory provided
    process_directory_parallel(args[1])
    
  } else if (length(args) == 2) {
    # JSON directory and output directory provided
    process_directory_parallel(args[1], args[2])
    
  } else if (length(args) == 3) {
    # All parameters provided
    max_workers <- as.integer(args[3])
    if (is.na(max_workers) || max_workers < 1) {
      stop("❌ Invalid max_workers value: ", args[3])
    }
    process_directory_parallel(args[1], args[2], max_workers)
    
  } else {
    cat("Usage: Rscript client_parallel.R [json_directory] [output_directory] [max_workers]\n")
    quit(status = 1)
  }
}
