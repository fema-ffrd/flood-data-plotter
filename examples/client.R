#!/usr/bin/env Rscript

# Single File R Client for Flood Data Plotter API
# Takes a JSON file path and generates HTML + PNG output

# Source common utilities
source(file.path(dirname(sys.frame(1)$ofile), "client_utils.R"))

#' Process a single JSON file
#' @param json_file_path Path to input JSON file
#' @param output_dir Output directory (optional, defaults to test-output)
process_single_file <- function(json_file_path, output_dir = NULL) {
  
  wkdir <- get_working_directory()
  
  # Set default output directory
  if (is.null(output_dir)) {
    output_dir <- fs::path(wkdir, "test-output")
  }
  
  cat("Single File Flood Data Plotter Client\n")
  cat("Input file:", json_file_path, "\n")
  cat("Output dir:", output_dir, "\n\n")
  
  # Find API server
  api_url <- find_api_server()
  
  # Load JSON data
  json_data <- load_json_data(json_file_path)
  
  # Generate output base path
  output_base <- generate_output_base(json_file_path, output_dir)
  
  # Make API request
  result <- make_api_request(json_data, api_url, output_base)
  
  # Report results
  if (result$success) {
    cat("✅ Success! Files created:\n")
    cat("   HTML:", result$html_path, "\n")
    if (!is.na(result$png_path)) {
      cat("   PNG: ", result$png_path, "\n")
    }
    cat("   Duration:", round(result$duration, 2), "seconds\n")
  } else {
    cat("❌ Failed:", result$error, "\n")
    stop("Request failed")
  }
  
  return(result)
}

# Interactive helper function
run_example <- function() {
  wkdir <- get_working_directory()
  json_file <- file.path(gsub("examples", "server", wkdir), "src", "example-jsons", "flow-example.json")
  
  if (!fs::file_exists(json_file)) {
    cat("❌ Example file not found:", json_file, "\n")
    return(FALSE)
  }
  
  cat("Running example with:", json_file, "\n")
  process_single_file(json_file)
}

# Interactive helper function for parallel processing
run_parallel_example <- function() {
  wkdir <- get_working_directory()
  json_dir <- file.path(gsub("examples", "server", wkdir), "src", "example-jsons")
  
  if (!fs::dir_exists(json_dir)) {
    cat("❌ Example JSON directory not found:", json_dir, "\n")
    return(FALSE)
  }
  
  cat("Running parallel example with directory:", json_dir, "\n")
  
  # Source and run the parallel client
  source(file.path(wkdir, "client_parallel.R"))
  results <- process_directory_parallel(json_dir)
  
  return(results)
}

# Main execution when run as script
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Default: use example file
    wkdir <- get_working_directory()
    json_file <- file.path(gsub("examples", "server", wkdir), "src", "example-jsons", "flow-example.json")
    
    if (!fs::file_exists(json_file)) {
      cat("❌ Default example file not found:", json_file, "\n")
      cat("Usage: Rscript client.R [json_file_path] [output_dir]\n")
      quit(status = 1)
    }
    
    cat("Using default example file:", json_file, "\n")
    process_single_file(json_file)
    
  } else if (length(args) == 1) {
    # JSON file path provided
    process_single_file(args[1])
    
  } else if (length(args) == 2) {
    # JSON file and output directory provided
    process_single_file(args[1], args[2])
    
  } else {
    cat("Usage: Rscript client.R [json_file_path] [output_dir]\n")
    quit(status = 1)
  }
}
