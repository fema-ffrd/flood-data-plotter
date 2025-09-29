#!/usr/bin/env Rscript

# Client Utilities for Flood Data Plotter API
# Common functions used by both single and parallel clients

# Load required packages
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(fs)
})

# Configuration constants
LOCAL_HOST <- "127.0.0.1"
LOCAL_PORT <- 8081
DOCKER_HOST <- "localhost" 
DOCKER_PORT <- 8080
STATUS_ENDPOINT <- "/health/status"
ENDPOINT <- "/realization/flows"

# Check if webshot2 is available for PNG generation
webshot2_available <- requireNamespace("webshot2", quietly = TRUE)

#' Get working directory with fallbacks
#' @return Working directory path
get_working_directory <- function() {
  wkdir <- tryCatch({
    dirname(rstudioapi::getActiveDocumentContext()$path)
  }, error = function(e) {
    # Fallback: use script location or current working directory
    if (length(commandArgs(trailingOnly = FALSE)) > 0) {
      script_path <- commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))]
      if (length(script_path) > 0) {
        return(dirname(sub("--file=", "", script_path)))
      }
    }
    getwd()
  })
  
  if (is.na(wkdir) || wkdir == "") {
    wkdir <- getwd()
  }
  
  return(wkdir)
}

#' Check if server is available at given URL
#' @param base_url Base URL to check
#' @return TRUE if server responds, FALSE otherwise
check_server <- function(base_url) {
  tryCatch({
    health_url <- paste0(base_url, STATUS_ENDPOINT)
    resp <- request(health_url) |>
      req_timeout(2) |>
      req_perform()
    return(resp_status(resp) == 200)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Find available API server (local first, then Docker)
#' @return API endpoint URL or stops with error
find_api_server <- function() {
  local_url <- paste0("http://", LOCAL_HOST, ":", LOCAL_PORT, "/api")
  docker_url <- paste0("http://", DOCKER_HOST, ":", DOCKER_PORT, "/api")
  
  if (check_server(local_url)) {
    api_url <- paste0(local_url, ENDPOINT)
    cat("✅ Connected to local development server at", local_url, "\n")
    return(api_url)
  } else if (check_server(docker_url)) {
    api_url <- paste0(docker_url, ENDPOINT) 
    cat("✅ Connected to Docker server at", docker_url, "\n")
    return(api_url)
  } else {
    stop("❌ Could not connect to server at:\n",
         "  - Local development: ", local_url, "\n",
         "  - Docker container: ", docker_url, "\n")
  }
}

#' Save HTML to PNG using webshot2
#' @param html_path Path to HTML file
#' @param png_path Output PNG path  
#' @param selector CSS selector to screenshot (NULL = full page)
#' @param width Viewport width
#' @param height Viewport height
#' @param zoom Zoom factor
#' @param delay Wait time in seconds
#' @return Path to PNG file or NULL if failed
save_html_to_png <- function(html_path, png_path, selector = NULL, 
                            width = 1280, height = 800, zoom = 2, delay = 0.5) {
  
  if (!webshot2_available) {
    warning("webshot2 not available, skipping PNG generation")
    return(NULL)
  }
  
  html_file <- fs::path_abs(html_path)
  png_file <- fs::path(png_path)
  
  # Create output directory if needed
  if (!fs::dir_exists(fs::path_dir(png_file))) {
    fs::dir_create(fs::path_dir(png_file), recurse = TRUE)
  }
  
  file_url <- paste0("file://", html_file)
  
  tryCatch({
    if (!is.null(selector)) {
      webshot2::webshot(
        url = file_url,
        file = as.character(png_file),
        selector = selector,
        vwidth = width,
        vheight = height, 
        zoom = zoom,
        delay = delay
      )
    } else {
      webshot2::webshot(
        url = file_url,
        file = as.character(png_file),
        vwidth = width,
        vheight = height,
        zoom = zoom,
        delay = delay
      )
    }
    
    return(png_file)
    
  }, error = function(e) {
    warning("PNG export failed: ", e$message)
    return(NULL)
  })
}

#' Load JSON data from file
#' @param json_file_path Path to JSON file
#' @return Parsed JSON data as list
load_json_data <- function(json_file_path) {
  if (!fs::file_exists(json_file_path)) {
    stop("JSON file not found: ", json_file_path)
  }
  
  tryCatch({
    jsonlite::fromJSON(json_file_path, simplifyVector = FALSE)
  }, error = function(e) {
    stop("Failed to parse JSON file '", json_file_path, "': ", e$message)
  })
}

#' Make API request and save outputs
#' @param json_data Parsed JSON data to send
#' @param api_url API endpoint URL
#' @param output_base Base path for output files (without extension)
#' @param job_id Optional job identifier for logging
#' @param timeout_sec Request timeout in seconds
#' @return List with success status, paths, and timing info
make_api_request <- function(json_data, api_url, output_base, 
                            job_id = NULL, timeout_sec = 60) {
  
  start_time <- Sys.time()
  
  # Generate output paths
  html_path <- paste0(output_base, ".html")
  png_path <- paste0(output_base, ".png")
  
  # Log start
  if (!is.null(job_id)) {
    cat("[", job_id, "] Starting request to API...\n")
  }
  
  # Create request
  req <- request(api_url) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(json_data) |>
    req_timeout(timeout_sec)
  
  # Send request
  resp_result <- tryCatch({
    resp <- req_perform(req)
    list(success = TRUE, response = resp)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  # Handle request failure
  if (!resp_result$success) {
    duration <- as.numeric(Sys.time() - start_time, units = "secs")
    if (!is.null(job_id)) {
      cat("[", job_id, "] ❌ Request failed:", resp_result$error, "\n")
    }
    return(list(
      success = FALSE,
      error = resp_result$error,
      duration = duration,
      html_path = NA,
      png_path = NA
    ))
  }
  
  resp <- resp_result$response
  
  # Get response content
  content <- resp_body_string(resp)
  
  # Create output directory if needed
  out_dir <- fs::path_dir(html_path)
  if (!fs::dir_exists(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
  }
  
  # Save HTML content
  writeLines(content, html_path, useBytes = TRUE)
  
  # Check response status
  success <- resp_status(resp) == 200
  if (!success) {
    duration <- as.numeric(Sys.time() - start_time, units = "secs")
    if (!is.null(job_id)) {
      cat("[", job_id, "] ❌ Server error: HTTP", resp_status(resp), "\n")
    }
    return(list(
      success = FALSE,
      error = paste("HTTP", resp_status(resp)),
      duration = duration,
      html_path = html_path,
      png_path = NA
    ))
  }
  
  # Generate PNG
  png_result <- save_html_to_png(html_path, png_path)
  png_success <- !is.null(png_result)
  
  duration <- as.numeric(Sys.time() - start_time, units = "secs")
  
  # Log completion
  if (!is.null(job_id)) {
    cat("[", job_id, "] ✅ Completed in", round(duration, 2), "seconds")
    if (png_success) {
      cat(" (HTML + PNG)")
    } else {
      cat(" (HTML only)")
    }
    cat("\n")
  }
  
  return(list(
    success = TRUE,
    duration = duration,
    html_path = html_path,
    png_path = if (png_success) png_path else NA,
    response_status = resp_status(resp)
  ))
}

#' Generate output base path from JSON file
#' @param json_file_path Input JSON file path
#' @param output_dir Output directory
#' @param suffix Optional suffix to add to filename
#' @return Base path for output files (without extension)
generate_output_base <- function(json_file_path, output_dir, suffix = NULL) {
  base_name <- fs::path_ext_remove(fs::path_file(json_file_path))
  
  if (!is.null(suffix)) {
    base_name <- paste0(base_name, "_", suffix)
  }
  
  fs::path(output_dir, base_name)
}
