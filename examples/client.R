#!/usr/bin/env Rscript

# R Client Script for Flood Data Plotter API
# Equivalent to client.py with similar functionality

# Load required packages
library(httr2)
library(jsonlite)
library(fs)

# Load webshot2 conditionally for PNG generation
webshot2_available <- requireNamespace("webshot2", quietly = TRUE)

# Configuration constants
LOCAL_HOST <- "127.0.0.1"
LOCAL_PORT <- 8081

DOCKER_HOST <- "localhost" 
DOCKER_PORT <- 8080

STATUS_ENDPOINT <- "/health/status"


# --USER INPUTS ---
EXAMPLE_FILE_PATH <- file.path(gsub("examples", "server", WKDIR), "src", "example-jsons", "flow-example.json")
EXAMPLE_OUTPUT_PATH <- file.path(WKDIR, "test-output", "Flow_AMS_Green_200th_01Hour")
ENDPOINT <- "/realization/flows"
HTML_AND_PNG <- TRUE

# Get working directory and file paths
WKDIR <- tryCatch({
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

if (is.na(WKDIR) || WKDIR == "") {
  WKDIR <- getwd()
}



save_html_to_png <- function(html_path, png_path, selector = NULL, 
                            width = 1280, height = 800, zoom = 2, delay = 0.5) {
  
  if (!webshot2_available) {
    stop("PNG output requested but webshot2 is not installed. ",
         "Run: install.packages('webshot2')")
  }
  
  html_file <- fs::path_abs(html_path)
  png_file <- fs::path(png_path)
  
  # Create output directory if needed
  if (!fs::dir_exists(fs::path_dir(png_file))) {
    fs::dir_create(fs::path_dir(png_file), recurse = TRUE)
  }
  
  # Convert to file:// URL
  file_url <- paste0("file://", html_file)
  
  tryCatch({
    if (!is.null(selector)) {
      # Screenshot specific element
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
      # Full page screenshot
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
    stop("PNG export failed: ", e$message)
  })
}


make_plot_request <- function(json_data, url, output_html_path, 
                             output_png_path = NULL, png_selector = NULL, 
                             timeout_sec = 60) {
  
  cat("Making request to:", url, "\n")
  
  # Create request
  req <- request(url) |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(json_data) |>
    req_timeout(timeout_sec)
  
  # Send request
  tryCatch({
    resp <- req_perform(req)
  }, error = function(e) {
    stop("HTTP request failed: ", e$message)
  })
  
  # Get response content
  content <- resp_body_string(resp)
  
  # Create output directory if needed
  out_path <- fs::path(output_html_path)
  if (!fs::dir_exists(fs::path_dir(out_path))) {
    fs::dir_create(fs::path_dir(out_path), recurse = TRUE)
  }
  
  # Save HTML/JSON content (even on error for debugging)
  writeLines(content, out_path, useBytes = TRUE)
  
  # Check response status
  if (resp_status(resp) != 200) {
    cat("Failed: Server returned HTTP", resp_status(resp), "\n")
    cat(substr(content, 1, 2000), "\n")
  }
  
  png_path <- NULL
  # Only attempt PNG if path provided and response was OK
  if (!is.null(output_png_path) && resp_status(resp) == 200) {
    tryCatch({
      png_path <- save_html_to_png(
        html_path = out_path,
        png_path = output_png_path,
        selector = png_selector,
        width = 1280,
        height = 800,
        zoom = 2,
        delay = 0.5
      )
      cat("PNG saved to:", fs::path_abs(png_path), "\n")
    }, error = function(e) {
      cat("PNG export failed:", e$message, "\n")
    })
  }
  
  cat("Request successful. File saved to:", fs::path_abs(out_path), "\n")
  
  return(list(
    response = resp,
    content = content,
    png_path = png_path
  ))
}


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


# --------- MAIN SCRIPT  ---------

cat("Working directory:", WKDIR, "\n")

LOCAL_URL <- paste0("http://", LOCAL_HOST, ":", LOCAL_PORT, "/api")
DOCKER_URL <- paste0("http://", DOCKER_HOST, ":", DOCKER_PORT, "/api")

# Try to connect to local development server first, then Docker
api_url <- NULL

if (check_server(LOCAL_URL)) {
  api_url <- paste0(LOCAL_URL, ENDPOINT)
  cat("✅ Connected to local development server at", LOCAL_URL, "\n")
} else if (check_server(DOCKER_URL)) {
  api_url <- paste0(DOCKER_URL, ENDPOINT)
  cat("✅ Connected to Docker server at", DOCKER_URL, "\n")
} else {
  stop("Could not connect to server at:\n",
       "  - Local development: ", LOCAL_URL, "\n",
       "  - Docker container: ", DOCKER_URL, "\n")
}

# Load JSON payload
if (!fs::file_exists(EXAMPLE_FILE_PATH)) {
  stop("Example JSON file not found at: ", EXAMPLE_FILE_PATH)
}

json_data <- jsonlite::fromJSON(EXAMPLE_FILE_PATH, simplifyVector = FALSE)

# Make request based on JSON output preference
if (isTRUE(json_data$json)) {
  cat("Requesting JSON output from server...\n")
  output_json_path <- paste0(EXAMPLE_OUTPUT_PATH, ".json")
  make_plot_request(json_data, api_url, output_json_path)
} else {
  cat("Requesting HTML output from server...\n")
  output_html_path <- paste0(EXAMPLE_OUTPUT_PATH, ".html")
  
  if (HTML_AND_PNG && webshot2_available) {
    output_png_path <- paste0(EXAMPLE_OUTPUT_PATH, ".png")
    make_plot_request(json_data, api_url, output_html_path, 
                     output_png_path = output_png_path)
  } else {
    if (HTML_AND_PNG && !webshot2_available) {
      cat("Note: webshot2 not available, skipping PNG generation\n")
    }
    make_plot_request(json_data, api_url, output_html_path)
  }
}

cat("Script completed successfully!\n")
