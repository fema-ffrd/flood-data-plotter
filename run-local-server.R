#!/usr/bin/env Rscript
# run-local-server.R
# Script to run the flood-data-plotter API locally

local_host <- "127.0.0.1"
local_port <- 8081

# Set working directory to server folder
setwd("server")

# Set environment variables for local development
Sys.setenv(
  R_ENV = "development",
  HOST = local_host, 
  PORT = local_port,
  PLUMBER_DEV = "TRUE",
  LOG_HTTP = "true",
  API_TITLE = "R Plotting Server for FFRD (Local)",
  API_DESCRIPTION = "API for data visualization - Local Development"
)

# Function to check if port is in use and kill process if needed
check_and_kill_port <- function(port) {
  if (Sys.info()["sysname"] == "Windows") {
    # Windows command to find process using port
    cmd <- paste0("netstat -ano | findstr :", port)
    result <- tryCatch(system(cmd, intern = TRUE, ignore.stderr = TRUE), error = function(e) character(0))
    
    if (length(result) > 0) {
      # Extract PID and kill process
      for (line in result) {
        if (grepl("LISTENING", line)) {
          pid <- trimws(strsplit(line, "\\s+")[[1]][5])
          if (!is.na(as.numeric(pid))) {
            cat(sprintf("🔄 Killing process %s using port %d...\n", pid, port))
            system(paste0("taskkill /PID ", pid, " /F"), ignore.stdout = TRUE, ignore.stderr = TRUE)
          }
        }
      }
    }
  } else {
    # Unix/Linux/macOS command to find and kill process using port
    cmd <- paste0("lsof -ti:", port)
    pids <- tryCatch(system(cmd, intern = TRUE, ignore.stderr = TRUE), error = function(e) character(0))
    
    if (length(pids) > 0 && pids != "") {
      for (pid in pids) {
        if (!is.na(as.numeric(pid))) {
          cat(sprintf("🔄 Killing process %s using port %d...\n", pid, port))
          system(paste0("kill -9 ", pid), ignore.stdout = TRUE, ignore.stderr = TRUE)
          Sys.sleep(1)  # Give it a moment to terminate
        }
      }
    }
  }
}

cat("Starting FFRD Plotting Server (Local Development)...\n")
cat("Environment: development\n")
cat("Host: ", local_host, "\n")
cat("Port: ", local_port, "\n")
cat("=====================================\n")

# Check and kill any process using our port
cat("🔍 Checking if port", local_port, "is available...\n")
check_and_kill_port(local_port)

# Verify port is now free
port_check_cmd <- if (Sys.info()["sysname"] == "Windows") {
  paste0("netstat -ano | findstr :", local_port)
} else {
  paste0("lsof -ti:", local_port)
}

port_in_use <- tryCatch({
  result <- system(port_check_cmd, intern = TRUE, ignore.stderr = TRUE)
  length(result) > 0 && result != ""
}, error = function(e) FALSE)

if (port_in_use) {
  cat("❌ Port", local_port, "is still in use. Please manually kill the process or choose a different port.\n")
  cat("💡 Try: lsof -ti:", local_port, "| xargs kill -9\n")
  quit(status = 1)
} else {
  cat("✅ Port", local_port, "is available.\n")
}

# Load the plumber API
source("api/plumber.R")

cat(sprintf("Starting server on %s:%d\n", local_host, local_port))
cat(sprintf("Running plumber API at http://%s:%d\n", local_host, local_port))
cat(sprintf("Running swagger Docs at http://%s:%d/__docs__/\n", local_host, local_port))

plumber::pr_run(pr, host = local_host, port = local_port)
