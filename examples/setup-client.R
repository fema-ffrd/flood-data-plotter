#!/usr/bin/env Rscript

# Setup script for parallel R client dependencies
# Run this script to install required packages for parallel-client.R

cat("Installing packages for parallel flood-data-plotter client...\n\n")

# Required packages for parallel processing
required_packages <- c(
  "httr2",      # Modern HTTP client
  "jsonlite",   # JSON handling
  "fs",         # File system operations
  "future",     # Asynchronous parallel processing
  "furrr",      # Future-based apply functions
  "purrr",      # Functional programming tools
  "tibble"      # Modern data frames
)

# Optional packages
optional_packages <- c(
  "webshot2",   # HTML to PNG conversion
  "rstudioapi"  # For RStudio integration
)

# Install required packages
cat("Installing required packages...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, repos = "https://cran.r-project.org")
  } else {
    cat("✅ Already installed:", pkg, "\n")
  }
}

# Install optional packages with error handling
cat("\nInstalling optional packages...\n")
for (pkg in optional_packages) {
  tryCatch({
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("📥 Installing:", pkg, "\n")
      install.packages(pkg, repos = "https://cran.r-project.org")
    } else {
      cat("✅ Already installed:", pkg, "\n")
    }
  }, error = function(e) {
    cat("⚠️  Failed to install", pkg, ":", e$message, "\n")
  })
}

# Test parallel processing setup
cat("\n🧪 Testing parallel processing setup...\n")
tryCatch({
  library(future)
  library(furrr)
  
  # Test basic parallel functionality
  plan(multisession, workers = 2)
  test_result <- future_map(1:3, ~ Sys.getpid())
  plan(sequential)
  
  unique_pids <- length(unique(unlist(test_result)))
  cat("✅ Parallel processing test successful!\n")
  cat("   - Used", unique_pids, "different processes\n")
  
}, error = function(e) {
  cat("❌ Parallel processing test failed:", e$message, "\n")
})

# Performance recommendations
cat("\nPERFORMANCE RECOMMENDATIONS:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("• Default workers: 3 (adjust MAX_WORKERS in parallel-client.R)\n")
cat("• For CPU-intensive tasks: workers = parallel::detectCores() - 1\n")
cat("• For I/O-intensive tasks: workers = 4-8\n")
cat("• PNG generation disabled by default (resource intensive)\n")
cat("• Monitor memory usage with many concurrent requests\n")

cat("\n Setup complete! You can now run:\n")
cat("   Rscript examples/parallel-client.R\n")
cat("\n For custom usage, see the main() function in parallel-client.R\n")
