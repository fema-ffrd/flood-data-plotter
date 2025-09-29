#!/usr/bin/env Rscript

# Setup script for R client dependencies
# Run this script to install required packages for client.R

cat("Installing required R packages for flood-data-plotter client...\n")

# Required packages
required_packages <- c(
  "httr2",      # Modern HTTP client
  "jsonlite",   # JSON handling
  "fs"          # File system operations
)

# Optional packages for PNG generation
optional_packages <- c(
  "webshot2",   # HTML to PNG conversion
  "rstudioapi"  # For RStudio integration
)

# Install required packages
cat("Installing required packages:", paste(required_packages, collapse = ", "), "\n")
install.packages(required_packages, repos = "https://cran.r-project.org")

# Install optional packages with error handling
for (pkg in optional_packages) {
  tryCatch({
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing optional package:", pkg, "\n")
      install.packages(pkg, repos = "https://cran.r-project.org")
    } else {
      cat("Package", pkg, "already installed\n")
    }
  }, error = function(e) {
    cat("Warning: Failed to install optional package", pkg, ":", e$message, "\n")
  })
}

# Special note for webshot2
if (requireNamespace("webshot2", quietly = TRUE)) {
  cat("\n✅ webshot2 installed successfully!\n")
  cat("PNG generation will be available in client.R\n")
} else {
  cat("\n⚠️  webshot2 installation failed or not available\n") 
  cat("PNG generation will be skipped in client.R\n")
  cat("You can try installing manually with: install.packages('webshot2')\n")
}

cat("\n🎉 Setup complete! You can now run client.R\n")
cat("Usage: Rscript examples/client.R\n")
