#!/usr/bin/env Rscript
# setup-local-r-env.R
# Script to set up local R environment for flood-data-plotter

cat("Setting up local R environment for flood-data-plotter...\n")

# Set working directory to server folder
setwd("server")

# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installing renv...\n")
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Initialize renv and restore packages
cat("Initializing renv and restoring packages...\n")
renv::consent(provided = TRUE)
renv::restore()

# Install additional packages that might be missing
additional_packages <- c("rapidoc", "listviewer")
for (pkg in additional_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

cat("Setup complete! You can now run the server with:\n")
cat("cd server && Rscript -e \"source('api/plumber.R')\"\n")
