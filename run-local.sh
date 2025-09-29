#!/bin/bash
# run-local.sh
# Shell script to run the flood-data-plotter API locally

set -e

echo "FFRD Flood Data Plotter - Local Development Setup"
echo "=================================================="

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "❌ R is not installed. Please install R first."
    exit 1
fi

echo "✅ R found: $(R --version | head -n1)"

# Check if we're in the right directory
if [ ! -f "setup-local-r-env.R" ]; then
    echo "❌ Please run this script from the flood-data-plotter root directory"
    exit 1
fi

# Run setup if it hasn't been done
if [ ! -d "server/renv" ]; then
    echo "📦 Setting up R environment (this may take a few minutes)..."
    Rscript setup-local-r-env.R
fi

echo "Starting local server..."

# Run the server
Rscript run-local-server.R
