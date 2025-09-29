#!/bin/bash

# FFRD Plotting Server - Production Startup Script

set -e

# Default values
HOST=${HOST:-0.0.0.0}
PORT=${PORT:-8080}
R_ENV=${R_ENV:-production}

echo "Starting FFRD Plotting Server..."
echo "Environment: $R_ENV"
echo "Host: $HOST"
echo "Port: $PORT"

# Change to app directory
cd /app

# Create log directory if it doesn't exist
mkdir -p logs

# Start the R Plumber server
exec R --no-restore --slave -e "
# Load configuration and start server
library(plumber)
library(here)

# Source the main application
source('api/plumber.R')

# Start server with configuration
cat('Starting server on', Sys.getenv('HOST', '0.0.0.0'), ':', Sys.getenv('PORT', '8080'), '\n')
pr\$run(
  host = Sys.getenv('HOST', '0.0.0.0'),
  port = as.integer(Sys.getenv('PORT', '8080')),
  debug = identical(Sys.getenv('PLUMBER_DEV', 'FALSE'), 'TRUE')
)
"
