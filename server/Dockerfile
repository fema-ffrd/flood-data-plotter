# Use the Rocker r-ver image as base for reproducible R environment
FROM rocker/r-ver:4.5.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libsodium-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install chromium for webshot2 (used for plot rendering)
RUN apt-get update && apt-get install -y \
    chromium-browser \
    && rm -rf /var/lib/apt/lists/*

# Set chromium path for webshot2
ENV CHROMOTE_CHROME=/usr/bin/chromium-browser

# Create app directory
WORKDIR /app

# Copy renv files first for better Docker layer caching
COPY server/renv.lock renv.lock

# Install renv and restore packages
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN R -e "renv::consent(provided = TRUE)"
RUN R -e "renv::restore()"

# Ensure rapidoc and listviewer are installed (sometimes missing from renv restore)
RUN R -e "if (!require('rapidoc', quietly = TRUE)) install.packages('rapidoc', repos = 'https://cloud.r-project.org')"
RUN R -e "if (!require('listviewer', quietly = TRUE)) install.packages('listviewer', repos = 'https://cloud.r-project.org')"

# Copy application files
COPY server/ ./

# Create necessary directories
RUN mkdir -p logs tmp

# Set environment variables
ENV R_ENV=production
ENV HOST=0.0.0.0
ENV PORT=8080
ENV PLUMBER_DEV=FALSE
ENV LOG_HTTP=true

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8080/status || exit 1

# Make startup script executable
RUN chmod +x /app/scripts/start-server.sh

# Run the application
CMD ["/app/scripts/start-server.sh"]
