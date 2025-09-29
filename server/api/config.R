# Configuration loader for R Plumber API
# This uses environment variables for configuration

default_port <- 8080
default_host <- "0.0.0.0"
default_log_level <- "INFO"

#' Load configuration from environment variables
#' 
#' @return List containing configuration settings
load_config <- function() {
  config <- list(
    server = list(
      host = Sys.getenv("HOST", default_host),
      port = as.integer(Sys.getenv("PORT", default_port)),
      dev_mode = as.logical(Sys.getenv("PLUMBER_DEV", "FALSE"))
    ),
    logging = list(
      level = Sys.getenv("LOG_LEVEL", default_log_level),
      enable_http_logging = as.logical(Sys.getenv("LOG_HTTP", "true")),
      pretty_print = as.logical(Sys.getenv("LOG_PRETTY", "false"))
    ),
    api = list(
      timeout_seconds = as.integer(Sys.getenv("API_TIMEOUT", "60")),
      max_request_size = Sys.getenv("MAX_REQUEST_SIZE", "10MB"),
      enable_cors = as.logical(Sys.getenv("ENABLE_CORS", "true"))
    ),
    environment = Sys.getenv("R_ENV", "development")
  )
  
  return(config)
}
