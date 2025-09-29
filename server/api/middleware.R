# Middleware functions and route utilities for R Plumber API

#' Generic wrapper for plot routes that support JSON/HTML output
#' @param plotting_function The plotting function to call (e.g., realization_flow_summary_plot)
#' @param schema_name The JSON schema name for validation
#' @param req Plumber request object
#' @param res Plumber response object  
#' @return HTML or JSON response based on body$json parameter
create_plot_route <- function(plotting_function, schema_name, req, res) {
  # Validate JSON payload
  v <- validate_json_payload(schema_name, req$postBody, max_errors = 25)
  if (!isTRUE(v$ok)) {
    return(json_error(res, 422, list(
      message = "payload failed JSON Schema validation",
      schema_name = v$schema_name,
      errors = v$errors,        
      hint = v$hint
    )))
  }
  
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  
  tryCatch({
    # Call the plotting function with the request body parameters
    content <- do.call(plotting_function, body)
    
    # Normalize to single UTF-8 string
    if (is.list(content)) content <- unlist(content)
    content <- paste(content, collapse = "")
    content <- gsub("\r\n", "\n", content, fixed = TRUE)
    Encoding(content) <- "UTF-8"
    
    # Check if JSON output is requested
    if (isTRUE(body$json)) {
      # Return JSON content without HTML encoding
      res$status <- 200
      res$setHeader("Content-Type", "application/json; charset=utf-8")
      res$serializer <- plumber::serializer_json()
      
      # Parse the JSON content if it's a string, otherwise return as-is
      if (is.character(content)) {
        tryCatch({
          json_content <- jsonlite::fromJSON(content, simplifyVector = FALSE)
          return(json_content)
        }, error = function(e) {
          return(list(content = content, type = "json"))
        })
      } else {
        return(content)
      }
    } else {
      # Return raw HTML (default behavior)
      res$status <- 200
      res$setHeader("Content-Type", "text/html; charset=utf-8")
      res$serializer <- plumber::serializer_html()
      return(content)
    }
    
  }, error = function(e) {
    return(json_error(
      res,
      status = 400,
      obj = list(message = "failure", error = conditionMessage(e))
    ))
  })
}

#' CORS middleware
#' Adds CORS headers to allow cross-origin requests
#' 
#' @param req The request object
#' @param res The response object
cors_middleware <- function(req, res) {
  # Add CORS headers
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  # Handle preflight OPTIONS requests
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  # Continue to next handler
  plumber::forward()
}

#' Logging middleware
#' Logs incoming requests for debugging and saves to log files
#' 
#' @param req The request object
#' @param res The response object
logging_middleware <- function(req, res) {
  start_time <- Sys.time()
  
  # Create log entry
  log_entry <- sprintf("[%s] %s %s - %s\n", 
                      format(start_time, "%Y-%m-%d %H:%M:%S"), 
                      req$REQUEST_METHOD, 
                      req$PATH_INFO,
                      req$HTTP_USER_AGENT %||% "unknown")
  
  # Log to console
  cat(log_entry)
  
  # Log to file with rotation (if logs directory exists)
  log_dir <- "logs"
  if (dir.exists(log_dir)) {
    log_file <- file.path(log_dir, paste0("api-", Sys.Date(), ".log"))
    
    # Check file size and rotate if needed (limit: 10MB)
    if (file.exists(log_file)) {
      file_size <- file.size(log_file)
      if (!is.na(file_size) && file_size > 10 * 1024 * 1024) {  # 10MB
        # Rotate logs: keep last 3 files
        for (i in 2:1) {
          old_file <- paste0(log_file, ".", i)
          new_file <- paste0(log_file, ".", i + 1)
          if (file.exists(old_file)) {
            file.rename(old_file, new_file)
          }
        }
        file.rename(log_file, paste0(log_file, ".1"))
      }
    }
    
    cat(log_entry, file = log_file, append = TRUE)
  }
  
  # Continue to next handler
  plumber::forward()
}
