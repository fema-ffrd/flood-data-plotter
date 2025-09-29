library(plumber)
library(jsonlite)
library(jsonvalidate)
library(tools)
library(rapidoc)
library(here)
library(yaml)

source(here("api", "utils_api.R"))
source(here("api", "config.R"))
source(here("api", "middleware.R"))

# ---- Load configuration ----
CONFIG <- load_config()

default_port <- 8000
default_host <- "0.0.0.0"

# ---- Custom Swagger/OpenAPI info (TITLE, DESCRIPTION, etc.) ----
api_info <- make_api_info(
  title        = Sys.getenv("API_TITLE",        "R Plotting Server for FFRD"),
  description  = Sys.getenv("API_DESCRIPTION",  "API for data visualization"),
  version      = Sys.getenv("API_VERSION",      "0.1.0"),
  terms_url    = Sys.getenv("API_TERMS_URL",    ""),
  contact_name = Sys.getenv("API_CONTACT_NAME", "Melissa Mika"),
  # contact_email= Sys.getenv("API_CONTACT_EMAIL",""),
  license_name = Sys.getenv("API_LICENSE_NAME", "MIT")
)

# ---- Working dir / script path ----
script_path <- get_script_path()
setwd(normalizePath(file.path(script_path, "server"), mustWork = FALSE))

# ---- Log cleanup ----
cleanup_old_logs <- function(log_dir = "logs", days_to_keep = 30) {
  if (!dir.exists(log_dir)) return()

  cutoff_date <- Sys.Date() - days_to_keep
  log_files <- list.files(log_dir, pattern = "\\.(log|log\\.[0-9]+)$", full.names = TRUE)

  for (file in log_files) {
    file_info <- file.info(file)
    if (!is.na(file_info$mtime) && as.Date(file_info$mtime) < cutoff_date) {
      unlink(file)
      message(sprintf("Cleaned up old log file: %s", basename(file)))
    }
  }
}

# Clean up old logs on startup
cleanup_old_logs()

# ---- Schemas ----
SCHEMAS_DIR <- normalizePath(file.path(script_path, "server", "api", "schemas"), mustWork = FALSE)
SCHEMA_VALIDATORS <- new.env(parent = emptyenv())
load_schema_validators(SCHEMAS_DIR, SCHEMA_VALIDATORS)

# ---- App + routes ----
pr <- plumber::pr()
DEV_MODE <- isTRUE(interactive()) || isTRUE(as.logical(Sys.getenv("PLUMBER_DEV", "TRUE")))

route_files <- collect_route_files(DEV_MODE)
message(sprintf("[plumber] DEV_MODE=%s | script_path=%s | #routes=%d",
                DEV_MODE, script_path, length(route_files)))
pr <- mount_routes(pr, route_files)

# ---- Request/Response logging as JSON (no payloads) ----
if (identical(tolower(Sys.getenv("LOG_HTTP", "true")), "true")) {

  .now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

  .safe_str <- function(x, max_bytes = 200) {
    if (is.null(x)) return("-")
    s <- tryCatch(as.character(x)[1], error = function(e) "-")
    if (!nzchar(s)) return("-")
    n <- nchar(s, type = "bytes")
    if (n > max_bytes) substr(s, 1, max_bytes) else s
  }

  .log_json <- function(obj) {
    pretty <- identical(tolower(Sys.getenv("LOG_HTTP_PRETTY", "false")), "true")
    txt <- jsonlite::toJSON(
      obj,
      auto_unbox = TRUE,
      null = "null",
      POSIXt = "ISO8601",
      digits = NA,
      pretty = pretty
    )

    # Output to console (for docker logs)
    cat(as.character(txt), "\n")

    # Also log to file (if logs directory exists)
    log_dir <- "logs"
    if (dir.exists(log_dir)) {
      log_file <- file.path(log_dir, paste0("http-", Sys.Date(), ".log"))

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

      cat(as.character(txt), "\n", file = log_file, append = TRUE)
    }
  }

  .gen_req_id <- function() {
    paste0(
      as.integer(runif(1, 0, .Machine$integer.max)),
      "-",
      paste(sprintf("%02x", sample(0:255, 8, TRUE)), collapse = "")
    )
  }

  pr <- plumber::pr_hooks(pr, list(
    preroute = function(req, res) {
      req$.__start_time <- Sys.time()
      req$.__req_id     <- .gen_req_id()

      method <- .safe_str(req$REQUEST_METHOD)
      path   <- .safe_str(if (!is.null(req$PATH_INFO) && nzchar(req$PATH_INFO)) req$PATH_INFO else "/")
      qs     <- req$QUERY_STRING
      if (!is.null(qs) && nzchar(qs)) path <- paste0(path, "?", qs)

      ip <- if (!is.null(req$HTTP_X_FORWARDED_FOR) && nzchar(req$HTTP_X_FORWARDED_FOR)) {
        req$HTTP_X_FORWARDED_FOR
      } else if (!is.null(req$REMOTE_ADDR) && nzchar(req$REMOTE_ADDR)) {
        req$REMOTE_ADDR
      } else {
        "-"
      }

      ua <- .safe_str(req$HTTP_USER_AGENT, max_bytes = 200)

      # Skip internal assets if desired
      if (!grepl("^/(__docs__|favicon\\.ico)", path)) {
        .log_json(list(
          ts        = .now_utc(),
          event     = "request",
          id        = req$.__req_id,
          method    = method,
          path      = path,
          ip        = ip
          # userAgent = ua
        ))
      }
    },
    postserialize = function(req, res, value) {
      dur <- tryCatch(
        as.numeric(difftime(Sys.time(), req$.__start_time, units = "secs")),
        error = function(e) NA_real_
      )

      method <- .safe_str(req$REQUEST_METHOD)
      path   <- .safe_str(if (!is.null(req$PATH_INFO) && nzchar(req$PATH_INFO)) req$PATH_INFO else "/")
      status <- if (!is.null(res$status)) res$status else 200L

      .log_json(list(
        ts         = .now_utc(),
        event      = "response",
        id         = if (!is.null(req$.__req_id)) req$.__req_id else NA_character_,
        method     = method,
        path       = path,
        status     = status,
        duration_s = round(dur, 3)
      ))

      value  # IMPORTANT: return the serialized value unchanged
    }
  ))
}


servers <- list(
  list(url = "/", description = "Relative base (reverse-proxy / container)"),
  list(url = paste0("http://", Sys.getenv("HOST", default_port), ":", Sys.getenv("PORT", default_port)),
       description = "Direct host")
)

# One place to customize the spec:
pr <- plumber::pr_set_api_spec(pr, function(spec) {
  spec <- add_info_to_spec(spec, api_info)
  spec <- add_servers_to_spec(spec, servers)
  if (DEV_MODE) {
    spec <- add_dev_tag_to_spec(spec)
    spec <- tag_dev_operations(spec)
  } else {
    spec <- remove_dev_tag_from_spec(spec)
  }
  spec
})

# ---- Start / return ----
if (interactive()) {
  host <- Sys.getenv("HOST", default_host)
  port <- as.integer(Sys.getenv("PORT", default_port))
  plumber::pr_run(pr, host = host, port = port)
} else {
  pr
}
