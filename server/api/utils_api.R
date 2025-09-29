# Null-coalescing helper for strings
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}

# ---- Small utils ----
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Path helpers ----
get_script_path <- function() {
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable() &&
        interactive()) {
      p <- rstudioapi::getActiveDocumentContext()$path
      if (is.character(p) && nzchar(p)) dirname(p) else getwd()
    } else {
      getwd()
    }
  }, error = function(e) getwd())
}

ensure_final_newline <- function(path) {
  sz <- file.info(path)$size
  if (is.na(sz) || sz == 0) return(invisible())
  con <- file(path, open = "rb"); on.exit(close(con), add = TRUE)
  seek(con, where = sz - 1L, origin = "start")
  last <- readBin(con, what = "raw", n = 1L)
  if (!identical(last, as.raw(0x0A))) {
    con2 <- file(path, open = "ab"); on.exit(close(con2), add = TRUE)
    writeBin(as.raw(0x0A), con2)
  }
}

# ---- Schema helpers (each does one thing) ----
schema_endpoint_for <- function(name) {
  paste0("/api/schemas/", utils::URLencode(name, reserved = TRUE))
}

format_ajv_errors <- function(err_df, limit = 25) {
  if (!is.data.frame(err_df) || nrow(err_df) == 0) return(list())
  n <- min(nrow(err_df), limit)
  lapply(seq_len(n), function(i) {
    row <- err_df[i, , drop = FALSE]
    kw  <- if ("keyword"      %in% names(row)) as.character(row$keyword)      else NA_character_
    msg <- if ("message"      %in% names(row)) as.character(row$message)      else "invalid"
    ip  <- if ("instancePath" %in% names(row)) as.character(row$instancePath) else ""
    dp  <- if ("dataPath"     %in% names(row)) as.character(row$dataPath)     else ""
    path <- if (nzchar(ip)) ip else dp

    if (identical(kw, "required") && "params" %in% names(row)) {
      p <- row$params[[1]]
      if (is.list(p) && !is.null(p$missingProperty)) {
        mp <- as.character(p$missingProperty)
        if (nzchar(mp)) {
          sep <- if (nzchar(path) && substring(path, nchar(path)) != "/") "/" else ""
          path <- paste0(path, sep, mp)
        }
      }
    }
    if (nzchar(path) && !startsWith(path, "/")) {
      path <- sub("^\\.", "/", path)
      path <- gsub("\\.", "/", path)
    }
    if (!nzchar(path)) path <- "/"

    list(path = path, keyword = kw, detail = msg)
  })
}

load_schema_validators <- function(dir, env) {
  if (!dir.exists(dir)) {
    warning(sprintf("Schema directory '%s' does not exist; no validators loaded.", dir))
    return(invisible(FALSE))
  }
  schema_files <- list.files(dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
  if (!length(schema_files)) return(invisible(FALSE))

  rm(list = ls(env, all.names = TRUE), envir = env)

  for (f in schema_files) {
    ensure_final_newline(f)
    key <- tools::file_path_sans_ext(basename(f))
    key <- sub("\\.schema$", "", key, perl = TRUE)
    validator <- jsonvalidate::json_validator(f, engine = "ajv")
    assign(key, list(file = f, validator = validator), envir = env)
  }
  invisible(TRUE)
}

available_schemas <- function(env = get0("SCHEMA_VALIDATORS", ifnotfound = NULL, inherits = TRUE)) {
  if (is.null(env) || !is.environment(env)) return(character())
  sort(ls(env))
}


validate_json_payload <- function(
  schema_name,
  json_str,
  env = get0("SCHEMA_VALIDATORS", ifnotfound = NULL, inherits = TRUE),
  max_errors = 25
) {
  if (is.null(env) || !is.environment(env)) {
    stop("SCHEMA_VALIDATORS not found; call load_schema_validators(...) first or pass env explicitly.", call. = FALSE)
  }
  entry <- mget(schema_name, envir = env, ifnotfound = list(NULL))[[1]]
  if (is.null(entry)) {
    return(list(
      ok = FALSE,
      schema_name = schema_name,
      errors = list(list(path = "/", keyword = "schema_not_found",
                         detail = sprintf("Schema '%s' not found", schema_name))),
      hint = "List schemas at GET /api/schemas"
    ))
  }

  ok <- tryCatch(
    entry$validator(json_str, verbose = TRUE, error = FALSE, greedy = TRUE),
    error = function(e) {
      if (grepl("unused argument .*greedy", conditionMessage(e), ignore.case = TRUE)) {
        entry$validator(json_str, verbose = TRUE, error = FALSE)
      } else stop(e)
    }
  )
  if (isTRUE(ok)) return(list(ok = TRUE))

  err_df <- attr(ok, "errors", exact = TRUE)
  formatted <- format_ajv_errors(err_df, limit = max_errors)
  if (!length(formatted)) {
    formatted <- list(list(path = "/", keyword = "unknown", detail = "Validation failed"))
  }
  list(
    ok = FALSE,
    schema_name = schema_name,
    errors = formatted,
    hint = sprintf("See the schema at GET %s", schema_endpoint_for(schema_name))
  )
}

json_error <- function(res, status = 422, obj = list(message = "error")) {
  res$status <- status
  res$setHeader("Content-Type", "application/json; charset=utf-8")
  jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA)
}

ensure_valid_json_or_422 <- function(
  schema_name,
  req,
  res,
  env = get0("SCHEMA_VALIDATORS", ifnotfound = NULL, inherits = TRUE),
  max_errors = 25
) {
  if (is.null(env) || !is.environment(env)) {
    return(json_error(res, 500, list(message = "Server misconfiguration: schema validators not loaded")))
  }
  v <- validate_json_payload(schema_name, req$postBody, env = env, max_errors = max_errors)
  if (!isTRUE(v$ok)) {
    return(json_error(res, 422, list(
      message = "payload failed JSON Schema validation",
      schema_name = schema_name,
      errors = v$errors,
      hint = v$hint
    )))
  }
  NULL
}

# ---- Routing helpers ----
collect_route_files <- function(dev_mode) {
  routes_dir <- "api/routes"
  if (dev_mode) {
    subdirs <- list.dirs(routes_dir, recursive = FALSE, full.names = TRUE)
    c(
      list.files(routes_dir, pattern = "\\.R$", full.names = TRUE, recursive = FALSE),
      unlist(lapply(subdirs, function(d) list.files(d, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)),
             use.names = FALSE)
    )
  } else {
    list.files(routes_dir, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
  }
}

mount_routes <- function(pr, route_files, routes_base = normalizePath("api/routes", mustWork = FALSE)) {
  for (route_file in route_files) {
    nf <- normalizePath(route_file, mustWork = FALSE)
    rel <- fs::path_rel(nf, start = routes_base)
    rel_no_ext <- tools::file_path_sans_ext(rel)
    route_name <- gsub("[\\\\]+", "/", rel_no_ext)
    route_name <- gsub("/{2,}", "/", route_name)
    mount_path <- paste0("/api/", route_name)
    message(sprintf("[plumber] Mounting %s -> %s", route_file, mount_path))
    pr <- plumber::pr_mount(pr, mount_path, plumber::pr(route_file))
  }
  pr
}

# ---- OpenAPI/Swagger spec helpers ----
make_api_info <- function(
  title,
  description = NULL,
  version = "1.0.0",
  terms_url = NULL,
  contact_name = NULL,
  contact_email = NULL,
  license_name = NULL,
  license_url = NULL
) {
  info <- list(title = title, version = version)
  if (length(description)) info$description <- description
  if (length(terms_url))  info$termsOfService <- terms_url
  if (length(contact_name) || length(contact_email)) {
    info$contact <- Filter(Negate(is.null), list(name = contact_name, email = contact_email))
  }
  if (length(license_name) || length(license_url)) {
    info$license <- Filter(Negate(is.null), list(name = license_name, url = license_url))
  }
  info
}

add_info_to_spec <- function(spec, info_list) {
  spec$info <- modifyList(spec$info %||% list(), info_list, keep.null = TRUE)
  spec
}

add_servers_to_spec <- function(spec, servers) {
  spec$servers <- servers
  spec
}

add_dev_tag_to_spec <- function(spec) {
  tags <- spec$tags %||% list()
  have_dev <- any(vapply(tags, function(t) is.list(t) && identical(t$name, "DEV"), logical(1)))
  if (!have_dev) {
    spec$tags <- c(tags, list(list(
      name = "DEV",
      description = "Development endpoints included in /api/dev/."
    )))
  }
  spec
}

remove_dev_tag_from_spec <- function(spec) {
  spec$tags <- Filter(function(t) !identical(t$name, "DEV"), spec$tags %||% list())
  spec
}

tag_dev_operations <- function(spec) {
  if (is.null(spec$paths)) return(spec)
  for (p in names(spec$paths)) {
    if (grepl("^/api/dev(/|$)", p)) {
      for (m in names(spec$paths[[p]])) {
        op <- spec$paths[[p]][[m]]
        tags <- op$tags %||% character()
        spec$paths[[p]][[m]]$tags <- unique(c("DEV", tags))
      }
    }
  }
  spec
}
