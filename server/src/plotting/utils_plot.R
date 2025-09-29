library(here)
library(fs)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(webshot2)
library(jsonlite)
library(listviewer)

plot_to_html <- function(plot) {
  # write to a temporary self-contained html, read it back, optionally save to html_file,
  # normalize newlines to LF only, and remove any temporary files/folders
  tmp <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(ggplotly(plot), tmp, selfcontained = TRUE)

  # read raw html and normalize CRLF -> LF to avoid literal "\r\n" escape sequences later
  html <- readChar(tmp, nchars = file.info(tmp)$size, useBytes = TRUE)
  html <- gsub("\r\n", "\n", html, fixed = TRUE)
  html <- paste(html, collapse = "")          # ensure single string
  Encoding(html) <- "UTF-8"                   # mark encoding explicitly

  # remove temporary file
  if (file.exists(tmp)) unlink(tmp)

  return(as.character(html))
}

plot_to_json <- function(plot) {
  return(as.character(plotly_json(ggplotly(plot), jsonedit = FALSE)))
}

get_realization_df <- function(flows, real) {
  df <- NULL

  if (is.data.frame(flows) && !"flows" %in% names(flows)) {
    df <- tryCatch(dplyr::filter(flows, Realization == real), error = function(e) NULL)

  } else if (is.data.frame(flows) && "flows" %in% names(flows)) {
    row <- tryCatch(flows[which(as.character(flows$realization) == as.character(real))[1], , drop = FALSE],
                    error = function(e) NULL)
    if (!is.null(row) && nrow(row) == 1 && length(row$flows[[1]]) > 0) {
      df <- do.call(rbind, lapply(row$flows[[1]], function(flow) as.data.frame(flow, stringsAsFactors = FALSE)))
      # add Realization/Block/Event if missing
      if (!"Realization" %in% names(df)) df$Realization <- real
    }

  } else if (is.list(flows)) {
    for (entry in flows) {
      if (is.list(entry) && !is.null(entry$realization) && identical(as.character(entry$realization), as.character(real))) {
        df <- do.call(rbind, lapply(entry$flows, function(flow) as.data.frame(flow, stringsAsFactors = FALSE)))
        if (!"Realization" %in% names(df)) df$Realization <- real
        break
      }
    }
  }

  if (!is.data.frame(df)) return(NULL)

  # Ensure df is a proper data.frame (not a list-column or weird vector) and reset rownames
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  rownames(df) <- NULL

  # If Z exists, order by Z descending so rows follow desired plotting order (1,3,2 in your example)
  if ("Z" %in% names(df)) {
    znum <- suppressWarnings(as.numeric(df$Z))
    # preserve NA handling, order decreasing (highest Z first)
    df <- df[order(znum, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    rownames(df) <- NULL
  }

  if (exists("filter_trace_data", mode = "function")) {
    df2 <- tryCatch(df %>% filter_trace_data(threshold = 2), error = function(e) df)
    if (is.data.frame(df2)) return(df2)
  }

  df
}

get_obs_df <- function(obs_flows) {
  if (!is.list(obs_flows)) {
    stop("Input must be a list of lists")
  }
  df <- do.call(rbind, lapply(obs_flows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  df$Date <- as.POSIXct(df$Date, format = "%d %b %Y, %H:%M", tz = "UTC")

  return(df)
}

get_obs_cl_df <- function(obs_flows) {
  if (!is.list(obs_flows)) {
    stop("Input must be a list of lists")
  }
  df <- do.call(rbind, lapply(obs_flows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  return(df)
}

validate_json_body <- function(body,
                               schema,                 # schema as R object (list) or JSON text (string)
                               engine = "ajv") {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    stop("Package 'jsonvalidate' is required for schema validation. install.packages('jsonvalidate')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. install.packages('jsonlite')")
  }

  # prepare JSON text from body (accept either raw JSON string or an R object)
  json_text <- NULL
  if (is.character(body) && length(body) == 1) {
    json_text <- body
  } else {
    # convert R object to JSON
    json_text <- tryCatch({
      jsonlite::toJSON(body, auto_unbox = TRUE, pretty = FALSE)
    }, error = function(e) {
      stop("Failed to convert R object 'body' to JSON: ", conditionMessage(e))
    })
  }

  # prepare schema text: accept R list (converted to JSON) or JSON schema string
  schema_text <- NULL
  if (is.list(schema)) {
    schema_text <- tryCatch({
      jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = FALSE)
    }, error = function(e) {
      stop("Failed to convert R object 'schema' to JSON schema text: ", conditionMessage(e))
    })
  } else if (is.character(schema) && length(schema) == 1 && grepl("^[ \\t\\r\\n]*\\{", schema)) {
    schema_text <- schema
  } else {
    stop("Schema must be provided as an R list (parsed schema) or a JSON schema string.")
  }

  # validate (pass schema as JSON text; jsonvalidate accepts schema text)
  valid <- tryCatch({
    jsonvalidate::json_validate(json_text, schema = schema_text, engine = engine, verbose = TRUE)
  }, error = function(e) {
    return(structure(FALSE, errors = paste0("schema/engine error: ", conditionMessage(e))))
  })

  # collect errors if present
  errors <- attr(valid, "errors")
  if (!isTRUE(valid)) {
    if (is.null(errors)) errors <- "Document is not valid according to schema (no details returned)."
    return(list(valid = FALSE, errors = as.character(errors), parsed = NULL))
  }

  # parse JSON to R object for convenience
  parsed <- tryCatch({
    jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  }, error = function(e) {
    return(structure(NULL, parse_error = conditionMessage(e)))
  })

  return(list(valid = TRUE, errors = NULL, parsed = parsed))
}

save_html_to_png <- function(html_path,
                             png_path,
                             selector = NULL,
                             width = 1280,
                             height = 800,
                             scale = 2,
                             delay = 0.5,
                             quiet = TRUE) {
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("PNG output requested but the 'webshot2' package is not installed.")
  }
  html_abs <- normalizePath(html_path, winslash = "/", mustWork = TRUE)

  # Build a proper file:// URL (3 slashes on both POSIX and Windows)
  file_url <- if (.Platform$OS.type == "windows") {
    paste0("file:///", gsub("\\\\", "/", html_abs))
  } else {
    paste0("file://", html_abs)  # html_abs starts with '/', yields file:///...
  }

  out_dir <- dirname(png_path)
  if (!dir.exists(out_dir) && nzchar(out_dir)) dir.create(out_dir, recursive = TRUE)

  run_webshot <- function() {
    webshot2::webshot(
      url      = file_url,
      file     = png_path,
      vwidth   = width,
      vheight  = height,
      zoom     = scale,
      delay    = delay,
      selector = selector
    )
  }

  if (quiet) {
    # Suppress stdout, messages, and warnings from webshot/chromote
    invisible(capture.output(
      withCallingHandlers(
        suppressWarnings(suppressMessages(run_webshot())),
        message = function(m) invokeRestart("muffleMessage")
      ),
      type = "output"
    ))
  } else {
    run_webshot()
  }

  invisible(normalizePath(png_path, winslash = "/"))
}

# ---- main client: now with optional PNG output ----
make_plot_request <- function(file_path,
                              url,
                              output_file_path,
                              output_png_path = NULL,     # set a path to also save PNG
                              png_selector    = NULL,     # CSS selector for the plot element
                              png_width       = 1280,
                              png_height      = 800,
                              png_scale       = 2,
                              png_delay       = 0.5,      # seconds
                              timeout_sec     = 60) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the 'httr' and 'jsonlite' packages.")
  }

  # Load payload
  json_data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)

  # POST
  resp <- tryCatch({
    httr::POST(
      url,
      httr::add_headers(`Content-Type` = "application/json"),
      body   = json_data,
      encode = "json",
      httr::timeout(timeout_sec)
    )
  }, error = function(e) stop("HTTP request failed: ", conditionMessage(e)))

  # Response text (save even on error for debugging)
  html_content <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"),
                           error = function(e) "")

  # Ensure output dir and write HTML
  out_dir <- dirname(output_file_path)
  if (!dir.exists(out_dir) && nzchar(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  writeLines(html_content, con = output_file_path, useBytes = TRUE)
  # message("HTML saved to: ", normalizePath(output_file_path, winslash = "/"))

  # If server error, stop (HTML already saved)
  if (httr::http_error(resp)) {
    status <- httr::status_code(resp)
    text_snippet <- substr(html_content, 1, 2000)
    stop(sprintf("Server returned HTTP %d. Response body: %s", status, text_snippet))
  }

  # Optional PNG render
  png_result <- NULL
  
  # Check if PNG is requested but JSON output is requested or not specified
  if (!is.null(output_png_path) && (is.null(json_data$json) || isTRUE(json_data$json))) {
    message("PNG files are only exported if the HTML output file format is specified.")
  }
  
  if (!is.null(output_png_path) && identical(json_data$json, FALSE)) {
    png_result <- tryCatch(
      {
        save_html_to_png(
          html_path = output_file_path,
          png_path  = output_png_path,
          selector  = png_selector,
          width     = png_width,
          height    = png_height,
          scale     = png_scale,
          delay     = png_delay
        )
      },
      error = function(e) {
        warning("PNG export failed: ", conditionMessage(e))
        NULL
      }
    )
    # if (!is.null(png_result)) {
    #   message("PNG saved to: ", png_result)
    # }
  }

  invisible(list(response = resp, html = html_content, png = png_result))
}
