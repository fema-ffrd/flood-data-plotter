# routes/schemas.R
# Lists schemas and returns a specific schema
# Mounted at: /api/schemas

library(plumber)
library(jsonlite)

#* List all available JSON Schemas
#* @get /
#* @serializer json
function(req, res) {
  if (!exists("available_schemas", mode = "function")) {
    res$status <- 500
    return(list(message = "available_schemas() not found; ensure plumber.R defines it before mounting routes."))
  }
  if (!exists("SCHEMA_VALIDATORS", mode = "environment")) {
    res$status <- 500
    return(list(message = "SCHEMA_VALIDATORS env not found; ensure plumber.R initializes it before mounting routes."))
  }
  
  keys <- available_schemas()
  items <- lapply(keys, function(k) {
    entry <- mget(k, envir = SCHEMA_VALIDATORS, ifnotfound = list(NULL))[[1]]
    fp <- if (!is.null(entry)) entry$file else NA_character_
    meta <- tryCatch(jsonlite::read_json(fp, simplifyVector = FALSE), error = function(e) NULL)
    
    list(
      name = k,
      file = basename(fp),
      title = if (!is.null(meta$title)) meta$title else NULL,
      id = if (!is.null(meta[["$id"]])) meta[["$id"]] else NULL,
      schema = if (!is.null(meta[["$schema"]])) meta[["$schema"]] else NULL,
      description = if (!is.null(meta$description)) meta$description else NULL
    )
  })
  
  list(count = length(keys), schemas = items)
}

#* Return a specific JSON Schema by name
#* @get /<name>
#* @serializer json
function(name, req, res) {
  if (!exists("SCHEMA_VALIDATORS", mode = "environment")) {
    res$status <- 500
    return(list(message = "SCHEMA_VALIDATORS env not found; ensure plumber.R initializes it before mounting routes."))
  }
  
  entry <- mget(name, envir = SCHEMA_VALIDATORS, ifnotfound = list(NULL))[[1]]
  if (is.null(entry)) {
    res$status <- 404
    return(list(
      message = sprintf("Schema '%s' not found.", name),
      available = sort(ls(SCHEMA_VALIDATORS))
    ))
  }
  
  # Read and return the parsed schema; plumber will JSON-encode it
  doc <- tryCatch(jsonlite::read_json(entry$file, simplifyVector = FALSE), error = identity)
  if (inherits(doc, "error")) {
    res$status <- 500
    return(list(message = "Failed to read schema file.", error = conditionMessage(doc)))
  }
  
  res$setHeader("Content-Type", "application/schema+json; charset=utf-8")
  doc
}
