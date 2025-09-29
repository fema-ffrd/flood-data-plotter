#* Health check
#* @get /status
function() {
  list(status = "OK", timestamp = Sys.time())
}