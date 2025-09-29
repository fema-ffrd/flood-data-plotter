# routes/flows.R
library(plumber)
library(jsonlite)

source("../../src/plotting/realization_flow.R")

#* Create Flow Plot in HTML or JSON
#* @post /flows
function(req, res) {
  create_plot_route(realization_flow_summary_plot, "realization_flow", req, res)
}