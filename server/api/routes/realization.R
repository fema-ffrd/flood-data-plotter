# routes/realization
library(plumber)
library(jsonlite)

source("../../src/plotting/realization_flow.R")
source("../../src/plotting/realization_stage.R")

#* Create Flow Plot in HTML or JSON
#* @post /flows
function(req, res) {
  create_plot_route(realization_flow_summary_plot, "realization_flow", req, res)
}

#* Create Stage Plot in HTML or JSON
#* @post /stage
function(req, res) {
  create_plot_route(realization_flow_summary_plot, "realization_stage", req, res)
}