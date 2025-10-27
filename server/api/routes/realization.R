# routes/realization
library(plumber)
library(jsonlite)

source("../../src/plotting/realization_flow.R")
source("../../src/plotting/realization_stage.R")
source("../../src/plotting/realization_stage_discharge.R")

#* Create Flow Plot in HTML or JSON
#* @post /flows
function(req, res) {
  create_plot_route(realization_flow_summary_plot, "realization_flow", req, res)
}

#* Create Stage Plot in HTML or JSON
#* @post /stage
function(req, res) {
  create_plot_route(realization_stage_summary_plot, "realization_stage", req, res)
}

#* Create Stage Plot in HTML or JSON
#* @post /stage_discharge
function(req, res) {
  create_plot_route(realization_stage_discharge_summary_plot, "realization_stage_discharge", req, res)
}