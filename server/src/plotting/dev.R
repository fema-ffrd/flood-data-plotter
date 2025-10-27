rm(list = ls())

library(here)
library(fs)
library(tidyverse)
library(jsonlite)


source(file.path(getwd(), "src/plotting/utils_plot.R"))




# Test plot for stage-discharge
# -----------------------------
#make sure to set json=false in the input json and you should be all set
source(file.path(getwd(), "src/plotting/realization_stage_discharge.R"))

json_file_path <- file.path(getwd(), "src/example-jsons/stage-discharge-example.json")
html_file_path <- file.path(getwd(), "test_output/stage-discharge-example.html")

input_data <- fromJSON(json_file_path, simplifyVector = FALSE)

html_output <- do.call(realization_stage_discharge_summary_plot, input_data)

writeLines(html_output, html_file_path)




# Test plot for flow
# ------------------
#make sure to set json=false in the input json and you should be all set

source(file.path(getwd(), "src/plotting/realization_flow.R"))

flow_json_file_path <- file.path(getwd(), "src/example-jsons/flow-example.json")
flow_html_file_path <- file.path(getwd(), "src/example-jsons/flow-example.html")

flow_input_data <- fromJSON(flow_json_file_path, simplifyVector = FALSE)

html_output <- do.call(realization_flow_summary_plot, flow_input_data)

writeLines(html_output, html_file_path)




# Troubleshoot reading in data with get_obs_df
# --------------------------------------------
#flow_input_data <- fromJSON(flow_json_file_path, simplifyVector = FALSE)
input_data <- fromJSON(json_file_path, simplifyVector = FALSE)
source(file.path(getwd(), "src/plotting/utils_plot.R"))

get_obs_cl_df(input_data$ras_single_event)







# Troubleshoot reading in data with get_realization_df
# ----------------------------------------------------
source(file.path(getwd(), "src/plotting/utils_plot.R"))
get_realization_df_sd(input_data$ffrd_ras_event_peaks, 1)
get_realization_df(flow_input_data$ras_flows, 1)














# # WORKSPACE
# # ---------
# source("C:/Users/kyle.oneil/HOME/PROGRAMMING/Repositories/flood-data-plotter/server/src/plotting/realization_flow.R")
# input_file <- "C:/Users/kyle.oneil/HOME/PROGRAMMING/Repositories/flood-data-plotter/server/src/example-jsons/flow-example.json"
# json_body <- jsonlite::fromJSON(input_file, simplifyVector = FALSE)
# plot <- do.call(realization_flow_summary_plot, json_body)
# html_file <- "C:/Users/kyle.oneil/HOME/PROGRAMMING/Repositories/flood-data-plotter/server/src/example-jsons/flow-example-out.html"
# plot_json <- fromJSON(plot)
# new_plot <- plot_ly(data = plot_json$data) %>% layout(plot_json$layout)
# htmlwidgets::saveWidget(new_plot, html_file, selfcontained = TRUE)

