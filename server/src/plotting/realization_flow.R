library(here)
library(fs)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(webshot) #to save to png
library(jsonlite)

source(here("src/plotting/utils_plot.R"))

realization_flow_summary_plot <- function(
  shortname,
  ras_element,
  hms_element,
  usgs_id,
  duration,
  interval,
  json = TRUE,
  xmin = -3.7,
  xmax = 0,
  plot_tick_limits = c(2.326,
    1.28,
    0.675,
    0,
    -0.842,
    -1.282,
    -1.751,
    -2.054,
    -2.326,
    -2.576,
    -2.878,
    -3.291,
    -3.719,
    -4.265),
  plot_tick_labels = c("0.99",
    "0.9",
    "0.75",
    "0.5",
    "0.2",
    "0.1",
    "0.04",
    "0.02",
    "0.01",
    "0.005",
    "0.002",
    "5e-4",
    "1e-4",
    "1e-5"),
  plot_tick_labels_rp = c("1.01-yr",
    "1.11-yr",
    "1.33-yr",
    "2-yr",
    "5-yr",
    "10-yr",
    "25-yr",
    "50-yr",
    "100-yr",
    "200-yr",
    "500-yr",
    "2,000-yr",
    "10,000-yr",
    "100,000-yr"),
  ras_conf_limits,
  hms_conf_limits,
  ras_flows,
  hms_flows,
  ressim_flows,
  ras_color = "blue",
  hms_color = "green",
  ressim_color = "purple",
  ras_cl_color = "darkblue",
  hms_cl_color = "darkgreen",
  obs_flows,
  obs_conf_limits,
  obs_color = "black") {
  
  
  # Dynamically determine the min and max y-axis values
  # Combine all stage values from all sources
  all_flow_values <- c(
    unlist(lapply(ras_flows, function(x) sapply(x$flows, function(f) f$value))),
    unlist(lapply(hms_flows, function(x) sapply(x$flows, function(f) f$value))),
    unlist(lapply(ressim_flows, function(x) sapply(x$flows, function(f) f$value))),
    if (!is.null(obs_flows)) obs_flows$value else numeric(0)
  )
  # Remove NA and non-positive values
  all_flow_values <- all_flow_values[!is.na(all_flow_values) & all_flow_values > 0]
  
  # Set log y-axis limits
  log_y_min <- log10(min(all_flow_values)) * 0.9
  log_y_max <- log10(max(all_flow_values)) * 1.1
  
  # build base figure + layout
  fig <- plot_ly() %>%
    plotly::layout(
      title = list(
        text = paste0(
          "Flow Frequency Plot - All Realizations<br>",
          "FFRD Scenario: <span style='color:red; font-weight:bold; font-style:italic;'>", "Production", "</span><br>",
          "USGS ID: ", usgs_id, "<br>",
          "RAS Refline Name: ", ras_element, "<br>",
          "HMS/ResSim Name: ", if (!is.null(hms_element)) hms_element else "NA", "<br>",
          "Duration: ", duration, " ", interval
        ),
        x = 0.07, y = 0.95, xanchor = "left", yanchor = "top",
        font = list(size = 18, color = "black")
      ),
      xaxis = list(title = "AEP", range = c(xmax, xmin), showgrid = FALSE,
                   tickvals = plot_tick_limits, ticktext = plot_tick_labels,
                   tickfont = list(size = 20, weight = "1000")),
      xaxis2 = list(overlaying = "x", side = "top", anchor = "y",
                    title = "Return Period", tickvals = plot_tick_limits,
                    ticktext = plot_tick_labels_rp, range = c(xmax, xmin),
                    gridcolor = "darkgrey", tickfont = list(size = 20, weight = "1000")),
      yaxis = list(title = "Discharge", type = "log", range = c(log_y_min, log_y_max),
                   autorange = FALSE, titlefont = list(size = 20, weight = "bold"),
                   tickfont = list(size = 16, family = "Arial"), standoff = 30, showgrid = TRUE),
      margin = list(l = 100, r = 100, t = 200, b = 100),
      paper_bgcolor = "white", plot_bgcolor = "lightgrey", showlegend = TRUE,
      shapes = list(list(type = "rect", xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
                         line = list(color = "black", width = 2))),
      dragmode = FALSE
    )

  # summary quantiles (guard missing lists)
  ras_conf_limits <- get_obs_cl_df(ras_conf_limits)
  if (!is.null(ras_conf_limits) && length(ras_conf_limits) > 0) {
    fig <- fig %>%
      add_trace(x = ras_conf_limits$Z, y = ras_conf_limits$median, type = 'scatter', mode = 'lines',
                name = "RAS Median", xaxis = "x2", line = list(dash = 'dash', color = ras_cl_color, width = 3)) %>%
      add_trace(x = ras_conf_limits$Z, y = ras_conf_limits$p95, type = 'scatter', mode = 'lines',
                name = "RAS 95th Percentile", xaxis = "x2", line = list(dash = 'dash', color = ras_cl_color, width = 3)) %>%
      add_trace(x = ras_conf_limits$Z, y = ras_conf_limits$p05, type = 'scatter', mode = 'lines',
                name = "RAS 5th Percentile", xaxis = "x2", line = list(dash = 'dash', color = ras_cl_color, width = 3)) %>%
      add_trace(x = ras_conf_limits$Z, y = ras_conf_limits$mean, type = 'scatter', mode = 'lines',
                name = "RAS Mean", xaxis = "x2", line = list(color = "black", width = 3))
  }

  hms_conf_limits <- get_obs_cl_df(hms_conf_limits)
  if (!is.null(hms_conf_limits) && length(hms_conf_limits) > 0) {
    fig <- fig %>%
      add_trace(x = hms_conf_limits$Z, y = hms_conf_limits$median, type = 'scatter', mode = 'lines',
                name = "HMS Median", xaxis = "x2", line = list(dash = 'dash', color = hms_cl_color, width = 3)) %>%
      add_trace(x = hms_conf_limits$Z, y = hms_conf_limits$p95, type = 'scatter', mode = 'lines',
                name = "HMS 95th Percentile", xaxis = "x2", line = list(dash = 'dash', color = hms_cl_color, width = 3)) %>%
      add_trace(x = hms_conf_limits$Z, y = hms_conf_limits$p05, type = 'scatter', mode = 'lines',
                name = "HMS 5th Percentile", xaxis = "x2", line = list(dash = 'dash', color = hms_cl_color, width = 3)) %>%
      add_trace(x = hms_conf_limits$Z, y = hms_conf_limits$mean, type = 'scatter', mode = 'lines',
                name = "HMS Mean", xaxis = "x2", line = list(color = "black", width = 3))
  }
  
  # Get the realizations for each source
  ras_realizations <- lapply(ras_flows, function(x) x$realization)
  hms_realizations <- lapply(hms_flows, function(x) x$realization)
  ressim_realizations <- lapply(ressim_flows, function(x) x$realization)
  
  # Flatten to vectors
  ras_realizations_vec <- unique(unlist(ras_realizations))
  hms_realizations_vec <- unique(unlist(hms_realizations))
  ressim_realizations_vec <- unique(unlist(ressim_realizations))
  
  # Check for differences
  if (!(setequal(ras_realizations_vec, hms_realizations_vec) &&
        setequal(ras_realizations_vec, ressim_realizations_vec) &&
        setequal(hms_realizations_vec, ressim_realizations_vec))) {
    message("Warning: Realization sets differ between RAS, HMS, and ResSim.")
  }
  
  # List all of the realizations
  all_realizations <- c(
    ras_realizations_vec, 
    hms_realizations_vec, 
    ressim_realizations_vec
  )
  
  # realization traces (works for named-list or data.frame inputs)
  for (real in all_realizations) {
    # message("Realization ", real)
    df_ras <- get_realization_df(ras_flows, real)
    # message(df_ras)
    if (!is.null(df_ras) && nrow(df_ras) > 0) {
      fig <- fig %>% add_trace(
        x = df_ras$Z, y = df_ras$value, type = 'scatter', mode = 'lines',
        legendgroup = "RAS Realizations", name = "RAS Realizations",
        hovertext = paste0("Value: ",ifelse(is.numeric(df_ras$value), round(df_ras$value, 1), "Non-numeric value"),
                           "<br>Realization: ", df_ras$Realization,
                           "<br>Bhms_elementk: ", df_ras$Bhms_elementk,
                           "<br>Event: ", df_ras$Event),
        hoverinfo = "text", xaxis = "x",
        line = list(color = ras_color, width = 1.5, opacity = 0.5),
        showlegend = if (identical(as.character(real), as.character(all_realizations[1])) ) TRUE else FALSE
      )
    }

    # HMS
    df_hms <- get_realization_df(hms_flows, real)
    if (!is.null(df_hms) && nrow(df_hms) > 0) {
      fig <- fig %>% add_trace(
        x = df_hms$Z, y = df_hms$value, type = 'scatter', mode = 'lines',
        legendgroup = "HMS Realizations", name = "HMS Realizations",
        hovertext = paste0("Value: ",ifelse(is.numeric(df_hms$value), round(df_hms$value, 1), "Non-numeric value"),
                           "<br>Realization: ", df_hms$Realization,
                           "<br>Bhms_elementk: ", df_hms$Bhms_elementk,
                           "<br>Event: ", df_hms$Event),
        hoverinfo = "text", xaxis = "x",
        line = list(color = hms_color, width = 1.5, opacity = 0.5),
        showlegend = if (identical(as.character(real), as.character(all_realizations[1])) ) TRUE else FALSE
      )
    }

    # ResSim
    df_ressim <- get_realization_df(ressim_flows, real)
    if (!is.null(df_ressim) && nrow(df_ressim) > 0) {
      fig <- fig %>% add_trace(
        x = df_ressim$Z, y = df_ressim$value, type = 'scatter', mode = 'lines',
        legendgroup = "ResSim Realizations", name = "ResSim Realizations",
        hovertext = paste0("Value: ",ifelse(is.numeric(df_ressim$value), round(df_ressim$value, 1), "Non-numeric value"),
                           "<br>Realization: ", df_ressim$Realization,
                           "<br>Bhms_elementk: ", df_ressim$Bhms_elementk,
                           "<br>Event: ", df_ressim$Event),
        hoverinfo = "text", xaxis = "x",
        line = list(color = ressim_color, width = 1.5, opacity = 0.5),
        showlegend = if (identical(as.character(real), as.character(all_realizations[1])) ) TRUE else FALSE
      )
    }
  }

  # Observed traces
  obs_flows <- get_obs_df(obs_flows)
  if (!is.null(obs_flows) && length(obs_flows) > 0) {
    fig <- fig %>%
      add_trace(x = obs_flows$Z, y = obs_flows$Flow, type = 'scatter', mode = 'markers',
                name = "Observed", legendgroup = "Observed", xaxis = "x2",
                hovertext = paste0("Discharge: ", ifelse(is.numeric(obs_flows$Flow), round(obs_flows$Flow, 1), "Non-numeric value"), "<br>Date: ", obs_flows$Date),
                hoverinfo = "text", marker = list(symbol = 'circle-open', color = obs_color, size = 7))
  }

  obs_conf_limits <- get_obs_cl_df(obs_conf_limits)
  if (!is.null(obs_conf_limits) && length(obs_conf_limits) > 0) {
    fig <- fig %>%
      add_trace(x = obs_conf_limits$z, y = obs_conf_limits$Lower, type = 'scatter', mode = 'lines',
                name = "Confidence Limits", line = list(dash = 'dash', color = obs_color),
                xaxis = "x2", showlegend = FALSE, paste0("Value: ",ifelse(is.numeric(obs_conf_limits$Lower), round(obs_conf_limits$Lower, 1), "Non-numeric value")),
                hoverinfo = "text") %>%
      add_trace(x = obs_conf_limits$z, y = obs_conf_limits$Upper, type = 'scatter', mode = 'lines',
                name = "Confidence Limits", line = list(dash = 'dash', color = obs_color),
                xaxis = "x2", showlegend = FALSE, paste0("Value: ",ifelse(is.numeric(obs_conf_limits$Upper), round(obs_conf_limits$Upper, 1), "Non-numeric value")),
                hoverinfo = "text")
  }

  # Output either a json (default) or an html depending on the user-specification
  if (json){
    return(plot_to_json(fig))
  } else{
    return(plot_to_html(fig))
  }
}
