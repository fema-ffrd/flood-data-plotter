library(here)
library(fs)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(webshot) #to save to png
library(jsonlite)

source(here("src/plotting/utils_plot.R"))

realization_stage_discharge_summary_plot <- function(
  shortname,
  usgs_id,
  gage_zero,
  gage_datum,
  vert_conversion,
  Historic,
  ShiftDate,
  scenario_name,
  site_name_for_plot,
  json = TRUE,
  ffrd_ras_event_peaks_element,
  ffrd_ras_event_peaks,
  usgs_rating,
  usgs_measurements,
  usgs_annual_peaks,
  calibration_event_results,
  ras_single_event,
  xmin = 0,
  xmax = NULL,
  ymin = 0,
  ymax = NULL,
  ffrd_ras_color = "blue",
  usgs_rating_color = "#555555",
  usgs_measurements_color = "black",
  usgs_annual_peaks_color = "black",
  calibration_event_results_color = "red",
  ras_single_event_color = "darkgrey"
  ) {
  
  
  # Read Data
  # ---------
  # Multiple Realizations
  if (!missing(ffrd_ras_event_peaks) && !is.null(ffrd_ras_event_peaks)) {
    # Get the realizations for each source
    ffrd_ras_realizations <- lapply(ffrd_ras_event_peaks, function(x) x$realization)
    all_realizations <- unique(unlist(ffrd_ras_realizations))
    
    if(length(all_realizations) > 1){
  
      df_ffrd_ras <- data.frame() 
      
      for (real in all_realizations) {
        # message("Realization ", real)
        single_ffrd_df <- get_realization_df_sd(ffrd_ras_event_peaks, real)
        df_ffrd_ras <- bind_rows(df_ffrd_ras, single_ffrd_df)
      }
    }else(
      df_ffrd_ras <- get_realization_df_sd(ffrd_ras_event_peaks, real)
      )
  } else {
    df_ffrd_ras <- NULL
  }
  
  # Single Datasets
  rating_data <- if (!missing(usgs_rating) && !is.null(usgs_rating)) get_obs_cl_df(usgs_rating) else NULL
  meas_data <- if (!missing(usgs_measurements) && !is.null(usgs_measurements)) get_obs_df(usgs_measurements) else NULL
  USGS_peaks_data <- if (!missing(usgs_annual_peaks) && !is.null(usgs_annual_peaks)) get_obs_df(usgs_annual_peaks) else NULL
  calib_event_peaks <- if (!missing(calibration_event_results) && !is.null(calibration_event_results)) get_obs_df(calibration_event_results) else NULL
  RAS_Rating <- if (!missing(ras_single_event) && !is.null(ras_single_event)) get_obs_cl_df(ras_single_event) else NULL
  

  # Dynamic Axes
  # ------------
  # Stage  
  if (is.null(ymax)) {
    all_stage_values <- c(
      df_ffrd_ras$Stage,
      rating_data$Stage, 
      meas_data$Stage, 
      USGS_peaks_data$Stage, 
      calib_event_peaks$Stage,
      RAS_Rating$Stage
      )
    y_axis_range <- all_stage_values[!is.na(all_stage_values)]
    ymax <- max(y_axis_range) * 1.1
  }
  
  # Discharge  
  if (is.null(xmax)) {
    all_discharge_values <- c(
      df_ffrd_ras$Discharge,
      rating_data$Discharge, 
      meas_data$Discharge, 
      USGS_peaks_data$Discharge, 
      calib_event_peaks$Discharge,
      RAS_Rating$Discharge
    )
    x_axis_range <- all_discharge_values[!is.na(all_discharge_values)]
    xmax <- max(x_axis_range) * 1.1  
  }  

  # Link to gage info to be included in plot
  USGS_url <- paste0("https://waterdata.usgs.gov/monitoring-location/",usgs_id)

  # Build base figure + layout
  # --------------------------
  
  # Initialize the annotations list
  annotations <- list(
    list(
      text = paste0(
        "USGS Gage Zero: ", gage_zero, " ft (",
        gage_datum, ")\n",
        "Converted to ft-NAVD88 with factor: Value + ",
        gage_zero, " + ", vert_conversion,
        ifelse(is.na(Historic), 
               " ", 
               paste0("\n---\nAlso Adjusted by ", Historic, 
                      "-ft for data prior to ", format(ShiftDate)))
      ),
      x = 0.95, 
      y = 0.05, 
      xref = "paper", 
      yref = "paper", 
      showarrow = FALSE, 
      font = list(color = "black", size = 12, style = "italic"),
      align = "right",
      valign = "bottom",
      bgcolor = "lightgrey",
      bordercolor = "black",
      borderwidth = 2
    )
  )
  
  # Conditionally add extra annotation
  if (usgs_id == 12113390) {
    annotations <- append(annotations, list(
      list(
        text = "Site is significantly <br>influenced by tides",
        x = 0.95, 
        y = 0.25, 
        xref = "paper", 
        yref = "paper", 
        showarrow = FALSE, 
        font = list(color = "black", size = 12, style = "italic"),
        align = "right",
        valign = "bottom",
        bgcolor = "lightgrey",
        bordercolor = "black",
        borderwidth = 2
      )
    ))
  }
  
  # Plot
  fig <- plot_ly() %>%
    plotly::layout(
      title = list(
        text = paste0(
          "Rating Plot\n",
          "FFRD Scenario: <span style='color:red; font-weight:bold; font-style:italic;'>", scenario_name, "</span>\n",
          paste0(site_name_for_plot, "\n"),
          "Site Page: ", '<a href="', USGS_url, '" target="_blank">', usgs_id, "</a>"
        ),
        x = 0.07, y = 0.95, xanchor = "left", yanchor = "top",
        font = list(size = 20, color = "black")
      ),
      #annotations = annotations,
      xaxis = list(title = "Discharge", range = c(xmin, xmax), showgrid = TRUE),
      yaxis = list(title = "Stage", range = c(ymin, ymax)),
      margin = list(l = 100, r = 100, t = 200, b = 100),
      paper_bgcolor = "white",
      plot_bgcolor = "lightgrey",
      showlegend = TRUE,
      shapes = list(
        list(
          type = "rect",
          xref = "paper", yref = "paper",
          x0 = 0, x1 = 1,
          y0 = 0, y1 = 1,
          line = list(color = "black", width = 2)
        )
      ),
      dragmode = FALSE,
      annotations = annotations
    )
  
  
  # FFRD RAS Events
  # ---------------
  if (!is.null(df_ffrd_ras) && nrow(df_ffrd_ras) > 0) {
    fig <- fig %>% 
      add_trace(
        x = df_ffrd_ras$Discharge, 
        y = df_ffrd_ras$Stage, 
        type = 'scatter', 
        mode = 'markers',
        legendgroup = "FFRD RAS Event Peaks", 
        name = "FFRD RAS Event Peaks",
        hovertext = paste0("Ref Line: ",ffrd_ras_event_peaks_element,
                            "<br>Stage: ", round(df_ffrd_ras$Stage,1),
                            "<br>Discharge: ", round(df_ffrd_ras$Discharge,0),
                            "<br>Event/Block:", df_ffrd_ras$Event,"/", df_ffrd_ras$Block),
        hoverinfo = "text", xaxis = "x",
        marker = list(color = ffrd_ras_color, width = 1.5, opacity = 0.5),
        showlegend = TRUE
      ) 
  }      

  
  # Single Datasets 
  # ---------------
  # USGS Rating
  if (!is.null(rating_data) && length(rating_data) > 0) {
    fig <- fig %>% 
      add_trace(
        x=rating_data$Discharge, 
        y=rating_data$Stage,
        type = 'scatter', 
        mode = 'lines', 
        name = 'USGS Rating', 
        showlegend = TRUE,
        line = list(color = usgs_rating_color, 
                    dash="dash",
                    size = 4),
        hoverinfo = "text",
        hovertext = paste("USGS Rating",
                          "<br>Stage: ", rating_data$Stage,
                          "<br>Discharge: ", rating_data$Discharge)
      )
  }
  
  # USGS Measurements
  if (!is.null(meas_data) && length(meas_data) > 0) {
    fig <- fig %>%
      add_trace(
        x=meas_data$Discharge, 
        y=meas_data$Stage,
        type = 'scatter', 
        mode = 'markers', 
        name = 'USGS Measurements', 
        showlegend = TRUE,
        marker = list(color = usgs_measurements_color, 
                      size = 7),
        hoverinfo = "text",
        hovertext = paste("Measurement Date: ", meas_data$Date,
                          "<br>Stage (adjusted): ", meas_data$Stage,
                          "<br>Discharge: ", meas_data$Discharge)
      )
  }
  
  # USGS Annual Peaks
  if (!is.null(USGS_peaks_data) && length(USGS_peaks_data) > 0) {        
    fig <- fig %>%
      add_trace(
        x=USGS_peaks_data$Discharge, 
        y=USGS_peaks_data$Stage,
        type = 'scatter', 
        mode = 'markers', 
        name = 'USGS Annual Peaks', 
        showlegend = TRUE,
        marker = list(color = usgs_annual_peaks_color, 
                      symbols = "triangle", 
                      size = 7),
        hoverinfo = "text",
        hovertext = paste("Peak Date: ", USGS_peaks_data$Date,
                          "<br>Stage (adjusted): ", USGS_peaks_data$Stage,
                          "<br>Discharge: ", USGS_peaks_data$Discharge)
      )
  }
  
  # Calibration Event Results
  if (!is.null(calib_event_peaks) && length(calib_event_peaks) > 0) {
    fig <- fig %>%
      add_trace(
        x=calib_event_peaks$Discharge, 
        y=calib_event_peaks$Stage,
        type = 'scatter', 
        mode = 'markers', 
        name = 'Calibration Event Results', 
        showlegend = TRUE,
        marker = list(color = calibration_event_results_color, 
                      symbols = "triangle", 
                      size = 10),
        hoverinfo = "text",
        hovertext = paste("Event: ", calib_event_peaks$event,
                          "<br>Stage: ", calib_event_peaks$Stage,
                          "<br>Discharge: ", calib_event_peaks$Discharge)
      )
  }
  
  # RAS Single Event
  if (!missing(ras_single_event) && !is.null(ras_single_event)){
  fig <- fig %>%
    add_trace(
      x=RAS_Rating$Discharge, 
      y=RAS_Rating$Stage,
      type = 'scatter', 
      mode = 'markers', 
      name = 'RAS Single Event', 
      visible = 'legendonly',
      marker = list(color = ras_single_event_color,
                    symbols = "triangle",
                    size = 4),
      hoverinfo = "text",
      hovertext = paste("Reference Line: ", RAS_Rating$ref_line_name,
                        "<br>Stage: ", round(RAS_Rating$Stage,2),
                        "<br>Discharge: ", round(RAS_Rating$Discharge,2))
    )
  }
  
  
  # Output either a json (default) or an html depending on the user-specification
  if (json){
    return(plot_to_json(fig))
  } else{
    return(plot_to_html(fig))
  }
}


