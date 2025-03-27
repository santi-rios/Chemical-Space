#' Create Chemical Space Plot
#'
#' This function creates an interactive line plot using ggplot2 and plotly.
#' It is designed to show national contributions to chemical space with customizable
#' titles, axis labels, and variable mappings.
#'
#' @param data A data frame containing the main plot data.
#' @param end_labels_data A data frame subset that contains the last data points for label annotations.
#' @param min_year Numeric. Minimum value for the x-axis.
#' @param max_year Numeric. Maximum value for the x-axis.
#' @param x_var Character. Column name for the x-axis (default "year").
#' @param y_var Character. Column name for the y-axis (default "percentage").
#' @param color_var Character. Column name for the line color (default "cc").
#' @param group_var Character. Column name for the grouping variable (default "country").
#' @param region_var Character. Column name for the region variable in tooltips (default "region").
#' @param title Character. The plot title (default "National Contributions to Chemical Space").
#' @param y_label Character. Y axis label (default "% of New Substances").
#' @param x_label Character or NULL. X axis label. If NULL, the x-axis title is hidden.
#' @param x_continuous_limits_extra Numeric. Extra space added to the maximum x value for label placement (default 6).
#'
#' @return An interactive plotly object with WebGL rendering.
#' @import ggplot2 plotly scales
#' @export
createChemicalSpacePlot <- function(data, end_labels_data,
                                    min_year, max_year,
                                    x_var = "year",
                                    y_var = "percentage",
                                    color_var = "cc",
                                    group_var = "country",
                                    region_var = "region",
                                    # flag_png_col = "flags",   # column with PNG flag URLs
                                    title = "National Contributions to Chemical Space",
                                    y_label = "% of New Substances",
                                    x_label = NULL) {
  # Create a named vector mapping each country to its hex color
  country_colors <- unique(data[, c(group_var, color_var)])
  color_map <- setNames(country_colors[[color_var]], country_colors[[group_var]])
  
  # Build the ggplot
  p <- ggplot(
    data,
    aes(x = .data[[x_var]],
        y = .data[[y_var]],
        color = .data[[group_var]],  # now using country name
        group = .data[[group_var]],
        text = paste0(
          "<b>Country:</b> ", .data[[group_var]],
          "<br><b>Percentage:</b> ", scales::percent(.data[[y_var]], accuracy = 0.01, scale = 1),
          "<br><b>Year:</b> ", .data[[x_var]],
          "<br><b>Region:</b> ", .data[[region_var]]
        ))
  ) +
    geom_line(alpha = 0.85) +
    geom_point(
      aes(size = .data[[y_var]]),
      shape = 16,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    geom_text(
      data = end_labels_data,
      aes(
        label = .data[[group_var]],
        x = .data[[x_var]]
        # y = .data[[y_var]] + 0.4
      ),
      # hjust = max_year + 3,
      nudge_x = -1.3,
      vjust = 1,
      angle = 45,
      size = 3,
      alpha = 0.7,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    scale_radius(range = c(0.5, 4)) +
    scale_color_manual(values = color_map, name = "Country") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 1),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 7, face = "bold"),
      legend.title = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.title = element_text(size = 9),
      axis.title.x = if (is.null(x_label)) element_blank() else element_text()
    ) +
    labs(
      title = title,
      y = y_label,
      x = if (!is.null(x_label)) x_label else NULL
    )

  # Convert ggplot object to an interactive plotly object with WebGL
  plotly_obj <- plotly::ggplotly(p, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE
    ) %>%
    # UNCOMMENT THESE LINES - they're needed for proper layout
    layout(
      legend = list(
        orientation = "h",
        y = -0.15,
        yanchor = "top",
        x = 0.5,
        xanchor = "center"
      ),
      margin = list(b = 80, l = 40, r = 40, t = 40)
    )

  return(plotly_obj)
}


# ...existing code or additional functions if needed...

#' Create Static Map Plot
#'
#' This function takes a data frame, filters by a given year, merges with
#' world polygon data, and returns a static map plot annotated by a chosen
#' fill variable. The result is converted to a plotly object.
#'
#' @param df Data frame (e.g., gapminder_data) containing at least columns for year and the fill variable.
#' @param world_df Data frame containing map polygons (world boundaries).
#' @param year Numeric. The year to filter on.
#' @param map_key Character. Name of the column used to match world regions.
#' @param fill_var Character. Name of the column in df used to fill polygons.
#' @param fill_label Character. Legend title for fill variable.
#' @param main_title Character. Main plot title.
#' @param sub_title Character. Subtitle for context (e.g., "year: 2007").
#'
#' @return A plotly object showing the static world map plot.
#' @import ggplot2 dplyr plotly viridis
#' @export
createStaticMapPlot <- function(df,
                                world_df,
                                map_key = "country",
                                fill_var = "value",
                                fill_label = "Average Contribution",
                                main_title = "Country Contributions") {

  # Get max value for scaling (round up to nearest 5%)
  max_val <- ifelse(length(df[[fill_var]]) > 0, max(df[[fill_var]], na.rm = TRUE), 0)
  ceiling_val <- ceiling(max_val / 5) * 5

  # Create nice round breaks based on data range
  if (ceiling_val <= 5) {
    # For small ranges, use 1% steps
    breaks <- seq(0, max(5, ceiling_val), by = 1)
    label_fmt <- "%.1f-%.1f%%"
  } else if (ceiling_val <= 20) {
    # For medium ranges, use 2.5% steps
    breaks <- seq(0, ceiling_val, by = 2.5)
    label_fmt <- "%.1f-%.1f%%"
  } else {
    # For large ranges, use 5% steps
    breaks <- seq(0, ceiling_val, by = 5)
    label_fmt <- "%.0f-%.0f%%"
  }

  # Create labels showing ranges instead of just endpoints
  labels <- character(length(breaks) - 1)
  for (i in 1:(length(breaks) - 1)) {
    labels[i] <- sprintf(label_fmt, breaks[i], breaks[i+1])
  }

  # Merge data with world map
  plot_data <- world_df %>%
    left_join(df, by = "country") %>%
    mutate(
      formatted_value = ifelse(!is.na(.data[[fill_var]]),
                              sprintf("%.2f%%", .data[[fill_var]]),
                              "No data for current selection"),
      # Create discrete fill variable using our custom breaks
      fill_discrete = cut(
        .data[[fill_var]],
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE
      ),
      tooltip_text = paste(
        "<b>", country, "</b><br>",
        "Avg Contribution: ", formatted_value, "<br>",
        "Region: ", coalesce(region, "N/A"), "<br>",
        "Best Year: ", coalesce(as.character(best_year), "N/A"), "<br>",
        "Worst Year: ", coalesce(as.character(worst_year), "N/A")
      )
    )

  # Create plot with discrete fill
  p <- ggplot(plot_data, aes(
    lng, lat,
    group = group,
    fill = fill_discrete, # Use the discrete fill variable
    text = tooltip_text
  )) +
    geom_polygon(color = "#000000", size = 0.1) +
    # Use a discrete color scale with explicit NA handling
    scale_fill_brewer(
      palette = "Spectral",
      direction = -1,
      name = fill_label,
      na.value = "#EEEEEE", # Light gray
      # na.translate = TRUE,
      labels = function(x) {
        ifelse(is.na(x), "No data for current selection", as.character(x))
      },
      drop = FALSE
    ) +
    labs(title = main_title) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void() +
    # coord_fixed(ratio = 1.3) +
    coord_map(projection = "mollweide", xlim = c(-180, 180), ylim = c(-90, 90)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 8),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank()
    )

  # Convert to plotly with custom tooltip
  plotly::ggplotly(p, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = 'png',
        filename = 'bermudez-montana_etal_2025',
        height = 500,
        width = 700,
        scale = 1
      )
    ) %>%
    layout(
      legend = list(
        orientation = "h",
        y = -0.15,
        yanchor = "top",
        x = 0.6,
        xanchor = "center"
      ),
      margin = list(b = 80, l = 40, r = 40, t = 40)
    )
}


##############

# Create a custom variant of createStaticMapPlot for collaborations
# Add this function to your R/plot_function.R file
# Add this function to your R/plot_function.R file
createCollabMapPlot <- function(df,
                                world_df,
                                map_key = "country",
                                fill_var = "value",
                                fill_label = "Collaboration Strength",
                                main_title = "") {

  # Get max value for scaling (round up to nearest 5%)
  max_val <- ifelse(length(df[[fill_var]]) > 0, max(df[[fill_var]], na.rm = TRUE), 0)
  ceiling_val <- ceiling(max_val / 5) * 5

  # Create nice round breaks based on data range
  if (ceiling_val <= 5) {
    breaks <- seq(0, max(5, ceiling_val), by = 1)
    label_fmt <- "%.1f-%.1f%%"
  } else if (ceiling_val <= 20) {
    breaks <- seq(0, ceiling_val, by = 2.5)
    label_fmt <- "%.1f-%.1f%%"
  } else {
    breaks <- seq(0, ceiling_val, by = 5)
    label_fmt <- "%.0f-%.0f%%"
  }

  # Create labels showing ranges
  labels <- character(length(breaks) - 1)
  for (i in 1:(length(breaks) - 1)) {
    labels[i] <- sprintf(label_fmt, breaks[i], breaks[i+1])
  }

  # Merge data with world map
  plot_data <- world_df %>%
    left_join(df, by = "country") %>%
    mutate(
      formatted_value = ifelse(!is.na(.data[[fill_var]]),
                              sprintf("%.2f%%", .data[[fill_var]]),
                              "No data for current selection"),
      # Create discrete fill variable using our custom breaks
      fill_discrete = cut(
        .data[[fill_var]],
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE
      ),
      # Format collaboration list for better readability
      formatted_collab_list = ifelse(!is.na(collab_list),
                                    gsub(",", " & ", collab_list),
                                    NA_character_),
      # Enhanced tooltip with better explanation of collaboration information
      tooltip_text = paste(
        "<b>", country, "</b><br>",
        "<b>Collaboration Strength:</b> ", formatted_value, "<br>",
        "<i>This represents the country's average contribution to the chemical space<br>",
        "through international collaborations over the selected time period.</i><br>",
        ifelse(!is.na(best_year) & !is.na(worst_year),
               paste0(
                 "<b>Strongest Year:</b> ", best_year, " (", 
                 sprintf("%.2f%%", best_year_value), ")<br>",
                 "<b>Weakest Year:</b> ", worst_year, " (", 
                 sprintf("%.2f%%", worst_year_value), ")"
               ),
               ""),
        ifelse(!is.na(formatted_collab_list),
               paste0("<br><b>Main Collaborations:</b><br>• ", 
                      gsub("; ", "<br>• ", formatted_collab_list)),
               "")
      )
    )

  # Create plot with discrete fill
  p <- ggplot(plot_data, aes(
    lng, lat,
    group = group,
    fill = fill_discrete, # Use the discrete fill variable
    text = tooltip_text
  )) +
    geom_polygon(color = "white", size = 0.01) +
    # Use a discrete color scale with explicit NA handling
    scale_fill_brewer(
      palette = "Spectral",
      direction = -1,
      name = fill_label,
      na.value = "#EEEEEE", # Light gray
      na.translate = TRUE,
      labels = function(x) {
        ifelse(is.na(x), "No data for current selection", as.character(x))
      },
      drop = FALSE
    ) +
    labs(title = main_title) +
    theme_void() +
    coord_fixed(ratio = 1.3) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = 8)
    )

  # Convert to plotly with custom tooltip
  plotly::ggplotly(p, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = 'svg',
        filename = 'bermudez-montana_etal_2025_collab',
        height = 500,
        width = 700,
        scale = 1
      )
    ) %>%
    layout(
      legend = list(
        orientation = "h",
        y = -0.15,
        yanchor = "top",
        x = 0.5,
        xanchor = "center"
      ),
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(size = 12)
      ),
      margin = list(b = 80, l = 40, r = 40, t = 40)
    )
}


##########

createTrendPlot <- function(data, end_labels_data,
                            min_year, max_year,
                            color_var = "cc",
                            group_var = "country",
                            region_var = "region",
                            y_var = "percentage",
                            x_var = "year",
                            title = "Percentage of new compounds reported in journals",
                            y_label = "Percentage of new substances",
                            x_label = "Year") {

  # Create a named vector mapping each country to its hex color
  country_colors <- unique(data[, c(group_var, color_var)])
  color_map <- setNames(country_colors[[color_var]], country_colors[[group_var]])
  
  # Build the ggplot
  p <- ggplot(
    data,
    aes(x = .data[[x_var]],
        y = .data[[y_var]],
        color = .data[[group_var]],  # now using country name
        group = .data[[group_var]],
        text = paste0(
          "<b>Country:</b> ", .data[[group_var]],
          "<br><b>Percentage:</b> ", scales::percent(.data[[y_var]], accuracy = 0.01, scale = 1),
          "<br><b>Year:</b> ", .data[[x_var]],
          "<br><b>Region:</b> ", .data[[region_var]]
        ))
  ) +
    geom_line(alpha = 0.85) +
    geom_point(
      aes(size = .data[[y_var]]),
      shape = 16,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    geom_text(
      data = end_labels_data,
      aes(
        label = .data[[group_var]],
        x = .data[[x_var]]
        # y = .data[[y_var]] + 0.4
      ),
      # hjust = max_year + 3,
      nudge_x = -1.3,
      vjust = 1,
      angle = 45,
      size = 3,
      alpha = 0.7,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    scale_radius(range = c(0.5, 4)) +
    scale_color_manual(values = color_map, name = "Country") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 1),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 7, face = "bold"),
      legend.title = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.title = element_text(size = 9),
      axis.title.x = if (is.null(x_label)) element_blank() else element_text()
    ) +
    labs(
      title = title,
      y = y_label,
      x = if (!is.null(x_label)) x_label else NULL
    )

  # Convert ggplot object to an interactive plotly object with WebGL
  plotly_obj <- plotly::ggplotly(p, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE
    ) %>%
    # UNCOMMENT THESE LINES - they're needed for proper layout
    layout(
      legend = list(
        orientation = "h",
        y = -0.15,
        yanchor = "top",
        x = 0.5,
        xanchor = "center"
      ),
      margin = list(b = 80, l = 40, r = 40, t = 40)
    )

  return(plotly_obj)
}





##########
createArticleFlagPlot <- function(data,
                                  source_title,
                                  y_title,
                                  flag_size_range = c(0.5, 3)) {
  # Prepare data
  plot_data <- data %>%
    dplyr::mutate(
      iso2c = countrycode::countrycode(country, "country.name", "iso2c"),
      flag_url = paste0("https://flagcdn.com/80x60/", tolower(iso2c), ".png")
    )
  
  # Precompute formatted value based on source
  if (source_title == "Number of Researchers") {
    plot_data$formatted_value <- scales::comma(plot_data$percentage)
    y_title <- paste0(y_title, " (millions)")
    y_format <- ".2f"
    # Convert to millions for display
    plot_data$percentage <- plot_data$percentage / 1e6
  } else if (source_title == "Annual growth rate of the GDP") {
    plot_data$formatted_value <- paste0(scales::comma(plot_data$percentage), "%")
    y_format <- ".1f"
  } else if (source_title == "Country participation in the CS") {
    plot_data$formatted_value <- scales::comma(plot_data$percentage)
    y_format <- ",.0f"
  } else {
    plot_data$formatted_value <- scales::percent(plot_data$percentage / 100)
    y_format <- ".1%"
  }
  
  # Get all unique years for animation frames
  years <- sort(unique(plot_data$year))
  
  # Create an empty plot
  p <- plot_ly() %>%
    layout(
      xaxis = list(
        range = c(min(plot_data$year) - 1, max(plot_data$year) +1),
        title = "Year"
      ),
      yaxis = list(
        range = c(min(plot_data$percentage, na.rm = TRUE) * 0.9, 
                 max(plot_data$percentage, na.rm = TRUE) * 1.1),
        title = y_title
      )
    )
  
  # Create frames with dynamically sized flags
  frames_list <- list()
  
  # Calculate min/max for flag sizing
  y_min <- min(plot_data$percentage, na.rm = TRUE)
  y_max <- max(plot_data$percentage, na.rm = TRUE)
  y_range <- y_max - y_min
  
  for (yr in years) {
    yr_data <- plot_data %>% dplyr::filter(year == yr)
    
    # Add dummy traces to ensure countries show in the legend
    p <- p %>% add_trace(
      data = yr_data,
      x = ~year,
      y = ~percentage,
      color = ~country,
      colors = "Set1",
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0),  # Invisible markers
      text = ~paste(
        "<b>Country:</b> ", country,
        "<br><b>Year:</b> ", year,
        "<br><b>Value:</b> ", formatted_value
      ),
      hoverinfo = "text",
      frame = ~year,
      showlegend = TRUE
    )
    
    # Create flag images with size proportional to value
    frame_images <- lapply(seq_len(nrow(yr_data)), function(i) {
      # Normalize the value between min and max size
      normalized_value <- (yr_data$percentage[i] - y_min) / y_range
      size_factor <- flag_size_range[1] + normalized_value * (flag_size_range[2] - flag_size_range[1])
      
      # Ensure minimum size
      size_factor <- max(size_factor, flag_size_range[1])
      
      list(
        source = yr_data$flag_url[i],
        xref = "x", 
        yref = "y",
        x = yr_data$year[i],
        y = yr_data$percentage[i],
        sizex = size_factor,
        sizey = size_factor,
        xanchor = "center",
        yanchor = "middle",
        sizing = "contain",
        layer = "above"
      )
    })
    
    frames_list[[as.character(yr)]] <- list(
      name = yr,
      data = list(),
      layout = list(images = frame_images)
    )
  }
  
  # Base layout
  layout_args <- list(
    title = list(
      text = paste("Article Figures - Source:", source_title),
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Year", 
      gridcolor = "#eeeeee"
    ),
    yaxis = list(
      title = y_title,
      gridcolor = "#eeeeee",
      tickformat = y_format
    ),
    plot_bgcolor = "rgb(250, 250, 250)",
    paper_bgcolor = "rgb(250, 250, 250)"
  )
  
  # Add crisis annotations for GDP
  if (source_title == "Annual growth rate of the GDP") {
    layout_args$shapes <- list(
      list(
        type = "line",
        x0 = 2007, x1 = 2007,
        y0 = 0, y1 = 1,
        yref = "paper",
        line = list(color = "red", dash = "dash", width = 1.5)
      ),
      list(
        type = "line",
        x0 = 2020, x1 = 2020,
        y0 = 0, y1 = 1,
        yref = "paper",
        line = list(color = "red", dash = "dash", width = 1.5)
      )
    )
    layout_args$annotations <- list(
      list(
        x = 2007, y = 0.95, xref = "x", yref = "paper",
        text = "Global Financial Crisis",
        showarrow = TRUE, arrowhead = 0, ax = 0, ay = -40,
        font = list(size = 12, color = "red")
      ),
      list(
        x = 2020, y = 0.95, xref = "x", yref = "paper",
        text = "COVID-19",
        showarrow = TRUE, arrowhead = 0, ax = 0, ay = -40,
        font = list(size = 12, color = "red")
      )
    )
  }
  
  # Apply layout and animation
  p <- p %>%
    layout(layout_args) %>%
    animation_opts(
      frame = 800,
      transition = 300,
      redraw = FALSE
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "Year: ")
    ) %>%
    animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
  
  # Add frames
  p$x$frames <- lapply(years, function(yr) {
    frames_list[[as.character(yr)]]
  })
  
  # Partial bundle
  p %>% plotly::partial_bundle()
}


createArticleDotPlot <- function(data, source_title, y_title) {
  # Prepare data
  plot_data <- data %>%
    dplyr::mutate(
      iso2c = countrycode::countrycode(country, "country.name", "iso2c")
    )
  
  # For labeling last x value
  end_labels <- plot_data %>%
    dplyr::group_by(country) %>%
    dplyr::filter(year == max(year, na.rm = TRUE))

  # Handle special case for "China-US in the CS"
  if (source_title == "China-US in the CS") {
    plot_data$formatted_value <- scales::percent(plot_data$percentage / 100)
    
    # Split data (one axis for main countries, another for smaller collaborations)
    main_countries <- c("China", "United States")
    main_data <- plot_data %>% dplyr::filter(country %in% main_countries)
    collab_data <- plot_data %>% dplyr::filter(!country %in% main_countries)
    
    p <- plot_ly() %>%
      # Main countries on primary y-axis
      add_trace(
        data = main_data,
        x = ~year,
        y = ~percentage,
        color = ~country,
        colors = "Set1",
        type = "scatter",
        mode = "lines+markers",
        marker = list(
          size = ~percentage / 5,
          sizemode = "diameter",
          sizeref = 2,
          opacity = 0.8,
          line = list(width = 1, color = '#FFFFFF')
        ),
        line = list(width = 2),
        text = ~paste(
          "<b>Country:</b> ", country,
          "<br><b>Year:</b> ", year,
          "<br><b>Value:</b> ", formatted_value
        ),
        hoverinfo = "text",
        name = ~country
      ) %>%
      # Collaboration metrics on secondary y-axis
      add_trace(
        data = collab_data,
        x = ~year,
        y = ~percentage,
        color = ~country,
        colors = "Set2",
        type = "scatter",
        mode = "lines+markers",
        marker = list(
          size = ~percentage * 100,
          sizemode = "diameter",
          sizeref = 2,
          opacity = 0.8,
          line = list(width = 1, color = '#FFFFFF')
        ),
        line = list(width = 2),
        text = ~paste(
          "<b>Metric:</b> ", country,
          "<br><b>Year:</b> ", year,
          "<br><b>Value:</b> ", formatted_value
        ),
        hoverinfo = "text",
        name = ~country,
        yaxis = "y2"
      )
    
    # Layout with dual axes
    layout_args <- list(
      title = list(
        text = paste("Article Figures - Source:", source_title),
        font = list(size = 18)
      ),
      xaxis = list(title = "Year", gridcolor = "#eeeeee"),
      yaxis = list(
        title = paste0(y_title, " (countries)"),
        side = "left",
        tickformat = ".0%",
        range = c(0, max(main_data$percentage, na.rm = TRUE) * 1.1),
        gridcolor = "#eeeeee"
      ),
      yaxis2 = list(
        title = paste0(y_title, " (collaborations)"),
        overlaying = "y",
        side = "right",
        tickformat = ".0%",
        range = c(0, max(collab_data$percentage, na.rm = TRUE) * 1.1),
        gridcolor = "#eeeeee"
      ),
      plot_bgcolor = "rgb(250, 250, 250)",
      paper_bgcolor = "rgb(250, 250, 250)",
      annotations = list()
    )
    
    # Add country name annotations for end labels
    for (i in seq_len(nrow(end_labels))) {
      layout_args$annotations[[i]] <- list(
        x = end_labels$year[i],
        y = end_labels$percentage[i],
        text = end_labels$country[i],
        showarrow = FALSE,
        xanchor = "left",
        xshift = 10,
        font = list(size = 12)
      )
    }
    
    # Apply layout
    p <- p %>% layout(layout_args)
    
    return(p)
  }

  # Otherwise handle other source_titles
  if (source_title == "Expansion of the CS") {
    plot_data$formatted_value <- scales::comma(plot_data$percentage)
    y_format <- ",.0f"
  } else if (source_title == "Country participation in the CS") {
    plot_data$formatted_value <- scales::comma(plot_data$percentage)
    y_format <- ",.0f"
  } else {
    plot_data$formatted_value <- scales::percent(plot_data$percentage / 100)
    y_format <- ".1%"
  }
  
  # Build plot without animation
  p <- plot_ly(
    data = plot_data,
    x = ~year,
    y = ~percentage,
    color = ~country,
    colors = colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(plot_data$country))),
    type = "scatter",
    mode = "lines+markers",
    marker = list(
      size = 8,
      opacity = 0.8,
      line = list(width = 1, color = '#FFFFFF')
    ),
    line = list(width = 2),
    text = ~paste(
      "<b>Country:</b> ", country,
      "<br><b>Year:</b> ", year,
      "<br><b>Value:</b> ", formatted_value
    ),
    hoverinfo = "text"
  )
  
  # Layout
  layout_args <- list(
    title = list(
      text = paste("Article Figures - Source:", source_title),
      font = list(size = 18)
    ),
    xaxis = list(title = "Year", gridcolor = "#eeeeee"),
    yaxis = list(
      title = y_title,
      gridcolor = "#eeeeee",
      tickformat = y_format
    ),
    plot_bgcolor = "rgb(250, 250, 250)",
    paper_bgcolor = "rgb(250, 250, 250)",
    annotations = list()
  )
  
  # Add country name annotations for end labels
  for (i in seq_len(nrow(end_labels))) {
    layout_args$annotations[[i]] <- list(
      x = end_labels$year[i],
      y = end_labels$percentage[i],
      text = end_labels$country[i],
      showarrow = FALSE,
      xanchor = "left",
      xshift = 10,
      font = list(size = 12)
    )
  }
  
  # Apply layout
  p <- p %>% layout(layout_args)
  
  return(p)
}

# Main wrapper function that determines which plot type to use
createArticlePlot <- function(data, source_title, y_title, flag_size_range = c(0.5, 3)) {
  # Choose the appropriate visualization based on source_title
  if (source_title %in% c("Number of Researchers", "Country participation in the CS", "Annual growth rate of the GDP")) {
    # Flag-based visualization
    createArticleFlagPlot(data, source_title, y_title, flag_size_range)
  } else {
    # Dot and line visualization
    createArticleDotPlot(data, source_title, y_title)
  }
}