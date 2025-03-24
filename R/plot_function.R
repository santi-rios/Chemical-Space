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
                                    title = "National Contributions to Chemical Space",
                                    y_label = "% of New Substances",
                                    x_label = NULL,
                                    x_continuous_limits_extra = 6) {
  # Create base ggplot
  p <- ggplot(
    data,
    aes(x = .data[[x_var]],
        y = .data[[y_var]],
        color = .data[[color_var]],
        group = .data[[group_var]],
        text = paste0(
          "<b>Country:</b> ", .data[[group_var]],
          "<br><b>Percentage:</b> ", scales::percent(.data[[y_var]], accuracy = 0.01, scale = 1),
          "<br><b>Year:</b> ", .data[[x_var]],
          "<br><b>Region:</b> ", .data[[region_var]]
        ))
  ) +
    geom_line() +
    geom_point(aes(size = .data[[y_var]]/100), alpha = 0.4, show.legend = FALSE) +
    # Only label top countries with position adjustment to minimize overlap
    geom_text(
      data = end_labels_data,
      aes(label = .data[[group_var]]),
      hjust = -0.1,  # Push labels to the right of the last point
      nudge_x = 0.5, # Add additional horizontal push
      size = 3,
      show.legend = FALSE,
      check_overlap = TRUE
    ) +
    scale_colour_identity() +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_x_continuous(limits = c(min_year, max_year + x_continuous_limits_extra)) +
    theme(
      legend.position = "none",
      legend.text = element_text(size = 8, face = "bold"),
      legend.title = element_blank(),
      axis.title.x = if (is.null(x_label)) element_blank() else element_text()
    ) +
    labs(
      title = title,
      y = y_label,
      x = if (!is.null(x_label)) x_label else NULL
    )
  
  # Convert ggplot object to interactive plotly object with WebGL
  plotly_obj <- ggplotly(p, tooltip = "text") %>% 
    plotly::toWebGL()
  
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
  # Merge data with world map
  plot_data <- world_df %>%
    left_join(df, by = "country") %>%
    mutate(
      formatted_value = sprintf("%.2f%%", .data[[fill_var]]),
      tooltip_text = paste(
        "<b>", country, "</b><br>",
        "Value: ", formatted_value, "<br>",
        "Region: ", coalesce(region, "N/A"), "<br>",
        "Best Year: ", coalesce(as.character(best_year), "N/A"), "<br>",
        "Worst Year: ", coalesce(as.character(worst_year), "N/A")
      )
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(
    lng, lat, 
    group = group, 
    fill = .data[[fill_var]],
    text = tooltip_text
  )) +
    geom_polygon(color = "white", size = 0.01) +
    scale_fill_distiller(
      palette = "Spectral",
      labels = function(x) paste0(round(x, 2), "%"),
      name = fill_label
    ) +
    labs(title = main_title) +
    theme_void() +
    coord_fixed(ratio = 1.3) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "bottom"
    )
  
  # Convert to plotly with custom tooltip
  plotly::ggplotly(p, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = 'svg',
        filename = 'bermudez-montana_etal_2025',
        height = 500,
        width = 700,
        scale = 1
      )
    ) %>%
    layout(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(size = 12)
      )
    )
}



createTrendPlot <- function(data,
                            label_var = "country",
                            color_var = "cc",
                            group_var = "country",
                            region_var = "region",
                            y_var = "percentage",
                            x_var = "year",
                            title = "Percentage of new compounds reported by each country in journals",
                            y_label = "Percentage of new substances",
                            x_label = "Year",
                            label_nudge_x = 0.3,
                            label_nudge_y = 0.4,
                            label_size = 4,
                            top_n = NULL) {
  
  # Prepare label data
  label_data <- if (!is.null(top_n)) {
    data %>%
      group_by(.data[[group_var]]) %>%
      summarise(max_value = max(.data[[y_var]], na.rm = TRUE)) %>%
      arrange(desc(max_value)) %>%
      slice_head(n = top_n) %>%
      left_join(
        data %>% 
          group_by(.data[[group_var]]) %>% 
          filter(.data[[x_var]] == max(.data[[x_var]])),
        by = group_var
      )
  } else {
    data %>% 
      group_by(.data[[group_var]]) %>% 
      filter(.data[[x_var]] == max(.data[[x_var]]))
  }
  
  # Create base plot
  p <- ggplot(
    data,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[color_var]],
      group = .data[[group_var]],
      text = paste0(
        "<b>Country:</b> ", .data[[group_var]],
        "<br><b>Percentage:</b> ", scales::percent(.data[[y_var]], accuracy = 0.01, scale = 1),
        "<br><b>Year:</b> ", .data[[x_var]],
        "<br><b>Region:</b> ", .data[[region_var]]
      )
    )
  ) +
    geom_line(alpha = 0.8) +
    geom_point(
      aes(size = .data[[y_var]] / 100),
      alpha = 0.4,
      show.legend = FALSE
    ) +
    geom_text(
      data = label_data,
      aes(
        label = .data[[label_var]],
        x = .data[[x_var]] + label_nudge_x,
        y = .data[[y_var]] + label_nudge_y
      ),
      hjust = 0,
      size = label_size,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, scale = 1),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    scale_color_identity() +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
  
  # Convert to interactive plotly with performance optimizations
  plotly::ggplotly(p, tooltip = "text") %>%
    plotly::partial_bundle() %>%
    plotly::toWebGL() %>%
    plotly::layout(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(size = 12)
      )
    )
}


# functions.R
# functions.R
createArticlePlot <- function(data, 
               source_title,
               y_title = "Percentage of new substances",
               flag_size_range = c(10, 30)) {
  # Prepare data
  plot_data <- data %>%
  mutate(
    iso2c = countrycode::countrycode(country, "country.name", "iso2c"),
    # Use a larger flag to reduce pixelation
    flag_url = paste0("https://flagcdn.com/80x60/", tolower(iso2c), ".png")
  )
  
  # Plotly scatter (each row becomes a frame)
  p <- plot_ly(
  plot_data,
  x = ~year,
  y = ~percentage,
  color = ~country,
  colors = "Set1",
  type = "scatter",
  mode = "markers",
  marker = list(
    sizemode = "diameter",
    # Keep marker size modest
    size = ~percentage,
    opacity = 0.4,
    sizeref = 0.15 * max(plot_data$percentage) / max(flag_size_range)
  ),
  text = ~paste(
    "<b>Country:</b> ", country,
    "<br><b>Year:</b> ", year,
    "<br><b>Value:</b> ", scales::percent(percentage / 100)
  ),
  hoverinfo = "text",
  frame = ~year
  )
  
  # Show flags only for the last year of each country (appear at final frame)
  last_data <- plot_data %>%
  group_by(country) %>%
  filter(year == max(year))
  
  p <- p %>% layout(
  images = lapply(seq_len(nrow(last_data)), function(i) {
    list(
    source = last_data$flag_url[i],
    xref = "x", yref = "y",
    x = last_data$year[i],
    y = last_data$percentage[i],
    # Keep a fixed smaller size to avoid extreme scaling
    sizex = 1,
    sizey = 1,
    xanchor = "center", 
    yanchor = "middle",
    sizing = "contain",
    layer = "above"
    )
  }),
  title = list(
    text = paste("Article Figures - Source:", source_title),
    font = list(size = 18)
  ),
  xaxis = list(title = "Year", gridcolor = "#eeeeee"),
  yaxis = list(
    title = y_title, 
    gridcolor = "#eeeeee",
    tickformat = ".1%"
  ),
  plot_bgcolor = "rgb(250, 250, 250)",
  paper_bgcolor = "rgb(250, 250, 250)"
  ) %>%
  animation_opts(
    frame = 300,
    transition = 0,
    redraw = FALSE
  ) %>%
  plotly::partial_bundle() %>%
  plotly::toWebGL() %>%
  layout(
    hoverlabel = list(
    bgcolor = "white",
    bordercolor = "black",
    font = list(size = 12)
    )
  )
  
  p
}

