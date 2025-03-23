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
    geom_jitter(aes(size = .data[[y_var]]/100), alpha = 0.4, show.legend = FALSE) +
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
                                year = 2007, 
                                map_key = "region", 
                                fill_var = "lifeExp", 
                                fill_label = "Years", 
                                main_title = "Life Expectancy", 
                                sub_title = paste("year:", year)) {
  plot_data <- df %>%
    filter(.data[["year"]] == year) %>%
    right_join(world_df, by = c("mapname" = map_key))
  
  p <- ggplot(plot_data, aes(long, lat, group = group, fill = .data[[fill_var]])) +
    geom_polygon(color = "white", size = 0.01) +
    theme_void() +
    viridis::scale_fill_viridis(option = "B", name = fill_label) +
    labs(
      title = main_title,
      subtitle = sub_title
    ) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1)
    ) +
    coord_fixed(ratio = 1.3)
  
  plotly::ggplotly(p)
}
