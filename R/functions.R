# Core data loading and processing functions
#' Load and prepare country data
#' @param data_path Path to the parquet file
#' @return A list containing the data source and country list
#' @export
load_country_data <- function(data_path = "./data/data.parquet") {
  # Read data using duckplyr's optimized parquet reader
  ds <- read_parquet_duckdb(
    path = data_path,
    prudence = "lavish"
  )
  
  # Optimized country list calculation
  country_list <- ds %>%
    filter(is_collab == FALSE) %>%
    distinct(country, iso2c) %>%
    filter(!is.na(country) & !is.na(iso2c) & country != "" & iso2c != "") %>%
    arrange(country) %>%
    collect()
  
  # Get unique chemical categories
  chemical_categories <- ds %>%
    distinct(chemical) %>%
    collect() %>%
    pull(chemical) %>%
    sort()
    
  # Get unique regions
  regions <- ds %>%
    filter(is_collab == FALSE) %>%
    distinct(region) %>%
    collect() %>%
    pull(region) %>%
    sort()
  
  regions <- c("All", regions[!is.na(regions)])
  
  return(list(
    data = ds,
    country_list = country_list,
    chemical_categories = chemical_categories,
    regions = regions
  ))
}

#' Find collaborating countries and their contribution strengths
#' @param ds Data source
#' @param country ISO code of the selected country
#' @param year_range Year range to consider
#' @param chemical_category Chemical category filter
#' @return Data frame of collaborating countries with their contributions
#' @export
find_collaborating_countries <- function(ds, country, year_range, chemical_category = "All") {
  query <- ds %>%
    filter(
      is_collab == TRUE,
      between(year, year_range[1], year_range[2]),
      grepl(country, iso2c)
    )
  
  if (chemical_category != "All") {
    query <- query %>% filter(chemical == chemical_category)
  }
  
  # Get collaborations with contribution strengths
  collabs <- query %>%
    collect() %>%
    mutate(
      partners = strsplit(as.character(iso2c), "-")
    ) %>%
    unnest(partners) %>%
    filter(partners != country) %>%
    group_by(partners) %>%
    summarise(
      total_contribution = sum(percentage, na.rm = TRUE),
      collaboration_count = n()
    ) %>%
    arrange(desc(total_contribution))
  
  return(collabs)
}

#' Create an interactive selection map
#' @param selected_country Currently selected country ISO code
#' @param collaborating_countries Data frame of collaborating countries
#' @param country_list Country lookup table
#' @return Leaflet map object
#' @export
create_selection_map <- function(selected_country = NULL, 
                               collaborating_countries = NULL,
                               country_list) {
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Join with country list
  world_map <- world_map %>%
    left_join(country_list, by = c("iso_a2" = "iso2c"))
  
  # Create color palette for different states
  pal <- colorFactor(
    palette = c("#E5E5E5", "#3388ff", "#66c2a5"),  # Unselected, Selected, Collaborating
    domain = c("none", "selected", "collaborating")
  )
  
  # Determine country states
  world_map$state <- "none"
  if (!is.null(selected_country)) {
    world_map$state[world_map$iso_a2 == selected_country] <- "selected"
    
    if (!is.null(collaborating_countries)) {
      world_map$state[world_map$iso_a2 %in% collaborating_countries$partners] <- "collaborating"
    }
  }
  
  # Create map
  map <- leaflet(world_map) %>%
    addTiles() %>%
    setView(lng = 0, lat = 20, zoom = 2) %>%
    addPolygons(
      fillColor = ~pal(state),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~name,
      layerId = ~iso_a2,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#E5E5E5", "#3388ff", "#66c2a5"),
      labels = c("Available", "Selected", "Collaborating"),
      title = "Country Status",
      opacity = 0.7
    )
  
  return(map)
}

#' Process data for visualization
#' @param ds Data source
#' @param selected_country Selected country ISO code
#' @param year_range Year range
#' @param chemical_category Chemical category
#' @param region_filter Region filter
#' @return Processed data for plotting
#' @export
process_visualization_data <- function(ds, selected_country, year_range, 
                                     chemical_category = "All", 
                                     region_filter = "All") {
  # Base query for individual contributions
  base_query <- ds %>%
    filter(
      is_collab == FALSE,
      between(year, year_range[1], year_range[2])
    )
  
  # Apply filters
  if (chemical_category != "All") {
    base_query <- base_query %>% filter(chemical == chemical_category)
  }
  
  if (region_filter != "All") {
    base_query <- base_query %>% filter(region == region_filter)
  }
  
  if (!is.null(selected_country)) {
    base_query <- base_query %>% filter(iso2c == selected_country)
  }
  
  # Return processed data
  base_query %>%
    select(country, iso2c, year, chemical, percentage, region) %>%
    collect()
}

#' Create contributions visualization
#' @param data Processed data
#' @param chemical_category Chemical category
#' @return Plotly visualization
#' @export
create_contributions_plot <- function(data, chemical_category = "All") {
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
      add_annotations(
        text = "No data available",
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 16)
      )
    )
  }
  
  p <- ggplot(data, aes(x = year, y = percentage)) +
    geom_line(aes(color = country), linewidth = 1) +
    geom_point(
      aes(
        text = paste0(
          "<b>", country, "</b><br>",
          "Year: ", year, "<br>",
          "Chemical: ", chemical, "<br>",
          "Contribution: ", scales::percent(percentage/100, accuracy = 0.01)
        )
      ),
      size = 3
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.01, scale = 1)
    ) +
    scale_color_viridis_d() +
    labs(
      title = paste("Chemical Space Contributions -", chemical_category),
      x = "Year",
      y = "Contribution (%)"
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text") %>%
    layout(hoverlabel = list(bgcolor = "white"))
}

#' Get top contributors summary
#' @param ds Data source
#' @param year_range Year range
#' @param chemical_category Chemical category
#' @param region_filter Region filter
#' @param n Number of top contributors
#' @return Data frame of top contributors
#' @export
get_top_contributors <- function(ds, year_range, chemical_category = "All", 
                               region_filter = "All", n = 5) {
  query <- ds %>%
    filter(
      is_collab == FALSE,
      between(year, year_range[1], year_range[2])
    )
  
  if (chemical_category != "All") {
    query <- query %>% filter(chemical == chemical_category)
  }
  
  if (region_filter != "All") {
    query <- query %>% filter(region == region_filter)
  }
  
  query %>%
    group_by(country, iso2c) %>%
    summarise(
      total_contribution = sum(percentage, na.rm = TRUE),
      years_active = n_distinct(year),
      avg_contribution = mean(percentage, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_contribution)) %>%
    head(n) %>%
    collect()
}