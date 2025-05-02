# Helper functions for the Chemical Space Explorer Shiny App

#' Load and prepare initial data
#'
#' @param data_path Path to the parquet file
#' @return A list containing the data source (ds), country list, chemical categories, and regions
#' @export
load_country_data <- function(data_path = "./data/data.parquet") {
  # Read data using duckplyr's optimized parquet reader
  ds <- read_parquet_duckdb(
    path = data_path,
    prudence = "lavish" # Materialize data for testing/simpler debugging if needed
  )

  # Optimized country list calculation (including lat/lng/cc for map/plotting)
  # Handle potential NA regions
  country_list <- ds %>%
    filter(is_collab == FALSE) %>%
    distinct(country, iso2c, lat, lng, cc, region) %>%
    filter(!is.na(country) & !is.na(iso2c) & country != "" & iso2c != "") %>%
    collect() %>% # Collect before mutate for easier NA handling
    mutate(region = if_else(is.na(region) | region == "", "Other", region)) %>%
    arrange(country)

  # Get unique chemical categories
  chemical_categories <- ds %>%
    distinct(chemical) %>%
    collect() %>%
    pull(chemical) %>%
    sort()

    # Get unique regions for filtering, including the 'Other' category if present
  regions <- country_list %>%
    distinct(region) %>%
    pull(region) %>%
    sort()

  # Calculate min/max year directly from the full dataset
  year_range_data <- ds %>%
    summarise(
        min_year = min(year, na.rm = TRUE),
        max_year = max(year, na.rm = TRUE)
    ) %>%
    collect()

  return(list(
    data = ds,
    country_list = country_list,
    chemical_categories = chemical_categories,
    regions = regions, # Use regions derived from the processed country_list
    min_year = year_range_data$min_year,
    max_year = year_range_data$max_year
  ))
}

#' Create the initial Leaflet map
#'
#' @param selected_countries Vector of currently selected ISO codes
#' @param country_list Data frame with country info (iso2c, country, lat, lng, cc, region)
#' @param available_regions Vector of all possible region names for grouping
#' @return A Leaflet map object
#' @export
create_selection_map <- function(selected_countries = c(), country_list, available_regions) {
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Join map data with our country list to get consistent ISO codes and metadata
  # Use iso_a2 from world_map which corresponds to iso2c
  map_data <- world_map %>%
      select(iso_a2, name_long, geometry) %>% # Use name_long for potentially better matching
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      filter(!is.na(country)) %>% # Keep only countries present in our dataset
      mutate(region = factor(region, levels = available_regions)) # Ensure region is a factor with all levels

  # Define color palette
  pal <- colorFactor(
    palette = c("lightgray", "#3388ff"), # Gray for available, Blue for selected
    domain = c(FALSE, TRUE),
    na.color = "transparent" # Don't color polygons not in our data
  )

  # Create map
  map <- leaflet(map_data) %>%
    addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
             attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>') %>%
    setView(lng = 10, lat = 30, zoom = 2)

  # Add polygons by region group
  for (reg in available_regions) {
      region_data <- filter(map_data, region == reg)
      if (nrow(region_data) > 0) {
          map <- map %>% addPolygons(
              data = region_data,
              group = reg, # Assign polygon to its region group
              fillColor = ~pal(iso_a2 %in% selected_countries),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                  weight = 2,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.8,
                  bringToFront = TRUE
              ),
              label = ~paste(country), # Use country name from our joined list
              layerId = ~iso_a2,      # Use ISO code as layer ID
              labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
              )
          )
      }
  }

  # Add controls and search
  map %>%
    addLayersControl(
        overlayGroups = available_regions,
        options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE),
        position = "topright"
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("lightgray", "#3388ff"),
      labels = c("Available", "Selected"),
      title = "Country Status",
      opacity = 0.7
    ) %>%
    # Search should target the groups (regions)
    addSearchFeatures(
      targetGroups = available_regions, # Search within the region groups
      options = searchFeaturesOptions(
        propertyName = "label", # Search the label (country name)
        zoom = 5, # Zoom closer on search result
        openPopup = FALSE, # Don't need popup, highlight is enough
        # firstTip = "Search for a country...", # Placeholder text
        autoCollapse = TRUE,
        hideMarkerOnCollapse = TRUE,
        textPlaceholder = "Search country..." # Placeholder text
      )
    )
}

#' Update map polygons without redrawing the whole map
#'
#' @param map_proxy A leaflet proxy object
#' @param selected_countries Vector of currently selected ISO codes
#' @param country_list Data frame with country info
#' @param available_regions Vector of all possible region names
#' @return Updated leaflet proxy
#' @export
update_map_polygons <- function(map_proxy, selected_countries, country_list, available_regions) {
  # This function redraws polygons, which might interfere slightly with
  # show/hide group state if not managed carefully.
  # However, it's simpler than updating styles individually per polygon.
  # The region filtering logic in app.R will handle showing/hiding groups.

  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world_map %>%
      select(iso_a2, name_long, geometry) %>%
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      filter(!is.na(country)) %>%
      mutate(region = factor(region, levels = available_regions))

  pal <- colorFactor(
    palette = c("lightgray", "#3388ff"),
    domain = c(FALSE, TRUE),
    na.color = "transparent"
  )

  # Clear existing polygons first
  map_proxy %>% clearShapes()

  # Re-add polygons by region group with updated styles
  for (reg in available_regions) {
      region_data <- filter(map_data, region == reg)
      if (nrow(region_data) > 0) {
          map_proxy %>% addPolygons(
              data = region_data,
              group = reg, # Assign polygon to its region group
              fillColor = ~pal(iso_a2 %in% selected_countries),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                  weight = 2,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.8,
                  bringToFront = TRUE
              ),
              label = ~paste(country),
              layerId = ~iso_a2,
              labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
              )
          )
      }
  }
  # Note: We might need to re-apply show/hide logic after this, see app.R
  return(map_proxy)
}

#' Generate UI for displaying selected country information
#'
#' @param selected_isos Vector of selected ISO codes
#' @param country_list Data frame with country info
#' @return HTML tags for the sidebar
#' @export
get_selection_info_ui <- function(selected_isos, country_list) {
  if (length(selected_isos) == 0) {
    return(div(class = "alert alert-secondary", "No countries selected. Click on the map."))
  }

  country_names <- sapply(selected_isos, function(iso) {
    name <- country_list$country[country_list$iso2c == iso]
    if (length(name) == 0) iso else name # Fallback to ISO if not found
  })

  tags$div(
    h5("Current Selection:", class="mb-2"),
    tags$ul(
      class = "list-unstyled",
      lapply(country_names, function(name) tags$li(icon("flag"), name))
    ),
    if (length(selected_isos) > 1) {
      p(class="small text-muted", "Multiple countries selected. Choose display mode below the plot.")
    } else {
       p(class="small text-muted", "Showing individual contribution data.")
    }
  )
}

#' Generate dynamic plot header text
#'
#' @param selected_isos Vector of selected ISO codes
#' @param display_mode Current display mode ("individual", "compare_individuals", "find_collaborations")
#' @param chemical_category Selected chemical category
#' @param country_list Data frame with country info
#' @return HTML tag (h5) for the plot card header
#' @export
get_plot_header <- function(selected_isos, display_mode, chemical_category, country_list) {
  num_selected <- length(selected_isos)
  chem_text <- if (chemical_category == "All") "All Chemicals" else chemical_category

  title <- "Chemical Contribution Trends" # Default

  if (num_selected == 0) {
    title <- "Select Countries on the Map"
  } else if (num_selected == 1) {
    country_name <- country_list$country[country_list$iso2c == selected_isos[1]]
    title <- paste("Individual Contribution:", country_name, "-", chem_text)
  } else { # More than 1 selected
    if (display_mode == "compare_individuals") {
      title <- paste("Comparing Individual Contributions -", chem_text)
    } else if (display_mode == "find_collaborations") {
      title <- paste("Joint Collaborations Found -", chem_text)
    }
  }
  return(h5(title))
}


#' Fetch and process data based on user selections
#'
#' @param ds DuckDB relation (data source)
#' @param selected_isos Vector of selected ISO codes
#' @param year_range Vector c(min_year, max_year)
#' @param chemical_category Selected chemical category
#' @param display_mode Current display mode ("individual", "compare_individuals", "find_collaborations")
#' @param region_filter Selected region (or "All")
#' @param country_list Data frame with country info
#' @return A data frame ready for plotting/tabling
#' @export
get_display_data <- function(ds, selected_isos, year_range, chemical_category,
                             display_mode, region_filter = "All", country_list) {

  if (length(selected_isos) == 0) return(data.frame())

  base_query <- ds %>%
    filter(between(year, year_range[1], year_range[2]))

  # Apply chemical filter
  if (chemical_category != "All") {
    base_query <- base_query %>% filter(chemical == chemical_category)
  }

  # --- Logic based on display mode ---
  if (display_mode == "individual" || display_mode == "compare_individuals") {
    # Filter for individual data of selected countries
    result_data <- base_query %>%
      filter(is_collab == FALSE, iso2c %in% selected_isos)

    # Apply region filter if specified
    if (region_filter != "All") {
      result_data <- result_data %>% filter(region == region_filter)
    }

    # Collect necessary columns, including color code 'cc'
    result_data <- result_data %>%
      select(iso2c, year, percentage, chemical, cc, country, region) %>% # Ensure 'cc', 'country', 'region' are selected
      collect() %>%
      # Ensure correct types and prepare for plotting
      mutate(
          plot_group = iso2c, # Group lines/points by country ISO
          plot_color = cc,    # Use the country's color code
          tooltip_text = paste0(
              "<b>", country, "</b> (", iso2c, ")<br>",
              "<b>Region:</b> ", region, "<br>",
              "<b>Year:</b> ", year, "<br>",
              "<b>Chemical:</b> ", chemical, "<br>",
              "<b>Percentage:</b> ", scales::percent(percentage / 100, accuracy = 0.01)
          )
      ) %>%
      rename(total_percentage = percentage) # Use consistent naming

  } else if (display_mode == "find_collaborations") {
    # Filter for collaborations involving ALL selected countries
    collab_query <- base_query %>% filter(is_collab == TRUE)

    # Apply filter for each selected country
    for (iso in selected_isos) {
      collab_query <- collab_query %>% filter(grepl(iso, iso2c))
    }

    # Collect and process collaboration data
    collab_data <- collab_query %>%
      select(iso2c, year, percentage, chemical) %>%
      collect()

    # Further filter to ensure ALL countries are present (grepl might match subsets)
    if (nrow(collab_data) > 0) {
        result_data <- collab_data %>%
            mutate(partners = strsplit(as.character(iso2c), "-")) %>%
            filter(sapply(partners, function(p) all(selected_isos %in% p))) %>%
            # Define collaboration type
            mutate(
                collab_size = sapply(partners, length),
                collab_type = case_when(
                    collab_size == 2 ~ "Bilateral",
                    collab_size == 3 ~ "Trilateral",
                    collab_size == 4 ~ "4-country",
                    collab_size >= 5 ~ "5-country+",
                    TRUE ~ "Unknown"
                )
            ) %>%
            group_by(iso2c, year, chemical, collab_type) %>%
            summarise(total_percentage = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
            # Prepare for plotting
            mutate(
                plot_group = iso2c, # Group lines/points by collaboration ID
                plot_color = collab_type, # Color by collaboration type
                tooltip_text = paste0(
                    "<b>Collaboration:</b> ", iso2c, "<br>",
                    "<b>Type:</b> ", collab_type, "<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>Chemical:</b> ", chemical, "<br>",
                    "<b>Percentage:</b> ", scales::percent(total_percentage / 100, accuracy = 0.01)
                )
            )
    } else {
        result_data <- data.frame() # No collaborations found
    }
  } else {
    result_data <- data.frame() # Should not happen
  }

  # Ensure numeric types
  if (nrow(result_data) > 0) {
      result_data$year <- as.numeric(result_data$year)
      result_data$total_percentage <- as.numeric(result_data$total_percentage)
  }

  return(result_data)
}


#' Create the main plot based on processed data and display mode
#'
#' @param data The data frame returned by get_display_data
#' @param display_mode Current display mode
#' @param selected_isos Vector of selected ISO codes (used for titles etc.)
#' @param country_list Data frame with country info
#' @return A plotly object
#' @export
create_main_plot <- function(data, display_mode, selected_isos, country_list) {

  if (!nrow(data) > 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data to display for current selection."))
  }

  # Base plot
  p <- ggplot(data, aes(x = year, y = total_percentage, group = plot_group))

  # --- Aesthetics based on mode ---
  if (display_mode == "individual" || display_mode == "compare_individuals") {
    # Use country's own color, group by country
    p <- p +
      geom_line(aes(color = plot_group), linewidth = 0.5, alpha = 0.8) +
      geom_point(aes(color = plot_group, text = tooltip_text, size = total_percentage), alpha = 0.7)

    # Apply manual color scale using 'cc' values
    country_colors <- data %>% distinct(plot_group, plot_color)
    color_values <- setNames(country_colors$plot_color, country_colors$plot_group)
    # Add country names to the legend labels
    country_names_map <- setNames(country_list$country, country_list$iso2c)
    legend_labels <- setNames(country_names_map[names(color_values)], names(color_values))

    p <- p + scale_color_manual(values = color_values, name = "Country", labels = legend_labels)

  } else if (display_mode == "find_collaborations") {
    # Color by collaboration type, group by specific collaboration iso2c
    p <- p +
      geom_line(aes(color = plot_color), linewidth = 0.5, alpha = 0.8) + # plot_color is collab_type here
      geom_point(aes(color = plot_color, shape = plot_color, text = tooltip_text, size = total_percentage), alpha = 0.7) +
      scale_color_brewer(palette = "Set1", name = "Collaboration Type") +
      scale_shape_discrete(name = "Collaboration Type")
  }

  # --- Common plot elements ---
  p <- p +
    scale_radius(range = c(1, 6), name = "Contribution %") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.01, scale = 1),
      expand = expansion(mult = c(0.05, 0.15)) # Add padding
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    labs(x = "Year", y = "% Contribution") + # Simplified axis labels
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "#666666"),
      legend.key.size = unit(0.6, "lines"),
      legend.text = element_text(size = 8)
    )

  # Convert to plotly
  ggplotly(p, tooltip = "text") %>%
    layout(
      hoverlabel = list(bgcolor = "white", bordercolor = "black", font = list(size = 11)),
      legend = list(font = list(size = 9), itemsizing = 'constant', traceorder = 'normal')
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
}


#' Create a summary data table based on processed data
#'
#' @param data The data frame returned by get_display_data
#' @param display_mode Current display mode
#' @return A DT datatable object
#' @export
create_summary_table <- function(data, display_mode) {

  if (!nrow(data) > 0) {
    return(datatable(data.frame(Message = "No data available."), options = list(dom = 't'), rownames = FALSE))
  }

  if (display_mode == "individual" || display_mode == "compare_individuals") {
    summary_df <- data %>%
      group_by(iso2c, country, region, chemical) %>% # Group by country and chemical
      summarise(
        avg_percentage = mean(total_percentage, na.rm = TRUE),
        max_percentage = max(total_percentage, na.rm = TRUE),
        years_present = n_distinct(year),
        .groups = "drop"
      ) %>%
      arrange(country, desc(avg_percentage)) %>%
      mutate(
        avg_percentage = scales::percent(avg_percentage / 100, accuracy = 0.01),
        max_percentage = scales::percent(max_percentage / 100, accuracy = 0.01)
      ) %>%
      select(
        Country = country,
        ISO = iso2c,
        Region = region,
        `Chemical Category` = chemical,
        `Avg %` = avg_percentage,
        `Max %` = max_percentage,
        `Years Present` = years_present
      )

  } else if (display_mode == "find_collaborations") {
    summary_df <- data %>%
      group_by(iso2c, collab_type, chemical) %>% # Group by collaboration ID and chemical
      summarise(
        avg_percentage = mean(total_percentage, na.rm = TRUE),
        max_percentage = max(total_percentage, na.rm = TRUE),
        years_present = n_distinct(year),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_percentage)) %>%
      mutate(
        avg_percentage = scales::percent(avg_percentage / 100, accuracy = 0.01),
        max_percentage = scales::percent(max_percentage / 100, accuracy = 0.01)
      ) %>%
      select(
        Collaboration = iso2c,
        Type = collab_type,
        `Chemical Category` = chemical,
        `Avg %` = avg_percentage,
        `Max %` = max_percentage,
        `Years Present` = years_present
      )
  } else {
     return(datatable(data.frame(Message = "Invalid display mode."), options = list(dom = 't'), rownames = FALSE))
  }

  datatable(
    summary_df,
    options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 20),
      searchHighlight = TRUE,
      scrollX = TRUE # Enable horizontal scrolling if needed
    ),
    rownames = FALSE,
    filter = 'top' # Add column filters
  )
}


#' Calculate top contributing countries based on filters
#'
#' @param ds DuckDB relation
#' @param year_range Numeric vector c(min_year, max_year)
#' @param chemical_category String, chemical category filter
#' @param region_filter String, region filter
#' @param country_list Data frame with country info
#' @param top_n Integer, number of top contributors to return
#' @return Data frame with top N contributors (iso2c, country, avg_percentage)
#' @export
calculate_top_contributors <- function(ds, year_range, chemical_category, region_filter = "All", country_list, top_n = 10) {

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

    top_data <- query %>%
        group_by(iso2c) %>%
        summarise(avg_percentage = mean(percentage, na.rm = TRUE)) %>%
        collect() %>% # Collect aggregated data
        filter(!is.na(avg_percentage)) %>%
        arrange(desc(avg_percentage)) %>%
        head(top_n) %>%
        # Join with country_list to get full country names
        left_join(select(country_list, iso2c, country), by = "iso2c") %>%
        select(iso2c, country, avg_percentage) # Ensure correct columns

    return(top_data)
}