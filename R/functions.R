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

  # --- Load Static Article Data ---
  # Assuming columns like source, year_x, country_x, percentage_x exist
  # Adjust column names if they are different in your parquet file
  article_data_raw <- ds %>%
    # Use the correct column names from your parquet file here
    select(source, year = year_x, country = country_x, value = percentage_x) %>%
    filter(!is.na(value) & !is.na(source) & source != "") %>% # Filter out rows with missing essential data
    collect() # Collect this relatively small, static dataset

  return(list(
    data = ds,
    country_list = country_list,
    chemical_categories = chemical_categories,
    regions = regions,
    min_year = year_range_data$min_year,
    max_year = year_range_data$max_year,
    article_data = article_data_raw # Add the collected article data
  ))
}

# ... (load_country_data function) ...

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
  # Ensure 'cc' column is present and has a fallback for safety
  map_data <- world_map %>%
      select(iso_a2, name_long, geometry) %>%
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      filter(!is.na(country)) %>% # Keep only countries present in our dataset
      mutate(
          region = factor(region, levels = available_regions),
          # Provide a default color if 'cc' is missing for some reason
          cc = ifelse(is.na(cc) | cc == "", "#808080", cc) # Default to gray if cc is missing
      )

  # Define the 'available' color
  available_color <- "lightgray"

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
              # --- Updated fillColor logic ---
              fillColor = ~ifelse(iso_a2 %in% selected_countries, cc, available_color),
              # --- End Updated fillColor logic ---
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                  weight = 2,
                  color = "#666", # Highlight border color
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
    # OPTIONAL: Add region groups to the map
    # addLayersControl(
    #     overlayGroups = available_regions,
    #     options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE),
    #     position = "topright"
    # ) %>%
    # --- Simplified Legend ---
    addLegend(
      position = "bottomright",
      colors = c(available_color), # Only show available color
      labels = c("Available"),     # Only show available label
      title = "Country Status",
      opacity = 0.7
    ) %>%
    # --- End Simplified Legend ---
    # Search should target the groups (regions)
    addSearchFeatures(
      targetGroups = available_regions, # Search within the region groups
      options = searchFeaturesOptions(
        propertyName = "label", # Search the label (country name)
        zoom = 5, # Zoom closer on search result
        openPopup = FALSE, # Don't need popup, highlight is enough
        autoCollapse = TRUE,
        hideMarkerOnCollapse = TRUE,
        textPlaceholder = "Search country..." # Placeholder text
      )
    )
}

# ... (create_selection_map function) ...

#' Update map polygons without redrawing the whole map
#'
#' @param map_proxy A leaflet proxy object
#' @param selected_countries Vector of currently selected ISO codes
#' @param country_list Data frame with country info
#' @param available_regions Vector of all possible region names
#' @return Updated leaflet proxy
#' @export
update_map_polygons <- function(map_proxy, selected_countries, country_list, available_regions) {
  # This function redraws polygons. The region filtering logic in app.R
  # should handle showing/hiding groups correctly afterwards.

  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world_map %>%
      select(iso_a2, name_long, geometry) %>%
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      filter(!is.na(country)) %>%
      mutate(
          region = factor(region, levels = available_regions),
          cc = ifelse(is.na(cc) | cc == "", "#808080", cc) # Default color
      )

  available_color <- "lightgray"

  # Clear existing shapes first to avoid layering issues
  map_proxy %>% clearShapes()

  # Re-add polygons by region group with updated styles
  for (reg in available_regions) {
      region_data <- filter(map_data, region == reg)
      if (nrow(region_data) > 0) {
          map_proxy %>% addPolygons(
              data = region_data,
              group = reg, # Assign polygon to its region group
              # --- Use the same updated fillColor logic ---
              fillColor = ~ifelse(iso_a2 %in% selected_countries, cc, available_color),
              # --- End updated logic ---
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
  return(map_proxy)
}

# ... (rest of functions.R) ...

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
    title <- paste("Countrywise expansion of the chemical space:", country_name, "-", chem_text)
  } else { # More than 1 selected
    if (display_mode == "compare_individuals") {
      title <- paste("Comparing Countrywise expansion of the chemical space -", chem_text)
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
  # if (chemical_category != "All") {
    base_query <- base_query %>% filter(chemical == chemical_category)
  # }

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
          plot_group = country, # Group lines/points by country ISO
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
      select(iso2c, country, year, percentage, chemical) %>%
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
            group_by(iso2c, year, chemical, collab_type, country) %>%
            summarise(total_percentage = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
            # Prepare for plotting
            mutate(
                plot_group = country, # Group lines/points by collaboration ID
                plot_color = collab_type, # Color by collaboration type
                tooltip_text = paste0(
                    "<b>Collaboration:</b> ", country, "<br>",
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
      geom_point(aes(color = plot_group, text = tooltip_text, size = total_percentage), alpha = 0.7) +
      labs(x = "Year", y = "% of new substances contributed by country")
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
      labs(x = "Year", y = "% of new substances contributed by each collaboration") +
      scale_color_brewer(palette = "Set1", name = "Collaboration Type") +
      scale_shape_discrete(name = "Collaboration Type") +
      guides(color = "none")
  }

  # --- Common plot elements ---
  p <- p +
    scale_radius(range = c(1, 6), name = "") +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      expand = expansion(mult = c(0.05, 0.15)) # Add padding
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
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
    filter = 'none' # Add column filters
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
#' @param ignore_year_filter Logical, if TRUE, the year_range filter is skipped. Defaults to FALSE.
#' @return Data frame with top N contributors (iso2c, country, avg_percentage), sorted by avg_percentage.
#' @export
calculate_top_contributors <- function(ds, year_range, chemical_category, region_filter = "All", country_list, top_n = 10, ignore_year_filter = FALSE) {

    query <- ds %>%
        filter(is_collab == FALSE) # Always filter for solo contributions

    # Conditionally apply year filter
    if (!ignore_year_filter) {
        query <- query %>% filter(between(year, year_range[1], year_range[2]))
    }

    # Apply other filters
    if (chemical_category != "All") {
        query <- query %>% filter(chemical == chemical_category)
    }

    if (region_filter != "All") {
        query <- query %>% filter(region == region_filter)
    }

    top_data <- query %>%
        group_by(iso2c) %>%
        summarise(avg_percentage = mean(percentage, na.rm = TRUE)) %>%
        collect() %>%
        filter(!is.na(avg_percentage), avg_percentage > 0) %>% # Also filter out zero averages
        arrange(desc(avg_percentage)) %>%
        head(top_n) %>%
        # Add rank after sorting
        mutate(rank = row_number()) %>%
        # Join with country_list to get full country names
        left_join(select(country_list, iso2c, country), by = "iso2c") %>%
        # Ensure correct columns and order
        select(rank, iso2c, country, avg_percentage)

    return(top_data)
}

#' Create Static Contribution Map Plot
#'
#' Generates a choropleth map showing the average contribution percentage
#' over the selected period for the countries present in the input data.
#'
#' @param processed_data_df Data frame output from get_display_data (must contain iso2c, country, region, year, total_percentage).
#' @param fill_label Character. Legend title for fill variable.
#' @param main_title Character. Main plot title.
#' @return A plotly object showing the static world map plot.
#' @import ggplot2 dplyr sf rnaturalearth scales plotly
#' @export
create_contribution_map_plot <- function(processed_data_df,
                                         fill_label = "Average Contribution (%)",
                                         main_title = "Average Contribution Over Selected Period") {

  if (!nrow(processed_data_df) > 0 || !"total_percentage" %in% names(processed_data_df)) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data for map."))
  }

  # Calculate average percentage and best/worst year per country
  map_summary_data <- processed_data_df %>%
    group_by(iso2c, country, region) %>%
    summarise(
      avg_percentage = mean(total_percentage, na.rm = TRUE),
      best_year = year[which.max(total_percentage)],
      worst_year = year[which.min(total_percentage)],
      .groups = "drop"
    ) %>%
    filter(!is.na(avg_percentage)) # Ensure we have a value to plot

  if (!nrow(map_summary_data) > 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No average data to display on map."))
  }

  # --- Discrete Color Scale Logic (adapted from example) ---
  max_val <- max(map_summary_data$avg_percentage, na.rm = TRUE)
  ceiling_val <- ceiling(max_val / 5) * 5 # Round up to nearest 5

  if (ceiling_val <= 0) ceiling_val <- 5 # Handle case where max is 0 or negative

  if (ceiling_val <= 5) {
    breaks <- seq(0, ceiling_val, by = 1)
    label_fmt <- "%.1f-%.1f"
  } else if (ceiling_val <= 20) {
    breaks <- seq(0, ceiling_val, by = 2.5)
    label_fmt <- "%.1f-%.1f"
  } else {
    breaks <- seq(0, ceiling_val, by = 5)
    label_fmt <- "%.0f-%.0f"
  }
  # Ensure breaks cover the max value if ceiling logic was imperfect
  if (max(breaks) < max_val) breaks <- c(breaks, max(breaks) + (breaks[2]-breaks[1]))

  labels <- character(length(breaks) - 1)
  for (i in 1:(length(breaks) - 1)) {
    labels[i] <- sprintf(label_fmt, breaks[i], breaks[i+1])
  }
  # --- End Color Scale Logic ---

  # Get world map data
  world_map_sf <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
      select(iso_a2, name_long, geometry) # Use iso_a2 for joining

  # Join map data with summary data
  plot_data_sf <- world_map_sf %>%
    left_join(map_summary_data, by = c("iso_a2" = "iso2c")) %>%
    # Filter antartica and other non-country geometries
    filter(!is.na(name_long) & !name_long %in% c("Antarctica", "Greenland")) %>%
    mutate(
      # --- Conditionally create tooltip text ---
      tooltip_text = if_else(
        !is.na(avg_percentage), # Only create text if avg_percentage is NOT NA
        paste0(
          "<b>", name_long, "</b> (", iso_a2, ")<br>",
          "Avg Contribution: ", scales::percent(avg_percentage / 100, accuracy = 0.01), "<br>",
          "Region: ", coalesce(region, "N/A"), "<br>",
          "Best Year: ", coalesce(as.character(best_year), "N/A"), "<br>",
          "Worst Year: ", coalesce(as.character(worst_year), "N/A")
        ),
        NA_character_ # Set to NA if avg_percentage IS NA
      ),
      # --- Keep other mutate steps ---
      fill_discrete = cut(
        avg_percentage,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        right = FALSE
      )
      # formatted_value is no longer needed here if only used in tooltip
    )

  
    # Create plot using geom_sf
    p <- ggplot(plot_data_sf) +
      geom_sf(aes(fill = fill_discrete, text = tooltip_text), # tooltip_text is now NA for grey countries
              color = "black", linewidth = 0.1) +
      # ... (rest of scale_fill_brewer, labs, coord_sf, theme) ...
      scale_fill_brewer(
        palette = "Spectral", # Example Brewer palette
        direction = - 1,
        name = fill_label,
        na.value = "#EEEEEE", # Color for countries with no data
        drop = FALSE # Keep all levels in the legend
      ) +
      labs(title = main_title) +
      coord_sf(crs = "+proj=robin") + # Keep projection if desired
      theme_void() + # Minimal theme
      theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.8, "lines"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
      ) +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

    # Convert to plotly - it should ignore NA text values
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.05, x = 0.5, xanchor = "center"),
        margin = list(b = 50, t = 40, l = 10, r = 10)
      ) %>%
      layout(paper_bgcolor='rgba(0,0,0,0)', plot_bgcolor='rgba(0,0,0,0)')
  
  }
  
  # ... (rest of functions.R) ...

#' Create Simplified Plot for Static Article Data (using ggplot2 + ggplotly or pure Plotly)
#'
#' Generates a plot for the pre-loaded article data.
#' Uses ggplot2 + ggplotly for most plots, but pure Plotly for the dual-axis "China-US" plot.
#' Handles basic formatting and dual-axis for specific sources.
#'
#' @param article_df Data frame containing the static article data for a specific source.
#' @param source_title Character. The 'source' identifier (e.g., "Annual growth rate of the GDP").
#' @param y_title Character. Label for the Y-axis.
#' @param animate Logical. Should the plot be animated by year? Defaults to FALSE (Animation only supported for ggplotly plots).
#' @return A plotly object.
#' @import ggplot2 plotly dplyr scales RColorBrewer
#' @export
create_article_plot_simple <- function(article_df, source_title, y_title, animate = FALSE) {

  if (!nrow(article_df) > 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data for this figure."))
  }

  # --- Define Consistent Colors ---
  country_colors <- c(
    "China" = "#c5051b", "China alone" = "#c56a75", "China w/o US" = "#9b2610",
    "France" = "#0a3161", "Germany" = "#000000", "India" = "#ff671f",
    "Japan" = "#000091", "Russia" = "#d51e9b", "USA alone" = "#3b5091",
    "USA w/o China" = "#006341", "United Kingdom" = "#74acdf", "United States" = "#002852",
    "All substances" = "#4879a7", "Organic Chemicals" = "#ff6d45",
    "Organometallics" = "#55713e", "Rare-Earths" = "#800525",
    "CN-US collab/CN" = "#6d2f56", "CN-US collab/US" = "#ff8888", "US" = "#002853",
  )

  available_items <- unique(article_df$country)
  plot_colors <- country_colors[names(country_colors) %in% available_items]
  missing_items <- setdiff(available_items, names(plot_colors))
  if (length(missing_items) > 0) {
    extra_colors <- suppressWarnings(
        colorRampPalette(RColorBrewer::brewer.pal(max(3, length(missing_items)), "Set2"))(length(missing_items))
    )
    names(extra_colors) <- missing_items
    plot_colors <- c(plot_colors, extra_colors)
  }

  # --- Data Prep & Formatting based on Source ---
  y_format_func <- scales::label_number(accuracy = 0.1) # Default format
  y_sec_title <- NULL # For secondary axis
  y_sec_format_func <- NULL
  y_limits <- NULL
  y_sec_limits <- NULL

  plot_data <- article_df %>%
      mutate(
          # Calculate plot_value based on source
          plot_value = case_when(
              source_title == "Number of Researchers" ~ value / 1e6,
              # Keep original values (0-100) for China-US plot
              source_title == "China-US in the CS" ~ value,
              # Keep original value for counts
              source_title %in% c("Expansion of the CS", "Country participation in the CS") ~ value,
              # Assume others are percentages, convert to 0-1 scale for ggplotly
              TRUE ~ value / 100
          ),
          # Format value string for tooltips
          formatted_value_str = case_when(
              source_title == "Number of Researchers" ~ scales::comma(value, accuracy = 1),
              source_title == "Annual growth rate of the GDP" ~ paste0(scales::comma(value, accuracy = 0.1), "%"),
              source_title %in% c("Expansion of the CS", "Country participation in the CS") ~ scales::comma(value, accuracy = 1),
              source_title == "China-US in the CS" ~ scales::percent(value / 100, accuracy = 0.1), # Assuming input is 0-100
              TRUE ~ scales::percent(value / 100, accuracy = 0.1) # Assuming input is 0-100
          ),
          tooltip_text = paste0(
              "<b>", country, "</b><br>",
              "Year: ", year, "<br>",
              "", formatted_value_str
          )
      ) %>%
      arrange(country, year)

  # Adjust titles and formats based on source
  if (source_title == "Number of Researchers") {
    y_title <- paste0(y_title, " (Millions)")
    y_format_func <- scales::label_number(accuracy = 0.01)
  } else if (source_title == "Annual growth rate of the GDP") {
    y_title <- y_title # Already includes %
    y_format_func <- scales::label_number(accuracy = 0.1, suffix = "%")
    # Adjust plot_value if input is not already scaled 0-100 for ggplotly
    plot_data <- plot_data %>% mutate(plot_value = value) # Assuming input is already correct scale for ggplotly
  } else if (source_title %in% c("Expansion of the CS", "Country participation in the CS")) {
    y_format_func <- scales::label_comma(accuracy = 1)
    y_title <- y_title
  } else if (source_title == "China-US in the CS") {
    # Dual Axis Setup (Specific values for pure Plotly)
    y_title <- "Contribution Share (%)" # Primary axis title
    y_limits <- c(60, 100) # Primary axis limits (original scale 0-100)

    y_sec_title <- "Collaboration Share (%)"
    y_sec_limits <- c(0, 20) # Secondary axis limits (original scale 0-100)
    # No rescaling needed for pure Plotly dual axis approach
  } else {
     # Default percentage formatting for ggplotly (needs 0-1 scale)
     y_format_func <- scales::label_percent(accuracy = 0.1)
  }

  # --- Plotting Logic ---

  # --- Option 1: Pure Plotly for "China-US in the CS" ---
  if (source_title == "China-US in the CS") {
    main_countries <- c("China", "United States")
    collab_countries <- c("CN-US collab/CN", "CN-US collab/US")

    p <- plot_ly()

    # Add traces for main countries (Primary Y-axis)
    for (ctry in main_countries) {
      df_ctry <- plot_data %>% filter(country == ctry)
      p <- p %>% add_trace(
        data = df_ctry,
        x = ~year,
        y = ~plot_value, # Use original value (0-100)
        type = 'scatter',
        mode = 'lines+markers',
        name = ctry,
        line = list(color = plot_colors[[ctry]], width = 2),
        marker = list(color = plot_colors[[ctry]], size = 6, opacity = 0.8),
        yaxis = "y1",
        hoverinfo = 'text',
        text = ~tooltip_text
      )
    }

    # Add traces for collaboration countries (Secondary Y-axis)
    for (ctry in collab_countries) {
      df_ctry <- plot_data %>% filter(country == ctry)
      p <- p %>% add_trace(
        data = df_ctry,
        x = ~year,
        y = ~plot_value, # Use original value (0-100)
        type = 'scatter',
        mode = 'lines+markers',
        name = ctry,
        line = list(color = plot_colors[[ctry]], width = 2, dash = 'dot'),
        marker = list(color = plot_colors[[ctry]], size = 5, opacity = 0.8, symbol = 'triangle-up'),
        yaxis = "y2", # Assign to secondary axis
        hoverinfo = 'text',
        text = ~tooltip_text
      )
    }

    # Configure layout for dual axes
    p <- p %>% layout(
      title = list(text = paste("Figure:", source_title), x = 0.5, font = list(size = 14, face = "bold")),
      xaxis = list(title = "Year", gridcolor = "#e8e8e8"),
      yaxis = list(
        title = y_title,
        range = y_limits,
        tickformat = '.0f', # Format as integer percentage
        ticksuffix = "%",
        gridcolor = "#e8e8e8",
        side = 'left'
      ),
      yaxis2 = list(
        title = y_sec_title,
        range = y_sec_limits,
        tickformat = '.0f', # Format as integer percentage
        ticksuffix = "%",
        overlaying = "y",
        side = "right",
        showgrid = FALSE # Avoid overlapping grids
      ),
      legend = list(orientation = 'h', y = -0.15),
      hovermode = "closest",
      hoverlabel = list(bgcolor = "white", font = list(size = 11)),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#ffffff",
      margin = list(l = 60, r = 80, b = 80, t = 50) # Ensure space for right axis
    )

  # --- Option 2: ggplotly for all other plots ---
  } else {
    # Define frame aesthetic for ggplotly animation
    frame_aes <- if (animate) aes(frame = year) else aes()

    # Base ggplot object
    gg <- ggplot(plot_data, aes(x = year, group = country, text = tooltip_text)) +
          frame_aes # Add frame aesthetic here

    # Standard Plot layers
    gg <- gg +
      geom_line(aes(y = plot_value, color = country), linewidth = 0.3, show.legend = FALSE) +
      geom_point(aes(y = plot_value, color = country, size = plot_value), alpha = 0.6, show.legend = FALSE) +
      scale_size_area(max_size = 5) +
      scale_y_continuous(name = y_title, labels = y_format_func, limits = y_limits) # Use potentially scaled plot_value (e.g., 0-1 for %)

    # Common ggplot Elements
    gg <- gg +
      scale_color_manual(values = plot_colors, name = "Country") +
      labs(
        # title = paste("Figure:", source_title),
        x = element_blank(),
        caption = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.grid.major = element_line(color = "#e8e8e8"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      )

    # Add GDP Annotations if applicable
    if (source_title == "Annual growth rate of the GDP") {
      gg <- gg +
        geom_vline(xintercept = 2007.5, linetype = "dashed", color = "grey", linewidth = 0.5) +
        geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey", linewidth = 0.5) +
        annotate("text", x = 2007.5, y = 11, label = "Financial Crisis", hjust = -0.1, vjust = 1.5, size = 3, color = "#000000") +
        annotate("text", x = 2019.5, y = 7, label = "COVID-19", hjust = -0.1, vjust = 1.5, size = 3, color = "#000000")
    }

    # Convert ggplot to plotly
    p <- ggplotly(gg, tooltip = "text") %>%
      config(displayModeBar = FALSE)

    # Apply Plotly Layout/Animation adjustments
    p <- p %>% layout(
      hovermode = "closest",
      hoverlabel = list(bgcolor = "white", font = list(size = 11)),
      legend = list(orientation = 'h', y = -0.15),
      xaxis = list(gridcolor = "#e8e8e8"),
      yaxis = list(gridcolor = "#e8e8e8"),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#ffffff",
      margin = list(l = 60, r = 40, b = 80, t = 50)
    ) 

    # Add Animation Controls if requested
    if (animate) {
      p <- p %>%
        animation_opts(
          frame = 100,
          transition = 50,
          redraw = FALSE
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Year: ", font = list(color="darkgrey"))
        ) %>%
        animation_button(
          x = 0, xanchor = "left", y = -0.2, yanchor = "bottom"
        )
    }
  } # End if/else for plotting logic

  # Apply final config to the resulting plotly object 'p'
  # p %>% config(displayModeBar = TRUE, displaylogo = FALSE)
  p
}

# ... (existing functions) ...

#' Create Plot for Top Collaboration Trends
#'
#' Generates a ggplot line/dot plot (converted to Plotly) showing the percentage
#' contribution over time for the top N collaborations.
#'
#' @param top_collab_data Data frame containing year, percentage, and collaboration identifier (iso2c/country)
#'                         for the top N collaborations. Must include columns: year, percentage, country (as collab ID).
#' @param title Character. The main title for the plot.
#' @return A plotly object.
#' @import ggplot2 plotly dplyr scales RColorBrewer
#' @export
create_top_collabs_plot <- function(top_collab_data, title = "Collaboration Trends") {

  if (!nrow(top_collab_data) > 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No collaboration data to display."))
  }

  # Ensure correct types and prepare tooltips
  plot_data <- top_collab_data %>%
    mutate(
      year = as.numeric(year),
      percentage = as.numeric(percentage),
      tooltip_text = paste0(
        "<b>Collaboration:</b> ", country, "<br>",
        "<b>Year:</b> ", year, "<br>",
        "<b>Percentage:</b> ", scales::percent(percentage / 100, accuracy = 0.01)
      )
    ) %>%
    # Arrange for correct line drawing
    arrange(country, year)

  # Determine number of unique collaborations for color palette
  num_collabs <- n_distinct(plot_data$country)
  # Use a color palette suitable for distinct categories
  collab_colors <- suppressWarnings(
      colorRampPalette(RColorBrewer::brewer.pal(min(9, max(3, num_collabs)), "Set1"))(num_collabs)
  )
  names(collab_colors) <- unique(plot_data$country)


  # Base ggplot object
  gg <- ggplot(plot_data, aes(x = year, y = percentage, group = country, color = country, text = tooltip_text)) +
    geom_line(linewidth = 0.4, alpha = 0.4) +
    geom_point(size = 2.5, alpha = 0.6) +
    scale_y_continuous(
        name = "% of new substances contributed",
        labels = scales::percent_format(scale = 1, accuracy = 0.1) # Assuming input is 0-100
    ) +
    scale_color_manual(values = collab_colors, name = "") +
    labs(
      title = title,
      x = element_blank(),
      caption = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      panel.grid.major = element_line(color = "#e8e8e8"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f8f9fa", color = NA),
      panel.background = element_rect(fill = "#f8f9fa", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = 8), # Smaller text if many collaborations
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )

  # Convert to Plotly
  p <- ggplotly(gg, tooltip = "text") %>%
    layout(
      hovermode = "closest",
      hoverlabel = list(bgcolor = "white", font = list(size = 11)),
      legend = list(orientation = 'h', y = -0.2), # Adjust legend position if needed
      xaxis = list(gridcolor = "#e8e8e8"),
      yaxis = list(gridcolor = "#e8e8e8"),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#ffffff",
      margin = list(l = 60, r = 40, b = 100, t = 50) # Increase bottom margin for legend
    ) %>%
    config(displayModeBar = FALSE)

  return(p)
}

# ... (rest of functions.R) ...