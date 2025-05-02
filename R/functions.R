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

  # Optimized country list calculation (including lat/lng/cc/region/flags for map/plotting/UI)
  country_list <- ds %>%
    filter(is_collab == FALSE) %>%
    # Ensure flags column is selected if it exists
    distinct(country, iso2c, lat, lng, cc, region, flags) %>%
    filter(!is.na(country) & !is.na(iso2c) & country != "" & iso2c != "") %>%
    # Clean up flag URL quotes if present
    mutate(flags = gsub("\"", "", flags)) %>%
    arrange(country) %>%
    collect()

  # Get unique chemical categories
  chemical_categories <- ds %>%
    distinct(chemical) %>%
    collect() %>%
    pull(chemical) %>%
    sort()

  # Get unique regions for individual data filtering
  regions <- ds %>%
    filter(is_collab == FALSE, !is.na(region)) %>%
    distinct(region) %>%
    collect() %>%
    pull(region) %>%
    sort()

  return(list(
    data = ds,
    country_list = country_list,
    chemical_categories = chemical_categories,
    regions = regions
  ))
}

#' Create the initial Leaflet map
#'
#' @param selected_countries Vector of currently selected ISO codes
#' @param country_list Data frame with country info (iso2c, country, lat, lng, cc)
#' @return A Leaflet map object
#' @export
create_selection_map <- function(selected_countries = c(), country_list) {
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Join map data with our country list to get consistent ISO codes and metadata
  map_data <- world_map %>%
      select(iso_a2, name_long, geometry) %>% # Use name_long for better matching potential
      # Match iso_a2 which corresponds to iso2c
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      # Keep only countries present in our dataset
      filter(!is.na(country))

  # Define color palette
  pal <- colorFactor(
    palette = c("lightgray", "#3388ff"), # Gray for available, Blue for selected
    domain = c(FALSE, TRUE),
    na.color = "transparent" # Don't color polygons not in our data
  )

  leaflet(map_data) %>%
    addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
             attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>') %>%
    setView(lng = 10, lat = 30, zoom = 2) %>%
    addPolygons(
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
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("lightgray", "#3388ff"),
      labels = c("Available", "Selected"),
      title = "Country Status",
      opacity = 0.7
    )
}

#' Update map polygons without redrawing the whole map
#'
#' @param map_proxy A leaflet proxy object
#' @param selected_countries Vector of currently selected ISO codes
#' @param country_list Data frame with country info
#' @return Updated leaflet proxy
#' @export
update_map_polygons <- function(map_proxy, selected_countries, country_list) {
  # This function redraws polygons with updated styles based on selection
  # It assumes the underlying map shapes don't need changing, just their fill
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world_map %>%
      select(iso_a2, name_long, geometry) %>%
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      filter(!is.na(country)) # Ensure we only work with countries in our list

  pal <- colorFactor(
    palette = c("lightgray", "#3388ff"),
    domain = c(FALSE, TRUE),
    na.color = "transparent"
  )

  # Use setStyle for potentially better performance than clear/add
  # However, clearShapes/addPolygons is more robust if shapes could change
  # Sticking with clear/add for simplicity and robustness here.
  map_proxy %>%
    clearShapes() %>% # Clear previous polygons
    addPolygons(
      data = map_data,
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


#' Generate UI for displaying selected country information (with flags)
#'
#' @param selected_isos Vector of selected ISO codes
#' @param country_list Data frame with country info (must include 'flags' column)
#' @return HTML tags for the sidebar
#' @export
get_selection_info_ui <- function(selected_isos, country_list) {
  if (length(selected_isos) == 0) {
    return(div(class = "alert alert-secondary mt-2", "No countries selected."))
  }

  selected_info <- country_list %>% filter(iso2c %in% selected_isos)

  tags$div(
    class="mt-2", # Add margin top
    # h5("Current Selection:", class="mb-1"), # Optional header
    tags$ul(
      class = "list-unstyled",
      style = "max-height: 150px; overflow-y: auto;", # Limit height and allow scroll
      lapply(1:nrow(selected_info), function(i) {
        tags$li(
          # Add flag image if URL exists
          if (!is.na(selected_info$flags[i]) && selected_info$flags[i] != "") {
            tags$img(src = selected_info$flags[i], height = "12px", style = "margin-right: 5px; vertical-align: middle;")
          } else {
            # Placeholder or icon if no flag
            icon("flag", style = "margin-right: 5px; vertical-align: middle;")
          },
          selected_info$country[i] # Country name
        )
      })
    ),
    if (length(selected_isos) > 1) {
      p(class="small text-muted mt-1", "Multiple countries selected. Choose display mode below the plot.")
    } else {
       p(class="small text-muted mt-1", "Showing individual contribution data.")
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
    title <- "Select Countries on the Map or via Search"
  } else if (num_selected == 1) {
    country_name <- country_list$country[country_list$iso2c == selected_isos[1]]
    title <- paste("Individual Contribution:", country_name, "-", chem_text)
  } else { # More than 1 selected
    if (display_mode == "compare_individuals") {
      title <- paste("Comparing Individual Contributions -", chem_text)
    } else if (display_mode == "find_collaborations") {
      # Generate country names string for title
      collab_countries <- country_list %>%
          filter(iso2c %in% selected_isos) %>%
          pull(country) %>%
          paste(collapse = ", ")
      title <- paste("Joint Collaborations:", collab_countries, "-", chem_text)
    }
  }
  return(h5(title, class="card-title")) # Use standard card title class
}


#' Fetch and process data based on user selections
#'
#' @param ds DuckDB relation (data source)
#' @param selected_isos Vector of selected ISO codes
#' @param year_range Vector c(min_year, max_year)
#' @param chemical_category Selected chemical category
#' @param display_mode Current display mode ("individual", "compare_individuals", "find_collaborations")
#' @param region_filter Selected region (or "All") - **Crucially, only applied for individual modes**
#' @param country_list Data frame with country info
#' @return A data frame ready for plotting/tabling, or an empty df if no data
#' @export
get_display_data <- function(ds, selected_isos, year_range, chemical_category,
                             display_mode, region_filter = "All", country_list) {

  # Return empty data frame immediately if no countries are selected
  if (length(selected_isos) == 0) return(data.frame())

  # --- Base Filtering (Year and Chemical) ---
  base_query <- ds %>%
    filter(between(year, year_range[1], year_range[2]))

  if (chemical_category != "All") {
    base_query <- base_query %>% filter(chemical == chemical_category)
  }

  # --- Logic based on display mode ---
  if (display_mode == "individual" || display_mode == "compare_individuals") {
    # Filter for individual data of selected countries
    ind_query <- base_query %>%
      filter(is_collab == FALSE, iso2c %in% selected_isos)

    # Apply region filter *only* to this individual data query
    if (region_filter != "All") {
      ind_query <- ind_query %>% filter(region == region_filter)
    }

    # Collect necessary columns, including color code 'cc', region, country
    result_data <- ind_query %>%
      select(iso2c, year, percentage, chemical, cc, country, region) %>%
      collect()

    # Check if data was found after filtering
    if (nrow(result_data) == 0) return(data.frame())

    # Prepare for plotting
    result_data <- result_data %>%
      mutate(
          plot_group = iso2c, # Group lines/points by country ISO
          plot_color = cc,    # Use the country's color code
          tooltip_text = paste0(
              "<b>", country, "</b> (", iso2c, ")<br>",
              "<b>Region:</b> ", region, "<br>",
              "<b>Year:</b> ", year, "<br>",
              "<b>Chemical:</b> ", chemical, "<br>",
              "<b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01) # Already a percentage
          )
      ) %>%
      rename(total_percentage = percentage) # Use consistent naming

  } else if (display_mode == "find_collaborations") {
    # Filter for collaborations involving ALL selected countries
    # Region filter is NOT applied here as collaborations don't have a single region
    collab_query <- base_query %>% filter(is_collab == TRUE)

    # Efficiently filter collaborations containing all selected ISOs
    # Create a regex pattern like "(?=.*ISO1)(?=.*ISO2)"
    pattern <- paste0("(?=.*", paste(selected_isos, collapse = ")(?=.*"), ")")
    collab_query <- collab_query %>% filter(grepl(pattern, iso2c, perl = TRUE))

    # Collect and process collaboration data
    collab_data <- collab_query %>%
      select(iso2c, year, percentage, chemical) %>%
      collect()

    # Check if data was found
    if (nrow(collab_data) == 0) return(data.frame())

    # Further process collaboration data
    result_data <- collab_data %>%
        mutate(partners = strsplit(as.character(iso2c), "-")) %>%
        # Double check: Ensure the number of partners matches the number of selected isos
        # AND that all selected isos are indeed present (grepl can have edge cases)
        filter(sapply(partners, length) == length(selected_isos)) %>%
        filter(sapply(partners, function(p) all(selected_isos %in% p)))

    # Check again if data remains after strict filtering
    if (nrow(result_data) == 0) return(data.frame())

    result_data <- result_data %>%
        # Define collaboration type based on the number of selected countries
        mutate(
            collab_size = length(selected_isos), # Size is based on the selection
            collab_type = case_when(
                collab_size == 2 ~ "Bilateral",
                collab_size == 3 ~ "Trilateral",
                collab_size == 4 ~ "4-country",
                collab_size >= 5 ~ "5-country+",
                TRUE ~ "Unknown" # Should not happen if selected_isos > 1
            )
        ) %>%
        # Group and summarise if multiple entries exist for the same collab-year-chem
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
                "<b>Percentage:</b> ", scales::percent(total_percentage, accuracy = 0.01) # Already a percentage
            )
        )

  } else {
    # Should not happen with current logic, but return empty df just in case
    result_data <- data.frame()
  }

  # Final check and type conversion before returning
  if (nrow(result_data) > 0) {
      result_data$year <- as.numeric(result_data$year)
      # Ensure total_percentage is numeric, handle potential NA/Inf
      result_data$total_percentage <- as.numeric(result_data$total_percentage)
      result_data <- result_data %>% filter(is.finite(total_percentage))
  } else {
      # Ensure an empty data frame with correct columns is returned if no data
      # This helps prevent errors in downstream functions like ggplot
       expected_cols <- c("iso2c", "year", "total_percentage", "chemical", "plot_group", "plot_color", "tooltip_text")
       # Add individual specific columns if needed
       if(display_mode %in% c("individual", "compare_individuals")) {
           expected_cols <- c(expected_cols, "cc", "country", "region")
       } else if (display_mode == "find_collaborations") {
           expected_cols <- c(expected_cols, "collab_type")
       }
       # Create empty df with those names
       result_data <- setNames(data.frame(matrix(ncol = length(expected_cols), nrow = 0)), expected_cols)
       # Ensure correct types for key columns even when empty
       result_data <- result_data %>% mutate(year = numeric(), total_percentage = numeric())

  }


  return(result_data)
}


#' Create the main plot based on processed data and display mode
#'
#' @param data The data frame returned by get_display_data (can be empty)
#' @param display_mode Current display mode
#' @param selected_isos Vector of selected ISO codes (used for titles etc.)
#' @param country_list Data frame with country info
#' @return A plotly object
#' @export
create_main_plot <- function(data, display_mode, selected_isos, country_list) {

  # Handle empty data case gracefully
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "No data to display for current selection.", y=0.5),
                    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
  }

  # Base plot
  # Ensure total_percentage is treated as the value it represents (e.g., 0.28 not 28)
  p <- ggplot(data, aes(x = year, y = total_percentage, group = plot_group))

  # --- Aesthetics based on mode ---
  if (display_mode == "individual" || display_mode == "compare_individuals") {
    # Use country's own color, group by country
    p <- p +
      geom_line(aes(color = plot_group), linewidth = 0.5, alpha = 0.8) +
      # Ensure size mapping uses the percentage value directly
      geom_point(aes(color = plot_group, text = tooltip_text, size = total_percentage), alpha = 0.7)

    # Apply manual color scale using 'cc' values
    # Need to handle cases where a selected country might have been filtered out (e.g., by region)
    present_countries <- unique(data$plot_group)
    country_colors_data <- country_list %>% filter(iso2c %in% present_countries) %>% select(iso2c, cc)
    if(nrow(country_colors_data) > 0) {
        color_values <- setNames(country_colors_data$cc, country_colors_data$iso2c)
        # Add country names to the legend labels
        country_names_map <- setNames(country_list$country, country_list$iso2c)
        legend_labels <- setNames(country_names_map[names(color_values)], names(color_values))
        p <- p + scale_color_manual(values = color_values, name = "Country", labels = legend_labels)
    } else {
         # Fallback if no colors found (shouldn't happen if data exists)
         p <- p + scale_color_discrete(name = "Country")
    }


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
    # Adjust size scale if needed, name it appropriately
    scale_radius(range = c(1, 8), name = "Contribution (%)") +
    scale_y_continuous(
      # Format y-axis as percentage
      labels = scales::percent_format(accuracy = 0.01),
      expand = expansion(mult = c(0.05, 0.15)) # Add padding
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    labs(x = "Year", y = "% Contribution") + # Simplified axis labels
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 9),
      plot.title = element_blank(), # Remove ggplot title, handled by card header
      plot.subtitle = element_blank(),
      legend.key.size = unit(0.6, "lines"),
      legend.text = element_text(size = 8)
    )

  # Convert to plotly
  # Suppress warnings during conversion if needed, e.g., about unknown aesthetics
  # p_plotly <- suppressWarnings(ggplotly(p, tooltip = "text"))
  p_plotly <- ggplotly(p, tooltip = "text")

  p_plotly %>%
    layout(
      hoverlabel = list(bgcolor = "white", bordercolor = "black", font = list(size = 11)),
      legend = list(font = list(size = 9), itemsizing = 'constant', traceorder = 'normal')
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
}


#' Create a summary data table based on processed data (FIXED)
#'
#' @param data The data frame returned by get_display_data
#' @param display_mode Current display mode
#' @param country_list Data frame with country info (for flags)
#' @return A DT datatable object
#' @export
create_summary_table <- function(data, display_mode, country_list) {

  # Handle empty data case
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(datatable(data.frame(Message = "No data available for table."), options = list(dom = 't'), rownames = FALSE))
  }

  # --- Logic based on display mode ---
  if (display_mode == "individual" || display_mode == "compare_individuals") {
    # Ensure necessary columns exist from get_display_data
    req_cols <- c("iso2c", "country", "region", "chemical", "total_percentage", "year")
    if (!all(req_cols %in% names(data))) {
         return(datatable(data.frame(Message = "Missing required columns for individual summary."), options = list(dom = 't'), rownames = FALSE))
    }

    summary_df <- data %>%
      group_by(iso2c, country, region, chemical) %>% # Group by country and chemical
      summarise(
        # Calculate summary stats - use total_percentage which is already scaled
        avg_percentage = mean(total_percentage, na.rm = TRUE),
        max_percentage = max(total_percentage, na.rm = TRUE),
        years_present = n_distinct(year),
        .groups = "drop"
      ) %>%
      arrange(country, desc(avg_percentage)) %>%
      # Join with country list to get flags
      left_join(select(country_list, iso2c, flags), by = "iso2c") %>%
      # Create flag HTML column
      mutate(
        flag_html = ifelse(!is.na(flags) & flags != "",
                           paste0('<img src="', flags, '" height="16px">'),
                           ""),
        # Format percentages
        avg_percentage_fmt = scales::percent(avg_percentage, accuracy = 0.01),
        max_percentage_fmt = scales::percent(max_percentage, accuracy = 0.01)
      ) %>%
      select(
        Flag = flag_html,
        Country = country,
        ISO = iso2c,
        Region = region,
        `Chemical Category` = chemical,
        `Avg %` = avg_percentage_fmt,
        `Max %` = max_percentage_fmt,
        `Years Present` = years_present
      )

  } else if (display_mode == "find_collaborations") {
     req_cols <- c("iso2c", "collab_type", "chemical", "total_percentage", "year")
     if (!all(req_cols %in% names(data))) {
         return(datatable(data.frame(Message = "Missing required columns for collaboration summary."), options = list(dom = 't'), rownames = FALSE))
     }

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
        # Format percentages
        avg_percentage_fmt = scales::percent(avg_percentage, accuracy = 0.01),
        max_percentage_fmt = scales::percent(max_percentage, accuracy = 0.01)
      ) %>%
      select(
        Collaboration = iso2c,
        Type = collab_type,
        `Chemical Category` = chemical,
        `Avg %` = avg_percentage_fmt,
        `Max %` = max_percentage_fmt,
        `Years Present` = years_present
      )
  } else {
     # Should not happen
     return(datatable(data.frame(Message = "Invalid display mode for summary table."), options = list(dom = 't'), rownames = FALSE))
  }

  # Final check if summary_df is valid
  if (!is.data.frame(summary_df) || nrow(summary_df) == 0) {
       return(datatable(data.frame(Message = "No data available for table after processing."), options = list(dom = 't'), rownames = FALSE))
  }

  datatable(
    summary_df,
    options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15),
      searchHighlight = TRUE,
      scrollX = TRUE # Enable horizontal scrolling if needed
    ),
    rownames = FALSE,
    escape = FALSE, # IMPORTANT: Render HTML for flags
    filter = 'top' # Add column filters
  )
}


#' Calculate top contributing countries based on filters (Region filter fixed)
#'
#' @param ds DuckDB relation
#' @param year_range Numeric vector c(min_year, max_year)
#' @param chemical_category String, chemical category filter
#' @param region_filter String, region filter (**Applied correctly**)
#' @param country_list Data frame with country info
#' @param top_n Integer, number of top contributors to return
#' @return Data frame with top N contributors (iso2c, country, avg_percentage)
#' @export
calculate_top_contributors <- function(ds, year_range, chemical_category, region_filter = "All", country_list, top_n = 10) {

    # Start with base query on individual data
    query <- ds %>%
        filter(
            is_collab == FALSE,
            between(year, year_range[1], year_range[2])
        )

    # Apply chemical filter
    if (chemical_category != "All") {
        query <- query %>% filter(chemical == chemical_category)
    }

    # Apply region filter *before* aggregation
    if (region_filter != "All") {
        query <- query %>% filter(region == region_filter)
    }

    # Aggregate, collect, arrange, and join
    top_data <- query %>%
        group_by(iso2c) %>%
        # Calculate mean percentage (already scaled 0-1)
        summarise(avg_percentage = mean(percentage, na.rm = TRUE)) %>%
        collect() %>% # Collect aggregated data
        filter(!is.na(avg_percentage) & is.finite(avg_percentage)) %>% # Ensure valid numbers
        arrange(desc(avg_percentage)) %>%
        head(top_n)

    # Check if any data remains
    if(nrow(top_data) == 0) {
        return(data.frame(iso2c=character(), country=character(), avg_percentage=numeric()))
    }

    # Join with country_list to get full country names
    top_data <- top_data %>%
        left_join(select(country_list, iso2c, country), by = "iso2c") %>%
        # Ensure correct columns and order, handle potential missing country names
        mutate(country = ifelse(is.na(country), iso2c, country)) %>%
        select(iso2c, country, avg_percentage)

    return(top_data)
}