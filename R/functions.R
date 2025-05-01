# Add chemical category filter to relevant functions

#' Load and prepare country data
#'
#' @param data_path Path to the parquet file
#' @return A list containing the data source and country list
#' @export
load_country_data <- function(data_path = "./data/data.parquet") {
  # Read data using duckplyr's optimized parquet reader
  ds <- read_parquet_duckdb(
    path = data_path,
    prudence = "lavish" # Materialize data for testing
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
    
  # Get unique regions for individual data filtering
  regions <- ds %>%
    filter(is_collab == FALSE) %>%
    distinct(region) %>%
    collect() %>%
    pull(region) %>%
    sort()
  
  # Add "All" as the first option
  regions <- c("All", regions[!is.na(regions)])
  
  return(list(
    data = ds,
    country_list = country_list,
    chemical_categories = chemical_categories,
    regions = regions
  ))
}

# Add a data_type parameter to your functions

#' Process country data for single country or collaborations
#'
#' @param ds Data source
#' @param iso Selected country ISO code
#' @param year_range Range of years to filter (vector of min and max)
#' @param data_type Type of data to show: "collaborations", "individual", or "both"
#' @param collab_types Vector of collaboration types to include ("Bilateral", "Trilateral", etc.)
#' @param chemical_category Chemical category to filter by
#' @param country_list Lookup table for country codes and names
#' @return Processed data ready for plotting
#' @export
# Update the process_collab_data function to handle multiple countries

# Fix the process_collab_data function to handle cc/country_color correctly

process_collab_data <- function(ds, iso, year_range = c(1996, 2022),
                              data_type = "collaborations",
                              collab_types = "Bilateral",
                              chemical_category = "All",
                              region_filter = NULL,
                              country_list) {
  # Apply filters and process data
  base_query <- ds
  
  # Separate handling for individual vs collaboration data
  if (data_type == "collaborations") {
    # For collaboration data - single country
    base_query <- base_query %>%
      filter(
        is_collab == TRUE,
        grepl(iso[1], iso2c), # Use the first country if multiple are selected
        between(year, year_range[1], year_range[2])
      )
  } else if (data_type == "individual") {
    # For individual data - multiple countries possible
    base_query <- base_query %>%
      filter(
        is_collab == FALSE,
        iso2c %in% iso, # Support multiple countries
        between(year, year_range[1], year_range[2])
      )
      
    # Apply region filter if provided
    if (!is.null(region_filter) && region_filter != "All") {
      base_query <- base_query %>% filter(region == region_filter)
    }
  } else {
    # For "both" data type
    if (length(iso) > 1) iso <- iso[1] # Only use first country for "both" mode
    
    # Filter for both individual and collaboration data
    base_query <- base_query %>%
      filter(
        # For collaborations, use grepl to match within multi-country strings
        # For individual, use direct ISO code matching
        (is_collab == TRUE & grepl(iso, iso2c)) | 
          (is_collab == FALSE & iso2c == iso),
        between(year, year_range[1], year_range[2])
      )
  }
  
  # Apply chemical filter if not "All"
  if (chemical_category != "All") {
    base_query <- base_query %>%
      filter(chemical == chemical_category)
  }
  
  # Collect filtered data including cc and region fields
  base_data <- base_query %>%
    select(iso2c, year, percentage, chemical, is_collab, cc, region) %>%
    collect()
  
  # Early exit if no data
  if (nrow(base_data) == 0) return(data.frame())
  
  # Process the data based on type
  if (data_type == "individual") {
    # For individual data, use iso2c directly as partner_list
    result <- base_data %>%
      mutate(
        collab_type = "Individual",
        partner_list = iso2c, # Use country code directly
        country_color = cc    # Preserve color code
      ) %>%
      group_by(partner_list, collab_type, year, chemical, country_color, region) %>%
      summarise(
        total_percentage = sum(percentage, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Process only collaboration data if we're in collaboration mode
    if (data_type == "collaborations") {
      collab_data <- base_data %>% filter(is_collab == TRUE)
    } else {
      # For "both" mode, split the data
      collab_data <- base_data %>% filter(is_collab == TRUE)
      indiv_data <- base_data %>% filter(is_collab == FALSE)
    }
    
    # Only process collaborations if we have any
    if (nrow(collab_data) > 0) {
      # Apply collaboration filter if needed
      if (length(collab_types) > 0 && all(collab_types != "All")) {
        allowed_counts <- c()
        
        for (type in collab_types) {
          if (type == "Bilateral") allowed_counts <- c(allowed_counts, 2)
          else if (type == "Trilateral") allowed_counts <- c(allowed_counts, 3)
          else if (type == "4-country") allowed_counts <- c(allowed_counts, 4)
          else if (type == "5-country+") allowed_counts <- c(allowed_counts, 5, 6, 7, 8, 9, 10)
        }
        
        collab_data <- collab_data %>%
          mutate(
            hyphen_count = str_count(iso2c, "-"),
            partner_count = hyphen_count + 1
          ) %>%
          filter(partner_count %in% allowed_counts) %>%
          select(-hyphen_count, -partner_count)
      }
      
      # Process collaborations
      collab_result <- collab_data %>%
        mutate(
          # Split the collaboration string
          partners = strsplit(as.character(iso2c), "-"),
          # Count number of countries in collaboration
          collab_size = sapply(partners, length),
          # Is this a bilateral or multi-country collaboration?
          collab_type = case_when(
            collab_size == 2 ~ "Bilateral",
            collab_size == 3 ~ "Trilateral",
            collab_size == 4 ~ "4-country",
            collab_size >= 5 ~ "5-country+",
            TRUE ~ "Unknown"
          ),
          # Extract all partners except the selected country
          partner_list = sapply(partners, function(x) {
            paste(setdiff(x, iso), collapse = ", ")
          }),
          # Initialize country_color as NA for collaborations
          country_color = NA_character_
        ) %>%
        group_by(partner_list, collab_type, year, chemical, country_color) %>%
        summarise(
          total_percentage = sum(percentage, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      collab_result <- data.frame() # Empty if no collaboration data
    }
    
    # Process individual data for "both" mode
    if (data_type == "both" && exists("indiv_data") && nrow(indiv_data) > 0) {
      indiv_result <- indiv_data %>%
        mutate(
          collab_type = "Individual",
          partner_list = iso2c,
          country_color = cc
        ) %>%
        group_by(partner_list, collab_type, year, chemical, country_color) %>%
        summarise(
          total_percentage = sum(percentage, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Combine results if we have both types
      if (nrow(collab_result) > 0) {
        result <- bind_rows(collab_result, indiv_result)
      } else {
        result <- indiv_result
      }
    } else {
      # Just use collaboration results
      result <- collab_result
    }
  }
  
  # Ensure all numeric columns are actually numeric
  result$total_percentage <- as.numeric(result$total_percentage)
  result$year <- as.numeric(result$year)
  
  return(result)
}

#' Find collaborations between specific countries
#'
#' @param ds Data source
#' @param countries Vector of country ISO2 codes to search for in collaborations
#' @param year_range Range of years to filter
#' @param chemical_category Chemical category to filter by
#' @param country_list Lookup table for country codes and names
#' @return Data frame of collaborations involving all specified countries
#' @export
find_specific_collaborations <- function(ds, countries,
                                         year_range = c(1996, 2022),
                                         chemical_category = "All",
                                         country_list) {
    if (length(countries) == 0) {
        return(data.frame())
    }

    # Create a proper filter for multiple countries
    withProgress(message = "Searching for collaborations...", {
        # First query - filter for records that might contain all countries
        base_query <- ds %>%
            filter(
                is_collab == TRUE,
                between(year, year_range[1], year_range[2])
            )

        # Apply chemical filter if not "All"
        if (chemical_category != "All") {
            base_query <- base_query %>%
                filter(chemical == chemical_category)
        }

        # Apply filters one by one to avoid the grepl vector issue
        for (country_code in countries) {
            # Use a proper single-value pattern for each iteration
            pattern <- country_code
            base_query <- base_query %>%
                filter(grepl(pattern, iso2c))
        }

        # Execute query and collect results
        base_data <- base_query %>%
            select(iso2c, year, percentage, chemical) %>%
            collect()

        # Early exit if no data
        if (nrow(base_data) == 0) {
            return(data.frame())
        }

        # Further process to confirm all countries are in each collaboration
        result <- base_data %>%
            mutate(
                # Split the collaboration string
                partners = strsplit(as.character(iso2c), "-"),
                # Count number of countries in collaboration
                collab_size = sapply(partners, length),
                # Check if all specified countries are included
                all_match = sapply(partners, function(x) all(countries %in% x)),
                # Create collab type classification
                collab_type = case_when(
                    collab_size == 2 ~ "Bilateral",
                    collab_size == 3 ~ "Trilateral",
                    collab_size == 4 ~ "4-country",
                    collab_size >= 5 ~ "5-country+",
                    TRUE ~ "Unknown"
                )
            ) %>%
            filter(all_match) %>%
            group_by(iso2c, collab_type, year, chemical) %>%
            summarise(
                total_percentage = sum(percentage, na.rm = TRUE),
                .groups = "drop"
            )

        # Ensure all numeric columns are actually numeric
        result$total_percentage <- as.numeric(result$total_percentage)
        result$year <- as.numeric(result$year)

        return(result)
    })
}

# Update the create_collab_plot function to handle individual data

# Update create_collab_plot function to use cc column for colors

create_collab_plot <- function(data, country_name, collab_types = NULL, 
                              chemical_category = "All", data_type = "collaborations") {
  # Count unique collaborations for subtitle
  collab_count <- nrow(distinct(data, partner_list))
  
  # Set up multi-type title
  type_text <- if (length(collab_types) == 1) {
    collab_types
  } else if (length(collab_types) > 1) {
    paste(collab_types, collapse = ", ")
  } else {
    "All"
  }
  
  # Add data type to title
  data_type_text <- case_when(
    data_type == "collaborations" ~ "Collaborations",
    data_type == "individual" ~ "Individual Contributions",
    data_type == "both" ~ "Individual & Collaborations",
    TRUE ~ "Data"
  )
  
  # Format country name for title (could be multiple for individual data)
  title_country <- if (data_type == "individual" && is.list(country_name)) {
    if (length(country_name) > 2) {
      paste0(country_name[1], ", ", country_name[2], " and ", length(country_name)-2, " others")
    } else {
      paste(country_name, collapse = " & ")
    }
  } else {
    country_name  # Single country name
  }
  
  # Add chemical category to title
  chemical_text <- if (chemical_category == "All") {
    "All Chemicals"
  } else {
    chemical_category
  }
  
  # Define shape and linetype scales based on available types
  available_types <- unique(data$collab_type)
  
  # Add Individual to the shape and linetype values
  shape_values <- c("Individual" = 19, "Bilateral" = 16, "Trilateral" = 17, "4-country" = 15, "5-country+" = 18)
  linetype_values <- c("Individual" = "solid", "Bilateral" = "solid", "Trilateral" = "dashed", "4-country" = "dotted", "5-country+" = "longdash")
  
  # Filter to only use values present in the data
  shape_values <- shape_values[names(shape_values) %in% available_types]
  linetype_values <- linetype_values[names(linetype_values) %in% available_types]
  
  # Create the plot
  p <- ggplot(data, aes(x = year, y = total_percentage)) +
    geom_line(aes(color = partner_list, linetype = collab_type), 
              linewidth = 0.2, alpha = 0.6) +
    geom_point(
      aes(
        size = total_percentage,
        color = partner_list,
        shape = collab_type,
        text = paste0(
          "<b>", partner_list, "</b><br>",
          "<b>Type:</b> ", collab_type, "<br>",
          "<b>Year:</b> ", year, "<br>",
          "<b>Chemical Type:</b> ", chemical, "<br>",
          "<b>Percentage:</b> ", scales::percent(total_percentage/100, accuracy = 0.01)
        )
      ),
      alpha = 0.8
    )
  
  # For individual data, use the country's own colors if available
  # For individual data, use the country's own colors if available
if (data_type == "individual" && "country_color" %in% colnames(data)) {
  # Extract the color mapping from the data
  color_mapping <- unique(data[, c("partner_list", "country_color")])
  # Remove any NA colors
  color_mapping <- color_mapping[!is.na(color_mapping$country_color),]
  
  if (nrow(color_mapping) > 0) {
    # Create color mapping
    color_values <- setNames(color_mapping$country_color, color_mapping$partner_list)
    p <- p + scale_color_manual(values = color_values, name = "Country")
  } else {
    # Fallback to default color scale
    p <- p + scale_color_viridis_d(option = "turbo", name = "Countries")
  }
} else {
  # Use default color scale for collaborations or when country_color is missing
  p <- p + scale_color_viridis_d(option = "turbo", name = "Partner Countries")
}
  
  # Complete the plot
  p <- p +
    scale_shape_manual(values = shape_values, name = "Type") +
    scale_linetype_manual(values = linetype_values, name = "Type") +
    scale_radius(range = c(1, 6), name = "") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.01, scale = 1),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(
      title = paste(data_type_text, "for", title_country, "-", chemical_text),
      subtitle = paste0(
        "Showing ", if(data_type == "individual") {
          paste0(length(unique(data$partner_list)), " countries")
        } else {
          paste0(collab_count, " unique ", type_text, ifelse(data_type == "both", " collaborations + individual", " collaborations"))
        }
      ),
      x = "Year",
      y = "% of substances contributed"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 8, color = "#666666"),
      legend.key.size = unit(0.5, "lines"),
      legend.text = element_text(size = 8)
    )
  
  # Convert to plotly with improved tooltip handling
  ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(family = "Arial", size = 12)
      ),
      legend = list(
        font = list(size = 9),
        itemsizing = "constant"
      )
    ) %>%
    config(displayModeBar = TRUE)
}

#' Create specific collaboration plot
#'
#' @param data Processed collaboration data
#' @param countries Vector of country ISO codes
#' @param country_list Country lookup table
#' @param chemical_category Chemical category being displayed
#' @return A plotly object
#' @export
create_specific_collab_plot <- function(data, countries, country_list, chemical_category = "All") {
    if (nrow(data) == 0) {
        return(plot_ly() %>%
            add_annotations(
                text = "No collaborations found involving all selected countries.",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 16)
            ))
    }

    # Get country names for display
    country_names <- sapply(countries, function(iso) {
        match_idx <- match(iso, country_list$iso2c)
        if (!is.na(match_idx)) country_list$country[match_idx] else iso
    })

    # Add chemical category to title
    chemical_text <- if (chemical_category == "All") {
        "All Chemicals"
    } else {
        chemical_category
    }

    # Create title text
    if (length(country_names) > 3) {
        title_text <- paste0(
            "Collaborations between ",
            paste(country_names[1:2], collapse = ", "),
            " and ", length(country_names) - 2, " other countries"
        )
    } else {
        title_text <- paste("Collaborations between", paste(country_names, collapse = ", "))
    }

    # Create the visualization
    p <- ggplot(data, aes(x = year, y = total_percentage)) +
        geom_line(linewidth = 0.5) +
        geom_point(
            aes(
                size = total_percentage,
                color = collab_type,
                text = paste0(
                    "<b>Collaboration:</b> ", iso2c, "<br>",
                    "<b>Type:</b> ", collab_type, "<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>Chemical Type:</b> ", chemical, "<br>",
                    "<b>Percentage:</b> ", scales::percent(total_percentage / 100, accuracy = 0.01)
                )
            ),
            alpha = 0.8
        ) +
        scale_color_brewer(
            palette = "Set1",
            name = "Collaboration Type"
        ) +
        scale_radius(range = c(2, 8), name = "") +
        scale_y_continuous(
            labels = scales::percent_format(accuracy = 0.01, scale = 1),
            expand = expansion(mult = c(0.05, 0.15))
        ) +
        scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10)
        ) +
        labs(
            title = paste(title_text, "-", chemical_text),
            subtitle = paste0(
                "Found ", nrow(data), " collaboration instances over time"
            ),
            x = "Year",
            y = "% of substances contributed"
        ) +
        theme_minimal() +
        theme(
            legend.position = "right",
            legend.title = element_text(face = "bold", size = 10),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(size = 8, color = "#666666"),
            legend.key.size = unit(0.5, "lines"),
            legend.text = element_text(size = 8)
        )

    # Convert to plotly with improved tooltip handling
    ggplotly(p, tooltip = "text") %>%
        plotly::layout(
            hoverlabel = list(
                bgcolor = "white",
                bordercolor = "black",
                font = list(family = "Arial", size = 12)
            ),
            legend = list(
                font = list(size = 9),
                itemsizing = "constant"
            )
        ) %>%
        config(displayModeBar = TRUE)
}
