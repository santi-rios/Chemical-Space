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

    return(list(
        data = ds,
        country_list = country_list,
        chemical_categories = chemical_categories
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
process_collab_data <- function(ds, iso, year_range = c(1996, 2022),
                                data_type = "collaborations",
                                collab_types = "Bilateral",
                                chemical_category = "All",
                                country_list) {
    # Apply filters and process data
    base_query <- ds %>%
        filter(
            # Modified filter condition to handle different data types
            case_when(
                data_type == "collaborations" ~ is_collab == TRUE,
                data_type == "individual" ~ is_collab == FALSE,
                data_type == "both" ~ TRUE,
                TRUE ~ TRUE # Default case
            ),
            # Always filter by the selected country and year range
            grepl(iso, iso2c),
            between(year, year_range[1], year_range[2])
        )

    # Apply chemical filter if not "All"
    if (chemical_category != "All") {
        base_query <- base_query %>%
            filter(chemical == chemical_category)
    }

    # Collect filtered data
    base_data <- base_query %>%
        select(iso2c, year, percentage, chemical, is_collab) %>%
        collect()

    # Early exit if no data
    if (nrow(base_data) == 0) {
        return(data.frame())
    }

    # Process the data differently based on type
    if (data_type == "individual") {
        # For individual data, simple processing
        result <- base_data %>%
            mutate(
                collab_type = "Individual",
                partner_list = "None"
            ) %>%
            group_by(partner_list, collab_type, year, chemical) %>%
            summarise(
                total_percentage = sum(percentage, na.rm = TRUE),
                .groups = "drop"
            )
    } else {
        # For collaborations, use existing logic
        # Pre-filter by collaboration size if possible (optimization)
        if (data_type != "individual" && length(collab_types) > 0 && all(collab_types != "All")) {
            # Only apply this to collaboration data
            collab_data <- base_data %>%
                filter(is_collab == TRUE)

            # Rest of your existing collab filtering logic
            allowed_counts <- c()

            for (type in collab_types) {
                if (type == "Bilateral") {
                    allowed_counts <- c(allowed_counts, 2)
                } else if (type == "Trilateral") {
                    allowed_counts <- c(allowed_counts, 3)
                } else if (type == "4-country") {
                    allowed_counts <- c(allowed_counts, 4)
                } else if (type == "5-country+") allowed_counts <- c(allowed_counts, 5, 6, 7, 8, 9, 10)
            }

            # Count hyphens in iso2c to estimate partner count without expensive string operations
            collab_data <- collab_data %>%
                mutate(
                    hyphen_count = str_count(iso2c, "-"),
                    partner_count = hyphen_count + 1
                ) %>%
                filter(partner_count %in% allowed_counts) %>%
                select(-hyphen_count, -partner_count)

            # Individual data if needed
            if (data_type == "both") {
                indiv_data <- base_data %>%
                    filter(is_collab == FALSE) %>%
                    mutate(
                        collab_type = "Individual",
                        partner_list = "None"
                    )

                # Combine the datasets
                base_data <- bind_rows(collab_data, indiv_data)
            } else {
                base_data <- collab_data
            }
        }

        # Process collaborations
        collab_result <- base_data %>%
            filter(is_collab == TRUE) %>%
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
                })
            )

        # Apply collaboration type filter after full processing
        if (length(collab_types) > 0 && all(collab_types != "All")) {
            collab_result <- collab_result %>%
                filter(collab_type %in% collab_types)
        }

        # Process individual data if needed
        if (data_type %in% c("individual", "both")) {
            indiv_result <- base_data %>%
                filter(is_collab == FALSE) %>%
                mutate(
                    collab_type = "Individual",
                    partner_list = "None"
                )

            # Combine results
            if (data_type == "both" && nrow(collab_result) > 0) {
                result <- bind_rows(collab_result, indiv_result)
            } else {
                result <- if (data_type == "individual") indiv_result else collab_result
            }
        } else {
            result <- collab_result
        }

        # Group by the entire collaboration configuration
        result <- result %>%
            group_by(partner_list, collab_type, year, chemical) %>%
            summarise(
                total_percentage = sum(percentage, na.rm = TRUE),
                .groups = "drop"
            )
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

create_collab_plot <- function(data, country_name, collab_types = NULL, chemical_category = "All", data_type = "collaborations") {
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
            linewidth = 0.2, alpha = 0.6
        ) +
        geom_point(
            aes(
                size = total_percentage,
                color = partner_list,
                shape = collab_type,
                text = paste0(
                    "<b>", ifelse(collab_type == "Individual", country_name, partner_list), "</b><br>",
                    "<b>Type:</b> ", collab_type, "<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>Chemical Type:</b> ", chemical, "<br>",
                    "<b>Percentage:</b> ", scales::percent(total_percentage / 100, accuracy = 0.01)
                )
            ),
            alpha = 0.8
        ) +
        scale_color_viridis_d(
            option = "turbo",
            name = "Countries"
        ) +
        scale_shape_manual(
            values = shape_values,
            name = "Type"
        ) +
        scale_linetype_manual(
            values = linetype_values,
            name = "Type"
        ) +
        scale_radius(range = c(1, 6), name = "") +
        scale_y_continuous(
            labels = scales::percent_format(accuracy = 0.01, scale = 1),
            expand = expansion(mult = c(0.05, 0.15))
        ) +
        scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10)
        ) +
        labs(
            title = paste(data_type_text, "for", country_name, "-", chemical_text),
            subtitle = paste0(
                "Showing ", if (data_type == "individual") {
                    "individual contributions"
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

    # For individual data, highlight it more prominently
    if (data_type %in% c("individual", "both")) {
        indiv_data <- data %>% filter(collab_type == "Individual")
        if (nrow(indiv_data) > 0) {
            p <- p +
                geom_line(
                    data = indiv_data,
                    aes(x = year, y = total_percentage),
                    color = "black",
                    linewidth = 0.8,
                    linetype = "solid"
                ) +
                geom_point(
                    data = indiv_data,
                    aes(
                        x = year,
                        y = total_percentage,
                        text = paste0(
                            "<b>", country_name, " (Individual)</b><br>",
                            "<b>Year:</b> ", year, "<br>",
                            "<b>Chemical Type:</b> ", chemical, "<br>",
                            "<b>Percentage:</b> ", scales::percent(total_percentage / 100, accuracy = 0.01)
                        )
                    ),
                    color = "black",
                    size = 3,
                    shape = 19
                )
        }
    }

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
