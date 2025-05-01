# Chemical Space Collaboration Explorer
# 2025-05-01
# Optimized by Santiago Garcia Rios

# Load required packages
library(shiny)
library(bslib)
library(shinycssloaders)
library(plotly)
library(ggplot2)
library(conflicted)
library(duckplyr)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(DT)
library(gt)
library(stringr)
library(leaflet) # For interactive maps
library(sf) # For spatial data handling
library(rnaturalearth) # For world map data

# Resolve conflicts
conflict_prefer("filter", "dplyr", quiet = TRUE)
theme_set(theme_light())

# Source helper functions
source("R/functions.R")

# Load data
data_objects <- load_country_data("./data/data.parquet")
ds <- data_objects$data
country_list <- data_objects$country_list
chemical_categories <- data_objects$chemical_categories
regions <- data_objects$regions

# Create UI with tabsets for different views
ui <- page_navbar(
  title = "Chemical Space Collaboration Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id = "nav",

  # First tab - Country explorer (original view)
  nav_panel(
    title = "Country Explorer",
    icon = icon("chart-line"),
    page_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Controls",
        # uiOutput("country_select_ui"),
        selectizeInput(
          "country_select", "Select Country:",
          choices  = NULL,
          # multiple = FALSE,
          options = list(
            placeholder      = "Search for a country"
            # onInitialize     = I('function() { this.setValue(""); }')
          )
        ),
        hr(),
                radioButtons(
          "data_type", "Data Type:",
          choices = c(
            "Collaborations" = "collaborations",
            "Individual Contributions" = "individual",
            "Both" = "both"
          ),
          selected = "individual"
        ),
        hr(),
        conditionalPanel(
          condition = "input.data_type == 'individual'",
          selectInput(
            "region_filter", "Region:",
            choices = c("All", regions),
            selected = "All"
          )
        ),
        sliderInput(
          "years", "Year Range:",
          min = 1996, max = 2022,
          value = c(1996, 2022),
          step = 1, sep = ""
        ),
        hr(),
        radioButtons(
          "chemical_category", "Chemical Category:",
          choices = chemical_categories,
          selected = "All"
        ),
        hr(),
        conditionalPanel(
          condition = "input.data_type != 'individual'",
          checkboxGroupInput(
            "collab_types", "Collaboration Types:",
            choices = c(
              "Bilateral" = "Bilateral",
              "Trilateral" = "Trilateral",
              "4-country" = "4-country",
              "5 or more countries" = "5-country+"
            ),
            selected = "Bilateral"
          ),
          actionButton(
            "select_all", "View All Types",
            icon = icon("check-double"),
            class = "btn-outline-primary btn-sm",
            width = "100%"
          )
        ),
        hr(),
        helpText(
          HTML("<strong>About the data:</strong><br>"),
          "Each point represents a unique collaboration configuration involving the selected country.",
          HTML("<br><br><strong>Performance tip:</strong> Select fewer collaboration types for faster loading and clearer visualization.")
        )
      ),
      verticalLayout(
        card(
          full_screen = TRUE,
          card_header("Collaboration Visualization"),
          uiOutput("plot_info"),
          withSpinner(plotlyOutput("collab_plot", height = "500px"))
        ),
        card(
          full_screen = TRUE,
          card_header("Collaboration Summary"),
          DTOutput("summary_table")
        )
      )
    )
  ),

  # Second tab - Map-based country selection for specific collaborations
  nav_panel(
    title = "Multi-Country Search",
    icon = icon("globe"),
    page_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Selected Countries",
        div(
          style = "padding: 10px 0;",
          p("Click countries on the map to select/deselect them. The visualization will show collaborations involving ALL selected countries."),
          hr()
        ),
        uiOutput("selected_countries_ui"),
        hr(),
        actionButton(
          "clear_selection", "Clear Selection",
          icon = icon("trash-alt"),
          class = "btn-outline-danger btn-sm",
          width = "100%"
        ),
        hr(),
        sliderInput(
          "map_years", "Year Range:",
          min = 1996, max = 2022,
          value = c(1996, 2022),
          step = 1, sep = ""
        ),
        radioButtons(
          "map_chemical_category", "Chemical Category:",
          choices = chemical_categories,
          selected = "All"
        ),
        hr(),
        helpText(
          HTML("<strong>How to use:</strong><br>"),
          "1. Click countries on the map to select them",
          HTML("<br>"),
          "2. The visualization will update to show only collaborations where ALL selected countries participated together."
        )
      ),
      verticalLayout(
        card(
          full_screen = TRUE,
          card_header("Country Selection Map"),
          leafletOutput("selection_map", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header("Multi-Country Collaboration Results"),
          withSpinner(plotlyOutput("specific_collab_plot", height = "400px")),
          hr(),
          DTOutput("specific_collab_table")
        )
      )
    )
  ),
  footer = div(
    style = "text-align: center; padding: 10px; background: #f8f9fa;",
    "Source: Bermúdez-Montaña, M., et al. (2025). China's rise in the chemical space and the decline of US influence.",
    a("Working Paper", href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6")
  )
)

# Define server
server <- function(input, output, session) {

  # Update country select input with country list
  updateSelectizeInput(session, "country_select", choices = setNames(country_list$iso2c, country_list$country), server = TRUE)


  ## as soon as data_type settles to "individual", set region to "All"
  observeEvent(input$data_type, {
    if (input$data_type == "individual") {
      updateSelectInput(session,
                        "region_filter",
                        selected = "All")
    }
  }, ignoreInit = FALSE)

  #----------------------
  # Country Explorer Tab
  #----------------------

  # Toggle all collaboration types
  observeEvent(input$select_all, {
    all_types <- c("Bilateral", "Trilateral", "4-country", "5-country+")
    if (length(input$collab_types) < length(all_types)) {
      updateCheckboxGroupInput(session, "collab_types", selected = all_types)
    } else {
      updateCheckboxGroupInput(session, "collab_types", selected = "Bilateral")
    }
  })

  # Get the country name for the selected ISO code
  selected_country_name <- reactive({
    req(input$country_select)

    if (input$data_type == "individual" && length(input$country_select) > 1) {
      # Multiple countries selected - get all names
      country_names <- sapply(input$country_select, function(iso) {
        country_match <- match(iso, country_list$iso2c)
        if (!is.na(country_match)) {
          country_list$country[country_match]
        } else {
          iso
        }
      })
      return(country_names)
    } else {
      # Single country - return as before
      iso <- input$country_select
      if (length(iso) > 1) iso <- iso[1] # Take first if multiple

      country_match <- match(iso, country_list$iso2c)
      if (!is.na(country_match)) {
        country_list$country[country_match]
      } else {
        iso
      }
    }
  })

  # Process data for selected country with collaboration type filtering
  # Update collaboration_data reactive expression

  # Update the collaboration_data reactive expression

  # Process data for selected country with collaboration type filtering
  collaboration_data <- reactive({
    req(input$country_select, input$years, input$chemical_category, input$data_type)

    # For collaboration mode, require collab_types
    if (input$data_type != "individual" && length(input$collab_types) == 0) {
      showNotification("Please select at least one collaboration type", type = "warning")
      return(data.frame())
    }

    # Get region filter if in individual mode
    region_filter <- if (input$data_type == "individual" && !is.null(input$region_filter)) {
      input$region_filter
    } else {
      NULL
    }

    # Show progress indication
    withProgress(message = "Loading data...", {
      process_collab_data(
        ds = ds,
        iso = input$country_select,
        year_range = input$years,
        data_type = input$data_type,
        collab_types = input$collab_types,
        chemical_category = input$chemical_category,
        region_filter = region_filter,
        country_list = country_list
      )
    })
  }) %>%
    bindCache(
      input$country_select, input$years, input$collab_types,
      input$chemical_category, input$data_type, input$region_filter
    ) %>%
    bindEvent(
      input$country_select, input$years, input$collab_types,
      input$chemical_category, input$data_type, input$region_filter
    )
  # Display information about the current view

  output$plot_info <- renderUI({
    req(collaboration_data())

    if (nrow(collaboration_data()) == 0) {
      return(div(
        class = "alert alert-warning",
        "No collaboration data available for the selected criteria."
      ))
    }

    # Count the different types of collaborations
    collab_counts <- collaboration_data() %>%
      count(collab_type) %>%
      arrange(desc(n))

    # Create the info text with counts for each type
    type_info <- paste(
      collab_counts$collab_type,
      " (", collab_counts$n, ")",
      sep = "",
      collapse = ", "
    )

    div(
      class = "alert alert-info",
      HTML(paste0(
        "<strong>Data summary:</strong> Found ", nrow(distinct(collaboration_data(), partner_list)),
        " unique collaboration configurations for ", selected_country_name(),
        " with selected filters.<br>",
        "<strong>Chemical category:</strong> ", input$chemical_category, "<br>",
        "<strong>Collaboration types:</strong> ", type_info
      ))
    )
  })

  # Create collaboration plot
  output$collab_plot <- renderPlotly({
    req(collaboration_data())
    validate(need(
      nrow(collaboration_data()) > 0,
      "No data available for this country with the selected filters."
    ))

    create_collab_plot(
      collaboration_data(),
      selected_country_name(),
      input$collab_types,
      input$chemical_category,
      input$data_type
    )
  })

  # Create summary table
  # Update the summary_table output rendering

  # Create summary table
  output$summary_table <- renderDT({
    req(collaboration_data())
    validate(need(nrow(collaboration_data()) > 0, "No collaboration data available."))

    # Create summary table with chemical category
    summary_data <- collaboration_data() %>%
      group_by(partner_list, collab_type, chemical) %>%
      summarize(
        avg_percentage = mean(total_percentage, na.rm = TRUE),
        max_percentage = max(total_percentage, na.rm = TRUE),
        years_present = n_distinct(year)
      ) %>%
      arrange(desc(avg_percentage)) %>%
      mutate(
        avg_percentage = scales::percent(avg_percentage / 100, accuracy = 0.01),
        max_percentage = scales::percent(max_percentage / 100, accuracy = 0.01)
      ) %>%
      rename(
        "Partner Countries" = partner_list,
        "Collaboration Type" = collab_type,
        "Chemical Category" = chemical,
        "Average %" = avg_percentage,
        "Maximum %" = max_percentage,
        "Years Present" = years_present
      )

    # Get unique values for styling
    collab_types_in_data <- unique(summary_data$`Collaboration Type`)
    chemical_types_in_data <- unique(summary_data$`Chemical Category`)

    # Create color vectors with matching lengths
    collab_colors <- c("#e6f7ff", "#e6ffe6", "#fff7e6", "#ffe6e6", "#f0f0f0")
    collab_colors <- collab_colors[1:length(collab_types_in_data)]

    chemical_colors <- c("#ffffff", "#f0f8ff", "#f5fffa", "#fff5f5", "#f8f8f8")
    # Ensure we have enough colors for all chemical categories
    if (length(chemical_types_in_data) > length(chemical_colors)) {
      chemical_colors <- colorRampPalette(chemical_colors)(length(chemical_types_in_data))
    } else {
      chemical_colors <- chemical_colors[1:length(chemical_types_in_data)]
    }

    # Create the datatable with proper styling
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        searchHighlight = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Collaboration Type",
        backgroundColor = styleEqual(
          collab_types_in_data,
          collab_colors
        )
      ) %>%
      formatStyle(
        "Chemical Category",
        backgroundColor = styleEqual(
          chemical_types_in_data,
          chemical_colors
        )
      )
  })
  #--------------------------
  # Multi-Country Search Tab
  #--------------------------

  # Store selected countries
  selected_countries <- reactiveVal(c())

  # Clear country selection
  observeEvent(input$clear_selection, {
    selected_countries(c())
  })

  # Display selected countries in the sidebar
  output$selected_countries_ui <- renderUI({
    countries <- selected_countries()

    if (length(countries) == 0) {
      return(div(
        class = "alert alert-secondary",
        "No countries selected yet. Click countries on the map to select them."
      ))
    }

    # Get country names for display
    country_names <- sapply(countries, function(iso) {
      match_idx <- match(iso, country_list$iso2c)
      if (!is.na(match_idx)) {
        country_list$country[match_idx]
      } else {
        iso
      }
    })

    # Create UI elements
    tags$div(
      lapply(seq_along(countries), function(i) {
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$span(country_names[i], style = "font-weight: bold;"),
          actionButton(
            inputId = paste0("remove_", countries[i]),
            label = NULL,
            icon = icon("times"),
            class = "btn-sm btn-outline-danger",
            style = "padding: 0px 5px; margin-left: 5px;"
          )
        )
      }),
      hr(),
      div(
        class = if (length(countries) < 2) "alert alert-warning" else "alert alert-info",
        if (length(countries) < 2) {
          "Select at least 2 countries to see collaborations."
        } else {
          paste0(
            "Showing ", input$map_chemical_category, " collaborations between all ",
            length(countries), " selected countries."
          )
        }
      )
    )
  })

  # Handle clicks on the map
  observeEvent(input$selection_map_shape_click, {
    # Get the clicked country ISO code
    clicked_iso <- input$selection_map_shape_click$id

    # Update the selected countries
    current <- selected_countries()

    if (clicked_iso %in% current) {
      # Remove if already selected
      selected_countries(setdiff(current, clicked_iso))
    } else {
      # Add if not already selected
      selected_countries(c(current, clicked_iso))
    }
  })

  # Handle remove buttons for each country
  observe({
    # Get current selected countries
    countries <- selected_countries()

    # Check for any remove button clicks
    lapply(countries, function(iso) {
      button_id <- paste0("remove_", iso)
      observeEvent(input[[button_id]],
        {
          selected_countries(setdiff(countries, iso))
        },
        ignoreInit = TRUE
      )
    })
  })

  # Create the selection map
  output$selection_map <- renderLeaflet({
    create_selection_map(selected_countries(), country_list)
  })

  # Update map when selections change
  observe({
    countries <- selected_countries()

    # Only update the map if it exists
    if (!is.null(input$selection_map_zoom)) {
      # Get world map data
      world_data <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

      leafletProxy("selection_map") %>%
        clearShapes() %>%
        addPolygons(
          data = world_data,
          fillColor = ~ colorFactor(c("lightgray", "#3388ff"), c(FALSE, TRUE))(iso_a2 %in% countries),
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
          # FIXED: Use the correct column name
          label = ~name,
          layerId = ~iso_a2,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    }
  })

  # Get data for specific country combinations
  specific_collab_data <- reactive({
    countries <- selected_countries()

    # Need at least 2 countries for a collaboration
    req(length(countries) >= 2, input$map_chemical_category)

    find_specific_collaborations(
      ds = ds,
      countries = countries,
      year_range = input$map_years,
      chemical_category = input$map_chemical_category,
      country_list = country_list
    )
  }) %>%
    bindCache(
      reactive(paste(sort(selected_countries()), collapse = "-")),
      input$map_years, input$map_chemical_category
    ) %>%
    bindEvent(selected_countries(), input$map_years, input$map_chemical_category)

  # Create visualization for specific collaborations
  output$specific_collab_plot <- renderPlotly({
    countries <- selected_countries()

    # Need at least 2 countries
    validate(need(length(countries) >= 2, "Select at least 2 countries to see their collaborations."))

    data <- specific_collab_data()
    validate(need(!is.null(data), "Error processing collaboration data."))

    create_specific_collab_plot(data, countries, country_list, input$map_chemical_category)
  })

  # Create table for specific collaborations
  output$specific_collab_table <- renderDT({
    countries <- selected_countries()

    # Need at least 2 countries
    validate(need(length(countries) >= 2, "Select at least 2 countries to see their collaborations."))

    data <- specific_collab_data()
    validate(need(!is.null(data), "Error processing collaboration data."))

    if (nrow(data) == 0) {
      return(datatable(
        data.frame(Message = "No collaborations found involving all selected countries."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    # Create summary table
    result_table <- data %>%
      group_by(iso2c, collab_type, chemical) %>%
      summarize(
        avg_percentage = mean(total_percentage, na.rm = TRUE),
        max_percentage = max(total_percentage, na.rm = TRUE),
        first_year = min(year),
        last_year = max(year),
        years_active = n_distinct(year)
      ) %>%
      arrange(desc(avg_percentage)) %>%
      mutate(
        avg_percentage = scales::percent(avg_percentage / 100, accuracy = 0.01),
        max_percentage = scales::percent(max_percentage / 100, accuracy = 0.01),
        period = paste0(first_year, " - ", last_year)
      ) %>%
      select(
        "Collaboration" = iso2c,
        "Type" = collab_type,
        "Chemical" = chemical,
        "Average %" = avg_percentage,
        "Maximum %" = max_percentage,
        "Period" = period,
        "Years Active" = years_active
      )

    datatable(
      result_table,
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 25),
        searchHighlight = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Type",
        backgroundColor = styleEqual(
          c("Bilateral", "Trilateral", "4-country", "5-country+"),
          c("#e6f7ff", "#e6ffe6", "#fff7e6", "#ffe6e6")
        )
      ) %>%
      formatStyle(
        "Chemical",
        backgroundColor = styleEqual(
          chemical_categories,
          c("#ffffff", "#f0f8ff", "#f5fffa", "#fff5f5")
        )
      )
  })
}

# Run the app
shinyApp(ui, server)
