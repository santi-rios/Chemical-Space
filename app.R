# Chemical Space Collaboration Explorer - Map Centric
# 2025-05-02
# Refactored by GitHub Copilot

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
library(stringr)
library(leaflet)
library(sf)
library(rnaturalearth)

# Resolve conflicts
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("layout", "plotly", quiet = TRUE)
theme_set(theme_light())

# Source helper functions
source("R/functions.R") # Make sure functions.R is updated accordingly

# Load data
data_objects <- load_country_data("./data/data.parquet")
ds <- data_objects$data
country_list <- data_objects$country_list
chemical_categories <- data_objects$chemical_categories
regions <- data_objects$regions

# --- UI Definition ---
ui <- page_sidebar(
  title = "Chemical Space Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,
    title = "Controls & Filters",
    uiOutput("selection_info_ui"),
    hr(),
    actionButton(
      "clear_selection", "Clear Selection",
      icon = icon("trash-alt"),
      class = "btn-outline-danger btn-sm mb-3",
      width = "100%"
    ),
    sliderInput(
      "years", "Year Range:",
      min = 1996, max = 2022,
      value = c(1996, 2022),
      step = 1, sep = ""
    ),
    radioButtons(
      "chemical_category", "Chemical Category:",
      choices = chemical_categories,
      selected = "All"
    ),
    # Conditional UI for region filter (when 1 or more countries selected for individual comparison)
    uiOutput("region_filter_ui"),
    hr(class="my-3"),
    h5("Top Contributors", class="mb-2"),
    helpText("Top individual contributors based on current filters (avg % contribution). Click to select."),
    uiOutput("top_contributors_ui")
  ), # End sidebar

  # Main Panel Layout
  layout_columns(
    col_widths = c(12, 12), # Map takes full width initially, then plot below
    row_heights = c(1, 1), # Adjust relative heights if needed
    card(
      full_screen = TRUE,
      card_header("Country Selection Map"),
      leafletOutput("selection_map", height = "400px")
    ),
    card(
      full_screen = TRUE,
      card_header(uiOutput("plot_header_ui")), # Dynamic header
      # Conditional UI for display mode (when >1 country selected)
      uiOutput("display_mode_ui"),
      withSpinner(plotlyOutput("main_plot", height = "400px")),
      hr(),
      h5("Data Summary Table"),
      DTOutput("summary_table")
    )
  ), # End layout_columns

  footer = div(
    style = "text-align: center; padding: 10px; background: #f8f9fa;",
    "Source: Bermúdez-Montaña, M., et al. (2025). China's rise in the chemical space and the decline of US influence.",
    a("Working Paper", href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6")
  )
) # End page_sidebar

# --- Server Logic ---
server <- function(input, output, session) {

  # --- Reactive Values ---
  selected_countries <- reactiveVal(c()) # Stores ISO codes of selected countries
  display_mode <- reactiveVal("compare_individuals") # Default mode for >1 selection

  # --- Map Interaction ---

  # Render initial map
  output$selection_map <- renderLeaflet({
    create_selection_map(selected_countries(), country_list)
  })

  # Handle map clicks
  observeEvent(input$selection_map_shape_click, {
    clicked_iso <- input$selection_map_shape_click$id
    current_selection <- selected_countries()

    if (clicked_iso %in% current_selection) {
      selected_countries(setdiff(current_selection, clicked_iso))
    } else {
      selected_countries(c(current_selection, clicked_iso))
    }
  })

  # Update map highlighting when selection changes
  observe({
    countries <- selected_countries()
    leafletProxy("selection_map") %>%
      update_map_polygons(countries, country_list) # Use updated function
  })

  # Clear selection button
  observeEvent(input$clear_selection, {
    selected_countries(c())
  })

  # --- Dynamic UI Elements ---

  # Display selected countries info
  output$selection_info_ui <- renderUI({
    countries <- selected_countries()
    get_selection_info_ui(countries, country_list) # Use helper function
  })

  # Show/hide region filter
  output$region_filter_ui <- renderUI({
    # Show if 1 country is selected OR if >1 country is selected AND mode is 'compare_individuals'
    show_filter <- length(selected_countries()) == 1 ||
                   (length(selected_countries()) > 1 && display_mode() == "compare_individuals")

    if (show_filter) {
      tagList(
        hr(),
        selectInput(
          "region_filter", "Filter by Region (Individual Data):",
          choices = c("All", regions),
          selected = "All"
        )
      )
    } else {
      NULL
    }
  })

  # Show/hide display mode selection
  output$display_mode_ui <- renderUI({
    if (length(selected_countries()) > 1) {
      radioButtons(
        "display_mode_select", "Display Mode:",
        choices = c(
          "Compare Individual Contributions" = "compare_individuals",
          "Find Joint Collaborations" = "find_collaborations"
        ),
        selected = display_mode(),
        inline = TRUE
      )
    } else {
      NULL
    }
  })

  # Update display_mode reactive value when radio button changes
  observeEvent(input$display_mode_select, {
    display_mode(input$display_mode_select)
  })

  # Dynamic plot header
  output$plot_header_ui <- renderUI({
    countries <- selected_countries()
    mode <- display_mode()
    chem <- input$chemical_category
    get_plot_header(countries, mode, chem, country_list) # Use helper function
  })

  # --- Top Contributors ---
  top_contributors_data <- reactive({
      req(input$years, input$chemical_category)
      # Use region filter only if it's available and selected
      current_region_filter <- if (!is.null(input$region_filter)) input$region_filter else "All"

      calculate_top_contributors(
          ds = ds,
          year_range = input$years,
          chemical_category = input$chemical_category,
          region_filter = current_region_filter,
          country_list = country_list,
          top_n = 10
      )
  }) %>% bindCache(input$years, input$chemical_category, input$region_filter)

  output$top_contributors_ui <- renderUI({
      top_data <- top_contributors_data()
      if (nrow(top_data) > 0) {
          buttons <- lapply(1:nrow(top_data), function(i) {
              country_iso <- top_data$iso2c[i]
              country_name <- top_data$country[i]
              avg_perc <- scales::percent(top_data$avg_percentage[i] / 100, accuracy = 0.1)
              actionButton(paste0("select_top_", country_iso),
                           HTML(paste0(i, ". ", country_name, " (", avg_perc, ")")),
                           class = "btn-link btn-sm d-block text-start",
                           style = "text-decoration: none; padding: 1px 5px;") # Minimal styling
          })
          tagList(buttons)
      } else {
          p("No contributor data.")
      }
  })

  # Observe clicks on top contributor buttons
  observe({
      top_data <- top_contributors_data()
      lapply(top_data$iso2c, function(iso) {
          observeEvent(input[[paste0("select_top_", iso)]], {
              current_selection <- selected_countries()
              if (!(iso %in% current_selection)) {
                  selected_countries(c(current_selection, iso))
              }
              # Optionally, could also make it toggle or switch selection
              # selected_countries(iso) # Select only this one
          })
      })
  })


  # --- Data Processing ---
  processed_data <- reactive({
    countries <- selected_countries()
    req(length(countries) > 0, input$years, input$chemical_category) # Require at least one selection

    mode <- if (length(countries) == 1) "individual" else display_mode()
    # Use region filter only if it's relevant for the current mode and available
    current_region_filter <- if (mode == "compare_individuals" && !is.null(input$region_filter)) {
        input$region_filter
    } else if (mode == "individual" && !is.null(input$region_filter)) {
        input$region_filter
    } else {
        "All" # Default to All if not applicable
    }


    withProgress(message = "Processing data...", {
      get_display_data(
        ds = ds,
        selected_isos = countries,
        year_range = input$years,
        chemical_category = input$chemical_category,
        display_mode = mode,
        region_filter = current_region_filter,
        country_list = country_list
      )
    })
  }) %>% bindCache(
      # Cache key needs to combine relevant inputs
      paste(sort(selected_countries()), collapse="-"),
      input$years,
      input$chemical_category,
      display_mode(),
      input$region_filter # Include region filter in cache key
      )

  # --- Outputs ---

  # Main Plot
  output$main_plot <- renderPlotly({
    validate(
        need(length(selected_countries()) > 0, "Click on the map to select a country.")
    )
    data_to_plot <- processed_data()
    validate(
        need(nrow(data_to_plot) > 0, "No data available for the current selection and filters.")
    )

    mode <- if (length(selected_countries()) == 1) "individual" else display_mode()

    create_main_plot(
        data = data_to_plot,
        display_mode = mode,
        selected_isos = selected_countries(),
        country_list = country_list
    )
  })

  # Summary Table
  output$summary_table <- renderDT({
      validate(
          need(length(selected_countries()) > 0, "Select countries to see summary data.")
      )
      data_to_summarize <- processed_data()
      validate(
          need(nrow(data_to_summarize) > 0, "No data available for the current selection.")
      )

      mode <- if (length(selected_countries()) == 1) "individual" else display_mode()

      create_summary_table(
          data = data_to_summarize,
          display_mode = mode
      )
  })

} # End server

# Run the app
shinyApp(ui, server)