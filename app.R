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
conflict_prefer("select", "dplyr", quiet = TRUE)
theme_set(theme_light())

# Source helper functions
source("R/functions.R") # Make sure functions.R is updated accordingly

# Load data
data_objects <- load_country_data("./data/data.parquet")
ds <- data_objects$data
country_list <- data_objects$country_list
chemical_categories <- data_objects$chemical_categories
regions <- data_objects$regions

# Prepare choices for selectizeInput (Country Name = ISO Code)
country_choices <- setNames(country_list$iso2c, country_list$country)

# --- UI Definition ---
ui <- page_sidebar(
  title = "Chemical Space Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 350, # Increased width slightly
    title = "Controls & Filters",
    # Synchronized Country Search
    selectizeInput(
        "country_search", "Search/Select Countries:",
        choices = country_choices,
        selected = NULL,
        multiple = TRUE,
        options = list(
            placeholder = 'Type to search for countries...',
            plugins = list('remove_button')
        )
    ),
    actionButton(
      "clear_selection", "Clear Selection",
      icon = icon("trash-alt"),
      class = "btn-outline-danger btn-sm mb-2", # Adjusted margin
      width = "100%"
    ),
    hr(),
    uiOutput("selection_info_ui"), # Moved selection info below search/clear
    hr(),
    h5("Data Filters", class="mb-2"),
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
    # Conditional UI for region filter
    uiOutput("region_filter_ui"),
    hr(class="my-3"),
    h5("Top Contributors", class="mb-2"),
    helpText("Top individual contributors based on current filters (avg % contribution)."),
    actionButton("select_top_5", "Select Top 5", class = "btn-primary btn-sm mb-2", width="100%"),
    helpText("Click name to add to selection:"),
    uiOutput("top_contributors_ui")
  ), # End sidebar

  # Main Panel Layout
  layout_columns(
    col_widths = c(12, 12), # Map takes full width initially, then plot below
    row_heights = c(1, 1), # Adjust relative heights if needed
    card(
      full_screen = TRUE,
      card_header("Country Selection Map"),
      # Added spinner to map as well
      withSpinner(leafletOutput("selection_map", height = "400px"))
    ),
    card(
      full_screen = TRUE,
      card_header(uiOutput("plot_header_ui")), # Dynamic header
      # Conditional UI for display mode (when >1 country selected)
      uiOutput("display_mode_ui"),
      withSpinner(plotlyOutput("main_plot", height = "400px")),
      hr(),
      h5("Data Summary Table"),
      # Added spinner
      withSpinner(DTOutput("summary_table"))
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
  # Use a temporary reactive value to break potential loops between map and selectize
  internal_selection_update <- reactiveVal(FALSE)

  display_mode <- reactiveVal("compare_individuals") # Default mode for >1 selection

  # --- Map & Selectize Synchronization ---

  # Observe map clicks to update selection and selectize input
  observeEvent(input$selection_map_shape_click, {
    req(!internal_selection_update()) # Prevent update if triggered by selectize

    clicked_iso <- input$selection_map_shape_click$id
    current_selection <- selected_countries()

    new_selection <- if (clicked_iso %in% current_selection) {
      setdiff(current_selection, clicked_iso)
    } else {
      unique(c(current_selection, clicked_iso))
    }
    selected_countries(new_selection)

    # Update selectize input to match map selection
    updateSelectizeInput(session, "country_search", selected = new_selection)
  })

  # Observe selectize input changes to update selection and map
  observeEvent(input$country_search, {
     # Use req() to handle initial NULL value gracefully
     req(input$country_search)
     # Allow update only if it's different from the current reactive value
     # AND if the internal flag is FALSE (meaning user changed selectize)
     if (!internal_selection_update() &&
         !identical(sort(input$country_search), sort(selected_countries()))) {

         selected_countries(input$country_search)
     }
     # Reset the flag after processing
     internal_selection_update(FALSE)

  }, ignoreNULL = FALSE, ignoreInit = TRUE) # ignoreNULL=FALSE needed to handle clearing


  # Update map highlighting when selection changes (from map or selectize)
  observe({
    countries <- selected_countries()
    leafletProxy("selection_map") %>%
      update_map_polygons(countries, country_list)

    # Set the internal flag before updating selectize to prevent loop
    internal_selection_update(TRUE)
    updateSelectizeInput(session, "country_search", selected = countries)

  })

  # Clear selection button (updates reactive value, which triggers map/selectize updates)
  observeEvent(input$clear_selection, {
    selected_countries(c())
    # No need to update selectize here, the observe above handles it
  })

  # --- Dynamic UI Elements ---

  # Display selected countries info (with flags)
  output$selection_info_ui <- renderUI({
    countries <- selected_countries()
    get_selection_info_ui(countries, country_list) # Use updated helper function
  })

  # Show/hide region filter (logic refined)
  output$region_filter_ui <- renderUI({
    # Show only if mode is 'individual' or 'compare_individuals'
    num_selected <- length(selected_countries())
    current_mode <- if (num_selected <= 1) "individual" else display_mode()

    if (current_mode %in% c("individual", "compare_individuals")) {
      tagList(
        selectInput(
          "region_filter", "Filter by Region (Individual Data):",
          choices = c("All", regions),
          selected = "All"
        )
      )
    } else {
      NULL # Don't show for 'find_collaborations' mode
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
    num_selected <- length(countries)
    mode <- if (num_selected <= 1) "individual" else display_mode()
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
          region_filter = current_region_filter, # Pass the potentially filtered region
          country_list = country_list,
          top_n = 10
      )
  })

  output$top_contributors_ui <- renderUI({
      top_data <- top_contributors_data()
      if (nrow(top_data) > 0) {
          buttons <- lapply(1:nrow(top_data), function(i) {
              country_iso <- top_data$iso2c[i]
              country_name <- top_data$country[i]
              # Ensure avg_percentage is accessed correctly and handle potential NA
              avg_perc_val <- top_data$avg_percentage[i]
              avg_perc_fmt <- if(!is.na(avg_perc_val)) scales::percent(avg_perc_val, accuracy = 0.1) else "N/A"

              actionButton(paste0("select_top_", country_iso),
                           HTML(paste0(i, ". ", country_name, " (", avg_perc_fmt, ")")),
                           class = "btn-link btn-sm d-block text-start",
                           style = "text-decoration: none; padding: 1px 5px;") # Minimal styling
          })
          tagList(buttons)
      } else {
          p("No contributor data for current filters.")
      }
  })

  # Observe click on "Select Top 5" button
   observeEvent(input$select_top_5, {
        top_data <- top_contributors_data()
        req(nrow(top_data) > 0)
        top_5_isos <- head(top_data$iso2c, 5)
        selected_countries(top_5_isos) # Set selection to only the top 5
   })

  # Create observers for ALL potential top contributor buttons ONCE at startup
  lapply(country_list$iso2c, function(iso) {
      observeEvent(input[[paste0("select_top_", iso)]], {
          current_selection <- selected_countries()
          if (!(iso %in% current_selection)) {
              new_selection <- unique(c(current_selection, iso))
              selected_countries(new_selection)
          }
      }, ignoreInit = TRUE, # Important: ignore initial NULL value
        #  destroyOnError = FALSE, # Keep observer even if inner code errors
         once = FALSE, # Ensure it can fire multiple times if button is re-rendered
         autoDestroy = TRUE, # Alternative to destroyMethod = "r"
         priority = 0) # Lower priority might help timing issues
  })

  # --- Data Processing ---
  processed_data <- reactive({
    countries <- selected_countries()
    # Require selections *only if* not in collaboration mode (or handle 0 selection there)
    # req(length(countries) > 0) # Keep this for now, might need adjustment for global views later

    # Determine mode based on selection count and UI choice
    num_selected <- length(countries)
    mode <- if (num_selected <= 1) "individual" else display_mode()

    # Determine region filter based on mode and UI availability
    current_region_filter <- "All" # Default
    if (mode %in% c("individual", "compare_individuals") && !is.null(input$region_filter)) {
        current_region_filter <- input$region_filter
    }

    # Only proceed if countries are selected
    req(length(countries) > 0, input$years, input$chemical_category)

    # Use withProgress for user feedback
    withProgress(message = "Processing data...", value = 0.5, {
      get_display_data(
        ds = ds,
        selected_isos = countries,
        year_range = input$years,
        chemical_category = input$chemical_category,
        display_mode = mode,
        region_filter = current_region_filter, # Pass the correct region filter
        country_list = country_list
      )
    })
  }) # Removed bindCache for debugging

  # --- Outputs ---

  # Render initial map (moved from functions.R to server to ensure context)
  output$selection_map <- renderLeaflet({
      create_selection_map(selected_countries(), country_list)
  })

  # Main Plot
  output$main_plot <- renderPlotly({
    validate(
        need(length(selected_countries()) > 0, "Click on the map or use search to select countries.")
    )
    data_to_plot <- processed_data()
    validate(
        need(is.data.frame(data_to_plot) && nrow(data_to_plot) > 0, "No data available for the current selection and filters.")
    )

    num_selected <- length(selected_countries())
    mode <- if (num_selected <= 1) "individual" else display_mode()

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
          need(is.data.frame(data_to_summarize) && nrow(data_to_summarize) > 0, "No summary data available for the current selection.")
      )

      num_selected <- length(selected_countries())
      mode <- if (num_selected <= 1) "individual" else display_mode()

      # Ensure the function is called correctly
      create_summary_table(
          data = data_to_summarize,
          display_mode = mode,
          country_list = country_list # Pass country_list for flags
      )
  }, server = TRUE) # Use server-side processing for DT if data gets large

} # End server

# Run the app
shinyApp(ui, server)
