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
library(leaflet.extras)
# Resolve conflicts
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("layout", "plotly", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE) # Add if needed
theme_set(theme_light())

# Source helper functions
source("R/functions.R") # Make sure functions.R is updated accordingly

# Load data
data_objects <- load_country_data("./data/data.parquet")
ds <- data_objects$data
country_list <- data_objects$country_list
chemical_categories <- data_objects$chemical_categories
regions <- data_objects$regions # Now includes "Other" if applicable
# Get min/max year from the loaded data objects
min_year_data <- data_objects$min_year
max_year_data <- data_objects$max_year


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
      min = min_year_data, # Use value from loaded data
      max = max_year_data, # Use value from loaded data
      # Set default value safely within the actual data range
      value = c(max(min_year_data, 1996, na.rm = TRUE), min(max_year_data, 2022, na.rm = TRUE)),
      step = 1, sep = ""
      # JS Error Note: If slider errors persist ($x.noUiSlider), check bslib/shiny versions,
      # try removing bootswatch theme temporarily, or check browser console for details.
    ),
    radioButtons(
      "chemical_category", "Chemical Category:",
      choices = chemical_categories,
      selected = "All"
    ),
    # Conditional UI for region filter (when 1 or more countries selected for individual comparison)
    uiOutput("region_filter_ui"),
    hr(class = "my-3"),
    h5("Top Contributors", class = "mb-2"),
    helpText("Top individual contributors based on current filters (avg % contribution). Click to select."),
    uiOutput("top_contributors_ui")
  ), # End sidebar

  # Main Panel Layout
  layout_columns(
    col_widths = c(12, 12), # Map takes full width initially, then plot below
    row_heights = c(1, 1), # Adjust relative heights if needed
    card(
      full_screen = TRUE,
      card_header("Country Selection Map (Filterable by Region)"), # Updated header
      # JS Error Note: If map errors persist (m is not a constructor), it might be linked
      # to the slider error or leaflet/dependency loading issues. Check console.
      leafletOutput("selection_map", height = "400px")
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = uiOutput("plot_header_ui"), # Dynamic header as title
      nav_panel(
        "Main Plot",
        # Optionally, give a title inside this tab
        card_title("Interactive Plot"),
        uiOutput("display_mode_ui"), # Conditional UI for multi-country selection
        withSpinner(plotlyOutput("main_plot", height = "400px"))
      ),
      nav_panel(
        "Data Table",
        card_title("Data Summary Table"),
        DTOutput("summary_table")
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
      )
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
    # Pass the full list of regions for group creation
    create_selection_map(selected_countries(), country_list, regions)
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
      # Pass regions list for group recreation if needed by the function
      update_map_polygons(countries, country_list, regions)

    # Re-apply region filtering after polygons are potentially redrawn
    # This ensures the correct layers remain visible/hidden
    current_region_filter <- input$region_filter
    if (!is.null(current_region_filter)) {
      proxy <- leafletProxy("selection_map")
      if (current_region_filter == "All") {
        proxy %>% showGroup(regions) # Show all groups
      } else {
        proxy %>%
          hideGroup(setdiff(regions, current_region_filter)) %>% # Hide others
          showGroup(current_region_filter) # Show selected
      }
    }
  })

  # Clear selection button
  observeEvent(input$clear_selection, {
    selected_countries(c())
  })

  # --- Region Filter Map Control ---
  observeEvent(input$region_filter,
    {
      req(input$region_filter) # Ensure it's not NULL
      proxy <- leafletProxy("selection_map")

      if (input$region_filter == "All") {
        proxy %>% showGroup(regions) # Show all region groups
      } else {
        # Hide all regions first, then show the selected one
        # This handles switching between regions cleanly
        proxy %>%
          hideGroup(regions) %>% # Hide all
          showGroup(input$region_filter) # Show only the selected one
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  ) # Ignore initial NULL state

  # --- Dynamic UI Elements ---

  # Display selected countries info
  output$selection_info_ui <- renderUI({
    countries <- selected_countries()
    get_selection_info_ui(countries, country_list) # Use helper function
  })

  # Show/hide region filter
  output$region_filter_ui <- renderUI({
    # Show always for now, as it controls map visibility
    # Or revert to conditional logic if map filtering isn't desired when showing collaborations
    # show_filter <- length(selected_countries()) == 1 ||
    #                (length(selected_countries()) > 1 && display_mode() == "compare_individuals")
    # if (show_filter) { ... }

    # Let's keep it always visible to filter the map layers
    tagList(
      hr(),
      selectInput(
        "region_filter", "Filter Map by Region:", # Updated label
        choices = c("All", regions), # Use dynamic regions list
        selected = "All"
      )
    )
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
    # Region filter input now directly controls this via the UI element
    current_region_filter <- req(input$region_filter) # Make sure it's available

    calculate_top_contributors(
      ds = ds,
      year_range = input$years,
      chemical_category = input$chemical_category,
      region_filter = current_region_filter, # Use the input value
      country_list = country_list,
      top_n = 10
    )
  }) %>% bindCache(input$years, input$chemical_category, input$region_filter) # Add region_filter to cache key

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
          style = "text-decoration: none; padding: 1px 5px;"
        ) # Minimal styling
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
    current_region_filter_for_data <- if (mode %in% c("compare_individuals", "individual")) {
      req(input$region_filter)
    } else {
      "All" # Collaborations don't use region filter for data processing
    }

    withProgress(message = "Processing data...", {
      get_display_data(
        ds = ds,
        selected_isos = countries,
        year_range = input$years,
        chemical_category = input$chemical_category,
        display_mode = mode,
        region_filter = current_region_filter_for_data, # Pass the relevant filter
        country_list = country_list
      )
    })
  }) %>% bindCache(
    # Cache key needs to combine relevant inputs
    paste(sort(selected_countries()), collapse = "-"),
    input$years,
    input$chemical_category,
    display_mode(),
    # Include region filter only if relevant for data processing cache key
    if (display_mode() %in% c("individual", "compare_individuals")) input$region_filter else "All"
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
