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
    # bs_add_dependencies(), # Explicitly load all dependencies

  sidebar = sidebar(
    width = 350, # Increased width slightly
    title = "Controls & Filters",
    # Improved primary country search
    div(
      class = "mb-3",
      tags$label(class="form-label h6", "Select Countries:"), # Clearer label
      tags$div(
        class = "input-group",
        tags$span(class="input-group-text bg-primary text-white", icon("search")), # Highlighted icon
        selectizeInput(
          "country_search", NULL,
          choices = country_choices,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = 'Type to search and select countries...',
            plugins = list('remove_button'),
            maxItems = 5 # Allow multiple selections
          ),
          width = "100%"
        )
      ),
      helpText("Search and select countries to analyze their chemical space data. The map will highlight your selections.")
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
      withSpinner(leafletOutput("selection_map", height = "400px")),
      div(
        class = "text-muted small p-2 text-center",
        "The map shows your selected countries. Use the search box above to select or change countries."
      )
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
  display_mode <- reactiveVal("compare_individuals") # Default mode for >1 selection
  primary_country <- reactiveVal(NULL)       # Stores the primary country ISO
  collaborator_countries <- reactiveVal(c()) # Stores collaborating countries

  # --- Search Box as Primary Input ---
  # Simplified search box observer - main driver of all updates
  observeEvent(input$country_search, {
    countries <- input$country_search
    selected_countries(countries)
    
    # If exactly one country selected, find collaborators
    if (length(countries) == 1) {
      iso <- countries[1]
      primary_country(iso)
      withProgress(message = "Finding collaborators...", {
        collabs <- find_collaborating_countries(ds, iso, input$years, input$chemical_category)
        collaborator_countries(collabs)
      })
    } else {
      # Multiple or zero countries - clear primary/collaborator highlighting
      primary_country(NULL)
      collaborator_countries(c())
    }
  }, ignoreNULL = FALSE) # Handle NULL for initial load and clearing

  # Clear selection button - only updates reactive values, not map clicks
  observeEvent(input$clear_selection, {
    updateSelectizeInput(session, "country_search", selected = c())
    # The search box observer will handle the rest
  })

  # --- Map Visualization (Passive) ---
  # Map is now only for visualization, no longer captures clicks for data selection
  output$selection_map <- renderLeaflet({
    create_selection_map(c(), country_list) # Start with empty selection
  })
  
  # Update map highlighting based on search box selection
  observe({
    countries <- selected_countries()
    primary <- primary_country()
    collabs <- collaborator_countries()
    
    # If we have a primary country, highlight it and its collaborators
    if (!is.null(primary) && primary != "") {
      leafletProxy("selection_map") %>%
        update_map_polygons_with_collabs(primary, collabs, country_list)
    } 
    # Otherwise just highlight all selected countries equally
    else {
      leafletProxy("selection_map") %>%
        update_map_polygons(countries, country_list)
    }
  })

  # --- Dynamic UI Elements ---
  # (Keep the existing UI reactive elements)

  # --- Rest of the logic ---
  # (Keep your existing data processing, plot creation, etc.)

  # --- Select Top 5 button ---
  # Modify to work with search box as primary input
  observeEvent(input$select_top_5, {
    top_data <- top_contributors_data()
    if (nrow(top_data) >= 5) {
      top5_isos <- top_data$iso2c[1:5]
      
      # Always update through the search box input
      updateSelectizeInput(session, "country_search", selected = top5_isos[1])
      # The search box observer will handle the rest
    }
  })
}

# Run the app
shinyApp(ui, server)
