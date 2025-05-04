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
# --- NEW: Get pre-loaded article data ---
article_data <- data_objects$article_data

# --- Links ---

link_github <- tags$a(
  shiny::icon("github"), "Github",
  href = "https://santi-rios.github.io/publications/",
  target = "_blank"
)


# --- UI Definition ---
ui <- page_navbar( # Changed from page_sidebar
  # title = "Chemical Space Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  # navbar_options = navbar_options(position = "static-top", collapsible = TRUE),
  nav_menu(
    icon = bsicons::bs_icon("patch-exclamation-fill"),
    title = "Useful Links",
    align = "right",
    input_dark_mode(id = "mode", mode = "light"),
    nav_item(link_github)
  ),
  # Main application sidebar (for selection info and top contributors)
  sidebar = sidebar(
    # --- Selection Info Card ---
    width = 300,
    title = "Selection & Top Contributors", # Updated title
    open = "desktop",
    uiOutput("selection_info_ui"),
    hr(class = "my-3"),
    h5("Top Contributors", class = "mb-2"),
    helpText("Top individual contributors based on current filters (avg % contribution). Click to select."),
    uiOutput("top_contributors_ui")
  ),
  # --- Explorer Tab ---
  nav_panel(
    title = "Chemical Space Explorer",
    # --- Map and Filters Card ---
    card(
      full_screen = TRUE,
      card_header(
        "Country Selection & Filters 🌎",
        tooltip(
          bsicons::bs_icon("info-circle"),
          "Select a country on the map to see its contribution to the chemical space.",
          placement = "top"
        )
      ), # Updated header
      # Main content area for the map
      leafletOutput("selection_map", height = "350px"), # Adjusted height slightly
      card_footer(
        p("Use the filters from the right sidebar to adjust the year range and chemical space category."),
        p("Select multiple countries to explore trends and collaborations.")
      )
    ), # End Map and Filters Card

    popover(
        bsicons::bs_icon("gear", class = "ms-auto"),
        title = "Data Filters",
        show = TRUE,
        sliderInput(
          "years", "Year Range 📅:",
          min = min_year_data,
          max = max_year_data,
          value = c(max(min_year_data, 1996, na.rm = TRUE), min(max_year_data, 2022, na.rm = TRUE)),
          step = 1, sep = ""
        ),
        radioButtons(
          "chemical_category", "Chemical Category 🧪:",
          choices = chemical_categories,
          selected = "All"
        ),
        uiOutput("region_filter_ui"), # Filter Map by Region
        actionButton(
          "clear_selection", "Clear Selection",
          icon = icon("trash-alt"),
          class = "btn-outline-danger btn-sm mb-3",
          width = "100%"
        )
      ),
    nav_spacer(),

    # --- Plot and Table Card ---
    navset_card_pill(
      full_screen = TRUE,
      title = uiOutput("plot_header_ui"), # Dynamic header as title
      # --- Main Plot Panel ---
      # This panel will show the main plot and the contribution map
      nav_panel(
        "Trends in Chemical Space 📈",
        # card_title("Interactive Time Series Plot"), # Added specific title
        uiOutput("display_mode_ui"), # Conditional UI for multi-country selection
        withSpinner(plotlyOutput("main_plot", height = "400px"))
      ),
      # --- NEW: Contribution Map Panel ---
      nav_panel(
        "Country Contribution Map 📌",
        # card_title("Country Contribution Map 📌"), # Added specific title
        helpText("Shows the average contribution percentage over the selected period for the currently displayed countries."),
        withSpinner(plotlyOutput("contributionMapPlot", height = "400px")) # New plot output
      ),
      # --- End NEW Panel ---
      nav_panel(
        "Data Table",
        card_title("Data Summary Table"), # Added specific title
        DTOutput("summary_table")
      )
    ) # End Plot and Table Card
    # Removed extra parenthesis here
  ), # End Explorer nav_panel

  # --- NEW: Article Figures Tab ---
  nav_panel(
    title = "Article Figures",
    helpText("These plots replicate key figures from the source article and are based on a static dataset."),
    layout_columns(
      col_widths = c(6, 6, 6, 6), # Arrange plots in 2x2 grid
      row_heights = c(1, 1), # Equal height rows
      card(
        card_header("GDP Growth Rate"),
        withSpinner(plotlyOutput("articleGdpPlot", height = "350px"))
      ),
      card(
        card_header("Number of Researchers"),
        withSpinner(plotlyOutput("articleResearchersPlot", height = "350px"))
      ),
      card(
        card_header("Chemical Space Expansion"),
        withSpinner(plotlyOutput("articleCsExpansionPlot", height = "350px"))
      ),
      card(
        card_header("China-US Contributions"),
        withSpinner(plotlyOutput("articleChinaUsPlot", height = "350px"))
      )
      # Add more cards here if needed for other figures like "Country Participation"
    )
  ),
  # --- End NEW Tab ---

  # --- About Shiny Tab ---
  nav_panel(
    title = "About Shiny",
    # Placeholder card with image
    card(
      # Removed fixed height to let content dictate size
      # height = 300,
      full_screen = TRUE,
      card_image(
        # Make sure you have a "shiny-hex.svg" file in your app's www directory
        # or replace with a valid web URL
        file = "https://raw.githubusercontent.com/rstudio/shiny/main/man/figures/logo.png", # Example web URL
        alt = "Shiny's hex sticker",
        height = "150px", # Control image height
        width = "auto", # Let width adjust
        class = "mx-auto" # Center image if needed
        # href = "https://github.com/rstudio/shiny" # Link removed for simplicity, add if needed
      ),
      card_body(
        fill = FALSE,
        card_title("Shiny for R"),
        p(
          class = "fw-light text-muted",
          "This application is built using the Shiny web framework for R."
        )
      )
    )
  ), # End About Shiny nav_panel

  # --- Legal & Privacy Tab ---
  nav_panel(
    title = "Legal & Privacy",
    card( # Wrap content in a card for consistent look
      card_title("Legal Notice & Privacy Policy"),
      p("Placeholder for Legal Notice content."),
      p("Placeholder for Privacy Policy content.")
      # Add detailed text here later
    )
  ), # End Legal & Privacy nav_panel

  # --- Footer ---
  footer = div(
    style = "text-align: center; padding: 10px; background: #f8f9fa;",
    "Source: Bermúdez-Montaña, M., et al. (2025). China's rise in the chemical space and the decline of US influence.",
    a("Working Paper", href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6")
  )
) # End page_navbar

# --- Server Logic ---
server <- function(input, output, session) {
  # --- Reactive Values ---
  selected_countries_immediate <- reactiveVal(c()) # Stores ISO codes immediately on click

  # --- Debounced Reactive for Selected Countries ---
  # This reactive will wait 1000ms (1 second) after the last change to
  # selected_countries_immediate before updating its own value.
  selected_countries <- debounce(selected_countries_immediate, 1000)

  display_mode <- reactiveVal("compare_individuals") # Default mode for >1 selection

  # --- Map Interaction ---

  # Render initial map - Use the debounced value for initial state if needed,
  # but immediate might be better for instant feedback on first load.
  # Let's use immediate for initial load, proxy updates will use it too.
  output$selection_map <- renderLeaflet({
    create_selection_map(selected_countries_immediate(), country_list, regions)
  })

  # Handle map clicks - Update the IMMEDIATE reactive value
  observeEvent(input$selection_map_shape_click, {
    clicked_iso <- input$selection_map_shape_click$id
    current_selection <- selected_countries_immediate() # Use immediate value

    if (clicked_iso %in% current_selection) {
      selected_countries_immediate(setdiff(current_selection, clicked_iso)) # Update immediate
    } else {
      selected_countries_immediate(c(current_selection, clicked_iso)) # Update immediate
    }
  })

  # Update map highlighting - Use the IMMEDIATE value for instant visual feedback
  observe({
    countries <- selected_countries_immediate() # Use immediate for map styling
    leafletProxy("selection_map") %>%
      update_map_polygons(countries, country_list, regions)

    # Re-apply region filtering (this uses input$region_filter directly, which is fine)
    current_region_filter <- input$region_filter
    if (!is.null(current_region_filter)) {
      proxy <- leafletProxy("selection_map")
      if (current_region_filter == "All") {
        proxy %>% showGroup(regions)
      } else {
        proxy %>%
          hideGroup(setdiff(regions, current_region_filter)) %>%
          showGroup(current_region_filter)
      }
    }
  })

  # Clear selection button - Update the IMMEDIATE value
  observeEvent(input$clear_selection, {
    selected_countries_immediate(c()) # Update immediate
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

    output$mode <- renderText({
    paste("You are in", input$mode, "mode.")
  })

  # Display selected countries info - Use the DEBOUNCED value
  output$selection_info_ui <- renderUI({
    countries <- selected_countries() # Use debounced value
    req(countries) # Wait until debounced value is ready
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

  # Show/hide display mode selection - Use the DEBOUNCED value
  output$display_mode_ui <- renderUI({
    countries <- selected_countries() # Use debounced value
    req(countries)
    if (length(countries) > 1) {
      radioButtons(
        "display_mode_select", "Display Mode: ✨",
        choices = c(
          "Individual Contributions 🗾" = "compare_individuals",
          "Find Joint Collaborations 🤝🏽" = "find_collaborations"
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

  # Dynamic plot header - Use the DEBOUNCED value
  output$plot_header_ui <- renderUI({
    countries <- selected_countries() # Use debounced value
    req(countries)
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

  # Observe clicks on top contributor buttons - Update the IMMEDIATE value
  observe({
    top_data <- top_contributors_data()
    lapply(top_data$iso2c, function(iso) {
      observeEvent(input[[paste0("select_top_", iso)]], {
        current_selection <- selected_countries_immediate() # Use immediate
        if (!(iso %in% current_selection)) {
          selected_countries_immediate(c(current_selection, iso)) # Update immediate
        }
        # Optionally, could also make it toggle or switch selection
        # selected_countries(iso) # Select only this one
      })
    })
  })


  # --- Data Processing ---
  # Use the DEBOUNCED selected_countries reactive here
  processed_data <- reactive({
    countries <- selected_countries() # Use debounced value
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
        selected_isos = countries, # Pass debounced value
        year_range = input$years,
        chemical_category = input$chemical_category,
        display_mode = mode,
        region_filter = current_region_filter_for_data, # Pass the relevant filter
        country_list = country_list
      )
    })
  }) %>% bindCache(
    # Cache key needs to combine relevant inputs
    paste(sort(selected_countries()), collapse = "-"), # Use debounced value
    input$years,
    input$chemical_category,
    display_mode(),
    # Include region filter only if relevant for data processing cache key
    if (display_mode() %in% c("individual", "compare_individuals")) input$region_filter else "All"
  )

  # --- Outputs ---

  # Main Plot - Depends on processed_data, which uses the debounced selection
  output$main_plot <- renderPlotly({
    # Validate using the debounced selection
    validate(
      need(length(selected_countries()) > 0, "Click on the map to select a country.")
    )
    data_to_plot <- processed_data() # This now uses debounced data
    validate(
      need(nrow(data_to_plot) > 0, "No data available for the current selection and filters.")
    )

    mode <- if (length(selected_countries()) == 1) "individual" else display_mode()

    create_main_plot(
      data = data_to_plot,
      display_mode = mode,
      selected_isos = selected_countries(), # Pass debounced value
      country_list = country_list
    )
  })

  # --- NEW: Contribution Map Plot ---
  output$contributionMapPlot <- renderPlotly({
    countries <- selected_countries() # Use debounced value
    req(countries) # Wait for debounced value
    mode <- if (length(countries) == 1) "individual" else display_mode()

    # Only show map for individual/compare modes
    validate(
      need(length(countries) > 0, "Select countries on the map."),
      need(mode %in% c("individual", "compare_individuals"), "Map is available only when viewing individual country data.")
    )

    data_for_map <- processed_data() # Use the already processed & cached data

    validate(
      need(nrow(data_for_map) > 0, "No data available to generate map.")
    )

    # Generate the map plot using the new function
    create_contribution_map_plot(
      processed_data_df = data_for_map,
      # Optional: Add dynamic title/label if needed
      main_title = paste("Chemical space selected:", input$chemical_category),
      fill_label = "Avg % Contribution"
    )
  })
  # --- End NEW Plot ---

  # Summary Table - Depends on processed_data
  output$summary_table <- renderDT({
    countries <- selected_countries() # Use debounced value
    validate(need(length(countries) > 0, "Select countries to see summary data."))
    data_to_summarize <- processed_data() # Uses debounced data
    validate(
      need(nrow(data_to_summarize) > 0, "No data available for the current selection.")
    )

    mode <- if (length(selected_countries()) == 1) "individual" else display_mode()

    create_summary_table(
      data = data_to_summarize,
      display_mode = mode
    )
  })

  # --- NEW: Article Figure Outputs ---
  output$articleGdpPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Annual growth rate of the GDP")
    create_article_plot_simple(df, "Annual growth rate of the GDP", "GDP Growth Rate (%)")
  })

  output$articleResearchersPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Number of Researchers")
    create_article_plot_simple(df, "Number of Researchers", "Researchers")
  })

  output$articleCsExpansionPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Expansion of the CS")
    create_article_plot_simple(df, "Expansion of the CS", "Number of New Substances")
  })

  output$articleChinaUsPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "China-US in the CS")
    create_article_plot_simple(df, "China-US in the CS", "Contribution Share (%)")
  })

  # Add more renderPlotly calls here if you add more plots (e.g., for "Country participation in the CS")

  # --- End NEW Outputs ---
} # End server

# Run the app
shinyApp(ui, server)
