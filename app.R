# app.R
library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(tidytable)
library(countrycode)
library(glue)
library(ggplot2)
library(data.table)
library(shinycssloaders)
library(RColorBrewer)
library(gt)
# library(leaflet)
# library(highcharter)
# library(viridisLite)
theme_set(theme_light())

source("R/plot_function.R")

# Efficient data preparation using Arrow and dplyr
ds <- arrow::open_dataset("./data6/df_cc_4.parquet", format = "parquet")

# Then in your app.R, replace the world_data computation with:
# In global scope (outside server function)
# Load world map data once and collect immediately (more efficient)
world_data <- arrow::read_parquet("data/world_data.parquet") %>%
  as.data.frame()


# Precompute country list from individual data (fast metadata operation)
country_list <- ds %>%
  dplyr::filter(is_collab == FALSE) %>%
  dplyr::select(country, iso2c) %>%
  dplyr::distinct() %>%
  dplyr::collect() %>%
  dplyr::arrange(country)


# Create a Shiny app object
ui <- page_navbar(
  id = "selected",
  selected = "National Trends 📈",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  header = NULL,
  navbar_options = navbar_options(collapsible = TRUE, underline = TRUE),
  sidebar = sidebar(
    title = "Country and Region Filters 🌍",
    width = "14rem",
    id = "sidebar",
    open = FALSE,
    tooltip(
      fontawesome::fa("info-circle", a11y = "sem", title = "Warnings"),
      "For more interactive visualizations, select the different tabs on the TOP panel.\n\n"
    ),
    sliderInput(
      min = 1996,
      max = 2022,
      value = c(
        1996,
        2022
      ),
      step = 1,
      sep = "",
      animate = FALSE,
      width = "100%",
      inputId = "years",
      label = "📅 Year Range"
    ),
    hr(),
    fluidRow(
      column(
        width = 6,
        actionButton("deselectAll", "Deselect All", class = "btn-primary", style = "width: 100%;")
      ),
      column(
        width = 6,
        actionButton("plotTopCountries", "Plot Top", class = "btn-danger", style = "width: 100%;")
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        actionButton("plotAll", "Plot All", class = "btn-success", style = "width: 100%;")
      )
    ),
    hr(),
    div(
      style = "margin-bottom: 18rem;",
      accordion(
        id = "countryAccordion",
        open = TRUE,
        accordion_panel(
          title = "Select Countries 🎌",
          value = "countriesPanel", # added value identifier
          checkboxGroupInput(
            inputId = "countries",
            label = NULL,
            choices = NULL,
            selected = NULL,
            width = "100%"
          )
        )
      )
    )
  ),
  # ------------------------------,
  # 1) NATIONAL TRENDS (INDIVIDUAL),
  # ------------------------------,
  nav_panel(
    "National Trends 📈",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          selectizeInput(
            "region", "Region Filter🗾",
            choices = "All",
            multiple = FALSE,
            options = list(plugins = "remove_button"),
            width = "30%"
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Trends📈",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "China's Chemical Revolution: From 1996 to 2022, China surged to claim the chemical discoveries—far outpacing the US’s share—driven almost entirely by domestic research. In contrast, US solo contributions has steadily dropped, with rising international collaboration. Toggle between country-specific and collaboration plots to explore these dynamics.", # nolint: line_length_linter.
              placement = "left"
            ),
            withSpinner(plotlyOutput("trendPlot", width = "100%", height = "100%"), color = "#024173"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          ),
          nav_panel(
            "Map📌",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Interactive map showing country contributions...",
              placement = "left"
            ),
            withSpinner(plotlyOutput("mapPlot"), color = "#024173"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          ),
          nav_panel(
            "Substance Types🧪",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Chemical type distribution over time...",
              placement = "left"
            ),
            fluidRow(
              column(
                width = 12,
                selectInput(
                  "chemicalSelector",
                  "Select Chemical Type",
                  choices = c("Organic", "Organometallic", "Rare-Earths"),
                  selected = "Organic",
                  width = "30%"
                )
              )
            ),
            withSpinner(plotlyOutput("substancePlot", width = "100%"), color = "#024173"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Top Contributors 🏆",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Top contributors by year...",
              placement = "left"
            ),
            gt_output("top_contributors_table"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          )
        )
      )
    )
  ),
  # ------------------------------,
  # 2) COLLABORATION TRENDS,
  # ------------------------------,
  nav_panel(
    "Collaboration Trends 🤝",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          # Removed value_box here
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Trends📈",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "China's Chemical Revolution: From 1996 to 2022...",
              placement = "left"
            ),
            withSpinner(plotlyOutput("collabTrendPlot", width = "100%"), color = "#024173"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          ),
          nav_panel(
            "Map📌",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Interactive map showing country contributions...",
              placement = "left"
            ),
            withSpinner(plotlyOutput("mapPlotCollab"), color = "#024173"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          ),
          nav_panel(
            "Substance Types🧪",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Chemical type distribution over time...",
              placement = "left"
            ),
            fluidRow(
              column(
                width = 12,
                selectInput(
                  "chemicalSelectorcollab",
                  "Select Chemical Type",
                  choices = c("Organic", "Organometallic", "Rare-Earths"),
                  selected = "Organic",
                  width = "30%"
                )
              )
            ),
            withSpinner(plotlyOutput("collabSubstancePlot", width = "100%"), color = "#024173"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          )
        )
      )
    )
  ),
  # ------------------------------,
  # 2.1) COLLABORATION TRENDS b,
  # ------------------------------,
  nav_panel(
    "Collaboration Trends B 🤝",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          selectizeInput(
            "country_select", "Select Country:",
            # selected = NULL,
            choices = setNames(country_list$iso2c, country_list$country),
            options = list(
              placeholder = "Search for a country and see its collaborations",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Explore All collaborations",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "China's Chemical Revolution: From 1996 to 2022, China surged to claim the chemical discoveries—far outpacing the US’s share—driven almost entirely by domestic research. In contrast, US solo contributions has steadily dropped, with rising international collaboration. Toggle between country-specific and collaboration plots to explore these dynamics.",
              placement = "left"
            ),
            uiOutput("conditionalCollabPlot"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Top Contributors 🏆",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Top contributors by year...",
              placement = "left"
            ),
            gt_output("top_contributors_tableB"),
            card_footer(
              "Source: China's rise in the chemical space and the decline of US influence.",
              popover(
                a("Learn more", href = "#"),
                markdown(
                  "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
                )
              )
            )
          )
        )
      )
    )
  ),
  # ------------------------------,
  # 3) ARTICLE FIGURES,
  # ------------------------------,
  nav_panel(
    "Article Figures 📰",
    fluidRow(
      column(
        width = 12,
        selectInput(
          "article_source", "Select Article Source",
          choices = c("Expansion of the CS", "China-US in the CS", "Annual growth rate of the GDP", "Number of Researchers", "Country participation in the CS"),
          selected = "Number of Researchers",
          width = "40%"
        )
      )
    ),
    withSpinner(plotlyOutput("articlePlot"), color = "#024173"),
    card_footer(
      "This plots show the chemichap space growth, enfatising China's rise in the chemical space (CS) and the decline of US influence.",
      popover(
        a("Learn more", href = "#"),
        markdown(
          "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
        )
      )
    ),
    br(),
    br(),
    hr(),
    tags$h3("Original country and collaborations contributions to the chemical space from the original article"),
    tags$p("Here we show, by analysing the chemical space between 1996 and 2022, that the chemical space expansion has been dominated by China ever since 2013. Chinese dominance is mainly the product of the country’s own efforts, rather than the result of international collaboration. Alternatively, the US share of the chemical space is more dependent on international collaboration, which mainly occurs with China.")
  ),
  # # ------------------------------,
  # 5) KNOW MORE,
  # ------------------------------,
  nav_panel(
    "Know more about the research 🥼",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          h3("Chinas rise in the chemical space and the decline of US influence"),
          p("This dashboard is based on the study China's rise in the chemical space and the decline of US influence. Between 1996 and 2022, the research shows that China has emerged as a dominant force in chemical discovery—especially after 2013—mainly through national efforts, while US contributions depend largely on international collaborations."),
          p("The analysis spans various chemical domains including organic, rare-earth, and organometallic chemistry, also highlighting the emerging role of India in the field. These insights provide a contemporary account of global shifts in the chemical space and may guide future science policies and R&D agendas."),
          p("Useful links for more information:"),
          tags$ul(
            tags$li(
              tags$a(
                href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6",
                target = "_blank",
                "Access the full preprint 📄"
              )
            ),
            tags$li(
              tags$a(
                href = "https://github.com/santi-rios/Chemical-Space/wiki",
                target = "_blank",
                "App wiki and documentation 📖"
              )
            ),
            tags$li(
              tags$a(
                href = "https://github.com/santi-rios/Chemical-Space",
                target = "_blank",
                "Code Repository 📦"
              )
            )
          )
        )
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      hr(),
      fluidRow(
        column(
          width = 12,
          tags$a(
            href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6",
            target = "_blank",
            tags$img(
              src = "logos_footer.png",
              class = "img-fluid",
              style = "max-width: 320px; height: 100px; display: block; margin: 0 auto;"
            )
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  active_tab <- reactive(input$selected)

  # Flag to track if user manually cleared selections
  user_cleared <- FALSE
  # Variable to track previous region selection
  prev_region <- "All"

  # Base reactive datasets with filtering
  individual_data <- reactive({
    ds %>%
      dplyr::filter(is_collab == FALSE) %>%
      dplyr::select(
        iso2c, year, percentage, chemical, iso3c, country,
        lat, lng, region, cc
      )
  })

  collab_data <- reactive({
    ds %>%
      dplyr::filter(is_collab == TRUE) %>%
      dplyr::select(
        iso2c, year, percentage, chemical, iso3c, country,
        lat, lng, region, cc
      )
  })

  # Define figure_article INSIDE the server
  figure_article <- reactive({
    ds %>%
      dplyr::filter(!is.na(percentage_x)) %>%
      dplyr::select(
        percentage = percentage_x,
        country = country_x,
        year = year_x,
        source, condition, region, cc
      ) %>%
      dplyr::collect() # Remove collect() if ds is already in memory
  })

  # Active dataset based on tab
  active_dataset <- reactive({
    if (active_tab() == "National Trends 📈") {
      individual_data()
    } else if (active_tab() == "Collaboration Trends 🤝") {
      collab_data() %>%
        dplyr::filter(iso2c %in% c(
          "CN-US", "DE-US", "GB-US", "IN-US", "CN-JP", "CA-US",
          "DE-RU", "JP-US", "DE-FR", "FR-US", "ES-GB", "IT-US",
          "CN-DE", "DE-ES", "ES-FR", "KR-US", "DE-GB", "FR-GB",
          "ES-IT", "DE-IN", "CN-HK", "ES-US", "CH-FR", "CN-GB",
          "FR-RU"
        )) %>%
        dplyr::collect()
    } else if (active_tab() == "Collaboration Trends B 🤝") {
      collab_data()
    } else if (active_tab() == "Article Figures 📰") {
      figure_article()
    }
  }) %>%
    bindCache(active_tab())

  # Initialize available countries (run once)
  all_countries <- reactive({
    # Get all available countries based on the active tab
    country_data <- active_dataset() %>%
      dplyr::distinct(country) %>%
      dplyr::collect()

    sort(unique(country_data$country))
  }) %>%
    bindCache(active_tab())

  # Get all available regions (regardless of filtering)
  all_regions <- reactive({
    active_dataset() %>%
      dplyr::distinct(region) %>%
      dplyr::filter(!is.na(region)) %>%
      dplyr::arrange(region) %>%
      dplyr::collect() %>%
      dplyr::pull(region)
  }) %>% bindCache(active_tab())

  # Get top countries accounting for all current filters
  top_countries <- reactive({
    # Start with active_dataset, then apply current filters
    result <- active_dataset() %>%
      dplyr::filter(
        year >= input$years[1],
        year <= input$years[2]
      )

    # Apply region filter if set
    if (!is.null(input$region) && input$region != "All") {
      result <- result %>% dplyr::filter(region == input$region)
    }

    # Calculate top countries based on filtered data
    result %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(val = sum(percentage, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::collect() %>%
      dplyr::pull(country)
  }) %>% bindCache(active_tab(), input$years, input$region)


  # Initial country choices setup - properly respond to region changes
  observe({
    req(all_countries())
    req(top_countries())  # Add explicit dependency on top_countries
    
    # Only auto-select countries when:
    # 1. User hasn't explicitly cleared selections
    # 2. Countries selection is empty
    # 3. Region filter has changed
    selected_countries <- if (user_cleared) {
      character(0)
    } else if (is.null(input$countries) || length(input$countries) == 0) {
      top_countries()
    } else if (length(input$countries) > 0 && input$region != prev_region) {
      # If region changed, update with new top countries
      top_countries()
    } else {
      input$countries
    }
    
    # Store current region for comparison
    prev_region <<- input$region
    
    updateCheckboxGroupInput(session, "countries",
      choices = all_countries(),
      selected = selected_countries
    )
    
    # Reset the flag
    user_cleared <- FALSE
  }) %>% bindEvent(all_countries(), top_countries(), input$region) 

  # Initialize region choices once when the app starts
  observe({
    req(all_regions())

    # Update the selectizeInput with ALL possible regions
    updateSelectizeInput(
      session,
      "region",
      choices = c("All", all_regions()),
      selected = if (is.null(input$region)) "All" else input$region
    )
  }) %>% bindEvent(all_regions())

  # Filtered data with caching
  filtered_data <- reactive({
    req(input$countries, input$years)

    result <- active_dataset() %>%
      dplyr::filter(
        year >= input$years[1],
        year <= input$years[2],
        country %in% input$countries
      )

    # Only apply region filter if input$region is not "All"
    # For the new dataset with NA regions, since "All" is used, the filter is skipped
    if (!is.null(input$region) && input$region != "All") {
      result <- result %>% dplyr::filter(region == input$region)
    }

    # Collect the data after all filters are applied
    result %>% dplyr::collect()
  }) %>%
    bindCache(active_tab(), input$years, input$countries, input$region) %>%
    debounce(300)

  # Button handlers with fixes
  observeEvent(input$plotTopCountries, {
    user_cleared <- FALSE
    top_countries_current_sel <- top_countries()
    updateCheckboxGroupInput(session, "countries", selected = top_countries_current_sel)
  })

  # Fixed deselect all button using isolation and priority
  observeEvent(input$deselectAll,
    {
      user_cleared <- TRUE
      updateCheckboxGroupInput(session, "countries", selected = character(0))
    },
    priority = 10
  )

  observeEvent(input$plotAll, {
    user_cleared <- FALSE
    updateCheckboxGroupInput(session, "countries", selected = all_countries())
  })

  observe({
    if (active_tab() == "National Trends 📈") {
      showNotification("Double click on the legend to isolate a country or single click to hide it.",
        type = "message",
        duration = 3
      )
    } else if (active_tab() == "Collaboration Trends 🤝") {
      showNotification("Please select Plot Top or Plot All from the sidebar to see the visualizations.",
        type = "error",
        duration = 4
      )
    } else if (active_tab() == "Collaboration Trends B 🤝") {
      showNotification("Start searching for a country at the top to see its collaborations. Please be patient as we are loading the data.",
        type = "warning",
        duration = 5
      )
    } else if (active_tab() == "Article Figures 📰") {
      showNotification("Toggle between the article sources to see the different figures.",
        type = "default",
        duration = 2
      )
    }
  })

  observe({
    if (active_tab() %in% c("National Trends 📈", "Collaboration Trends 🤝")) {
      sidebar_toggle(
        id = "sidebar",
        open = TRUE
      )
    } else {
      sidebar_toggle(
        id = "sidebar",
        open = FALSE
      )
    }
  })

  observe({
    if (active_tab() %in% c("Collaboration Trends B 🤝", "Article Figures 📰", "Know more about the research 🥼")) {
      accordion_panel_close(
        id = "countryAccordion",
        "countriesPanel" # Remove "panel =" - just pass the value directly
      )
    } else {
      accordion_panel_open(
        id = "countryAccordion",
        "countriesPanel" # Remove "panel =" - just pass the value directly
      )
    }
  })

  #########
  # Plots #
  #########

  # ...existing code...

  # Generate table data for top contributors by year
  top_contributors_by_year <- reactive({
    req(filtered_data())
    req(active_tab() == "National Trends 📈")

    # Extract top 3 contributors by year from filtered data
    filtered_data() %>%
      dplyr::filter(chemical == "All") %>%
      dplyr::group_by(year, iso2c, country) %>%
      # Using mean instead of sum since these are already percentages per entry
      dplyr::summarise(contribution = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(year) %>%
      dplyr::arrange(year, desc(contribution)) %>%
      dplyr::slice_head(n = 3) %>%
      dplyr::ungroup()
  })

  # Render the top contributors table with flags
  # Render the top contributors table with flags
  output$top_contributors_table <- render_gt({
    req(top_contributors_by_year())
    req(nrow(top_contributors_by_year()) > 0)

    top_contributors_by_year() %>%
      dplyr::select(year, iso2c, country, contribution) %>%
      gt::gt(groupname_col = "year") %>%
      gt::tab_header(
        title = "Top Contributors to the Chemical Space",
        subtitle = "Top 3 contributors by year based on current selected filters"
      ) %>%
      gt::fmt_flag(columns = iso2c) %>%
      # Display the number as is, with proper formatting
      gt::fmt_number(
        columns = contribution,
        decimals = 1
      ) %>%
      gt::cols_label(
        iso2c = "",
        country = "Country",
        contribution = "Total Contribution (%)"
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_row_groups()
      ) %>%
      gt::cols_width(
        iso2c ~ px(50),
        country ~ px(150),
        contribution ~ px(120)
      ) %>%
      gt::opt_row_striping()
  })


  #########
  # Plots #
  #########
  # National Trends Plot
  # National Trends Plot with optimized rendering
  output$trendPlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)

    # Filter data first
    data <- filtered_data()[filtered_data()$chemical == "All", ]


    # Find minimum and maximum years
    min_year <- min(data$year, na.rm = TRUE)
    max_year <- max(data$year, na.rm = TRUE)

    # Only show labels for top countries to avoid clutter
    # First, identify top countries by their max percentage
    top_countries_data <- data %>%
      group_by(country) %>%
      summarise(max_pct = max(percentage, na.rm = TRUE)) %>%
      arrange(dplyr::desc(max_pct)) %>%
      slice_head(n = 10) # Adjust number as needed

    # Get the end year data for labeling
    end_labels_data <- data %>%
      filter(country %in% top_countries_data$country) %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup()

    # Create base plot
    createChemicalSpacePlot(
      data = data,
      min_year = min_year,
      max_year = max_year,
      end_labels_data = end_labels_data
    )
  }) %>%
    bindCache(active_tab(), filtered_data())

  # Collaboration Trends Plot
  output$collabTrendPlot <- renderPlotly({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)

    data <- filtered_data()[filtered_data()$chemical == "All", ]

    # Find minimum and maximum years
    min_year <- min(data$year, na.rm = TRUE)
    max_year <- max(data$year, na.rm = TRUE)

    # Only show labels for top countries to avoid clutter
    # First, identify top countries by their max percentage
    top_countries_data <- data %>%
      group_by(country) %>%
      summarise(max_pct = max(percentage, na.rm = TRUE)) %>%
      arrange(dplyr::desc(max_pct)) %>%
      slice_head(n = 10) # Adjust number as needed

    # Get the end year data for labeling
    end_labels_data <- data %>%
      filter(country %in% top_countries_data$country) %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup()

    # Create base plot
    createChemicalSpacePlot(
      data = data,
      min_year = min_year,
      max_year = max_year,
      region_var = "country",
      end_labels_data = end_labels_data
    )
  }) %>%
    bindCache(active_tab(), filtered_data())

  output$mapPlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)

    # Filter data
    data <- filtered_data()[filtered_data()$chemical == "All", ]

    # IMPORTANT: Check if there's data after filtering
    req(nrow(data) > 0)

    # Compute aggregated values including best/worst years and region
    map_data <- data %>%
      group_by(country, year, region) %>%
      summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
      group_by(country, region) %>%
      summarise(
        value = mean(yearly_avg, na.rm = TRUE),
        best_year = year[which.max(yearly_avg)],
        worst_year = year[which.min(yearly_avg)],
        .groups = "drop"
      )

    # # Debug: print column names to help diagnose
    # print(paste("Map data columns:", paste(names(map_data), collapse=", ")))
    # print(paste("Number of rows in map_data:", nrow(map_data)))

    # Generate the map plot
    createStaticMapPlot(
      df = map_data,
      world_df = world_data,
      fill_var = "value",
      fill_label = "Average Contribution",
      main_title = ""
    )
  }) %>%
    bindCache(active_tab(), filtered_data())


output$mapPlotCollab <- renderPlotly({
    req(active_tab() == "Collaboration Trends 🤝")

    # Filter data for collaboration tab
    data <- filtered_data() %>%
      filter(chemical == "All")

    # IMPORTANT: Check if there's data after filtering
    req(nrow(data) > 0)

    # Process the collaboration data by splitting country pairs
    map_data <- data %>%
      # Step 1: Get yearly averages by collaboration pair
      dplyr::group_by(country, year) %>%
      dplyr::summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
      # Step 2: Split the country codes and expand the data
      mutate(
        country_parts = strsplit(as.character(country), ","),
        country1 = sapply(country_parts, function(x) if (length(x) > 0) x[1] else NA_character_),
        country2 = sapply(country_parts, function(x) if (length(x) > 1) x[2] else NA_character_)
      ) %>%
      # Filter out any rows where splitting didn't work properly
      filter(!is.na(country1), !is.na(country2)) %>%
      # Step 3: Create separate rows for each country in the pair
      tidyr::pivot_longer(
        cols = c(country1, country2),
        names_to = "country_position",
        values_to = "individual_country"
      ) %>%
      # Step 4: Group by individual country and year to get yearly stats
      dplyr::group_by(individual_country, year) %>%
      dplyr::summarise(
        value = sum(yearly_avg, na.rm = TRUE),
        # Store the specific collaboration pairs for this country by year
        collab_pairs = paste(sort(unique(country)), collapse = "; "),
        .groups = "drop"
      ) %>%
      # Step 5: Find best/worst years for each country with their values
      dplyr::group_by(individual_country) %>%
      dplyr::mutate(
        avg_value = mean(value, na.rm = TRUE),
        max_idx = which.max(value),
        min_idx = which.min(value),
      ) %>%
      dplyr::summarise(
        value = mean(value, na.rm = TRUE),
        best_year = year[max_idx][1],  # Take first if multiple ties
        worst_year = year[min_idx][1],
        best_year_value = value[max_idx][1],
        worst_year_value = value[min_idx][1],
        # Create a formatted list of countries this country collaborates with
        collab_list = paste(unique(unlist(strsplit(
          paste(unique(collab_pairs), collapse = "; "), 
          "; "
        ))), collapse = "; "),
        .groups = "drop"
      ) %>%
      # Step 6: Rename to match the expected format for the plot function
      rename(country = individual_country) %>%
      # Add a placeholder for region as it might be needed
      mutate(region = NA_character_)

    # Generate the map plot
    createCollabMapPlot(
      df = map_data,
      world_df = world_data,
      fill_var = "value",
      fill_label = "Average Collaboration Strength (%)",
      main_title = "International Research Collaboration Network"
    )
  }) %>%
    bindCache(active_tab(), filtered_data())

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Substance Types
  # Substance Plots


  output$substancePlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)

    data <- data <- filtered_data()[filtered_data()$chemical == input$chemicalSelector, ]

    # Find minimum and maximum years
    min_year <- min(data$year, na.rm = TRUE)
    max_year <- max(data$year, na.rm = TRUE)

    # Only show labels for top countries to avoid clutter
    # First, identify top countries by their max percentage
    top_countries_data <- data %>%
      group_by(country) %>%
      summarise(max_pct = max(percentage, na.rm = TRUE)) %>%
      arrange(dplyr::desc(max_pct)) %>%
      slice_head(n = 10) # Adjust number as needed

    # Get the end year data for labeling
    end_labels_data <- data %>%
      filter(country %in% top_countries_data$country) %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup()


    createTrendPlot(
      data = data,
      min_year = min_year,
      max_year = max_year,
      end_labels_data = end_labels_data,
      title = paste("Contribution to", input$chemicalSelector)
    )
  }) %>%
    bindCache(active_tab(), input$chemicalSelector, filtered_data())


  output$collabSubstancePlot <- renderPlotly({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)

    data <- data <- filtered_data()[filtered_data()$chemical == input$chemicalSelectorcollab, ]

    # Find minimum and maximum years
    min_year <- min(data$year, na.rm = TRUE)
    max_year <- max(data$year, na.rm = TRUE)

    # Only show labels for top countries to avoid clutter
    # First, identify top countries by their max percentage
    top_countries_data <- data %>%
      group_by(country) %>%
      summarise(max_pct = max(percentage, na.rm = TRUE)) %>%
      arrange(dplyr::desc(max_pct)) %>%
      slice_head(n = 10) # Adjust number as needed

    # Get the end year data for labeling
    end_labels_data <- data %>%
      filter(country %in% top_countries_data$country) %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup()


    createTrendPlot(
      data = data,
      min_year = min_year,
      max_year = max_year,
      end_labels_data = end_labels_data,
      title = paste("Contribution to", input$chemicalSelectorcollab)
    )
  }) %>%
    bindCache(active_tab(), input$chemicalSelectorcollab, filtered_data())


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Then add this to your server function:
output$conditionalCollabPlot <- renderUI({
  if (!is.null(input$country_select) && input$country_select != "") {
    tagList(
      withSpinner(
        plotlyOutput("collab_plot", width = "100%", height = "100%"), 
        color = "#024173"
      )
    )
  } else {
    div(
      style = "text-align: center; margin-top: 100px; color: #666;",
      h4("Please select a country to view collaboration data"),
      icon("search")
    )
  }
})

  # Reactive for filtered collaboration data
  filtered_collab <- reactive({
    req(active_tab() == "Collaboration Trends B 🤝", input$country_select, input$years)

    ds %>%
      dplyr::filter(
        is_collab == TRUE,
        year >= input$years[1],
        year <= input$years[2]
      ) %>%
      # Incluir todas las columnas necesarias
      dplyr::select(iso2c, year, percentage, country, cc) %>% # <- ¡NUEVO!
      dplyr::filter(grepl(input$country_select, iso2c)) %>%
      dplyr::collect() %>%
      mutate(
        partners = strsplit(iso2c, "-"),
        # Modificado para manejar múltiples partners
        partner = purrr::map(partners, ~ setdiff(.x, input$country_select))
      ) %>%
      tidyr::unnest(partner) %>% # Convertir lista en filas
      # Agregar nombre del país partner usando tu metadata
      left_join(country_list, by = c("partner" = "iso2c")) %>%
      rename(partner_country = country.y) # country.x es el original
  })

  # Collaboration plot
  output$collab_plot <- renderPlotly({
    req(active_tab() == "Collaboration Trends B 🤝")

    data <- filtered_collab()
    validate(need(nrow(data) > 0, "No collaborations found..."))

    agg_data <- data %>%
      group_by(partner_country, cc, year) %>% # Agrupar por nombre y color
      summarise(
        total_percentage = sum(percentage, na.rm = TRUE),
        .groups = "drop"
      )

    # Create plot
    ggplot(
      agg_data,
      aes(
        x = year,
        y = total_percentage,
        color = cc,
        text = paste(
          "Country:", partner_country,
          "<br>Year:", year,
          "<br>Total Collaboration Percentage:", scales::percent(total_percentage)
        )
      )
    ) +
      geom_jitter(
        aes(size = total_percentage)
      ) +
      scale_radius(range = c(0.8, 6)) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1, scale = 1),
        expand = expansion(mult = c(0.05, 0.15))
      ) +
      scale_color_identity() +
      labs(
        title = paste("Collaborations for", country_list$country[match(input$country_select, country_list$iso2c)]),
        x = "Year", y = "Total Collaboration Percentage"
      ) +
      theme_minimal()
    # scale_color_manual(values = setNames(agg_data$cc, agg_data$partner_country))
  }) %>% bindCache(input$country_select, input$years)


  # Añade esto al server después del gráfico

  # Top contributors table (historical)
  top_contributors <- reactive({
    req(filtered_collab())

    filtered_collab() %>%
      group_by(partner, partner_country) %>% # Asumiendo que ya tienes estas columnas
      summarise(total_contribution = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_contribution)) %>%
      slice_head(n = 3) %>%
      select(partner, partner_country) # Seleccionar solo las columnas necesarias
  })

  # Render the table with flags
  output$top_contributors_tableB <- render_gt({
    req(top_contributors())
    req(nrow(top_contributors()) > 0)

    top_contributors() %>%
      gt() %>%
      gt::tab_header(
        title = "Top 3 Historical Contributors",
        subtitle = "All selected years combined"
      ) %>%
      gt::fmt_flag(columns = partner) %>% # Mostrar banderas desde códigos ISO2
      gt::cols_label(
        partner = "", # Ocultar título de columna de banderas
        partner_country = "Country"
      ) %>%
      gt::cols_width(
        partner ~ px(50), # Ancho fijo para columna de bandera
        partner_country ~ px(200)
      ) %>%
      gt::opt_row_striping() %>%
      gt::tab_options(
        table.font.size = "14px",
        heading.title.font.size = "18px"
      )
  })






  #################
  # Article Figures
  ##################
  # app.R (server)
  # Updated server code for articlePlot
  output$articlePlot <- renderPlotly({
    req(active_tab() == "Article Figures 📰", input$article_source)

    # Define appropriate y-axis titles
    y_title <- switch(input$article_source,
      "Expansion of the CS" = "Number of new substances",
      "China-US in the CS" = "Percentage of national contribution",
      "Annual growth rate of the GDP" = "GDP per capita growth (annual %)",
      "Number of Researchers" = "Researchers", # Remove "(millions)" as the function will add it
      "Country participation in the CS" = "Number of new substances",
      "Value"
    )

    # Filter data based on selected source
    article_data <- figure_article() %>%
      dplyr::filter(source == input$article_source)
    # Remove the division by 1e6, let the plotting function handle this

    # Create plot based on selected source using wrapper function
    createArticlePlot(
      data = article_data,
      source_title = input$article_source,
      y_title = y_title,
      flag_size_range = c(2, 6) # Adjusted flag size range for better visualization
    )
  })
}

shinyApp(ui, server)
