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
ds <- arrow::open_dataset("./data6/df_cc_3.parquet", format = "parquet")

# Then in your app.R, replace the world_data computation with:
# In global scope (outside server function)
# Load world map data once and collect immediately (more efficient)
world_data <- arrow::read_parquet("data/world_data.parquet") %>%
  as.data.frame()

# OPTIMIZATION: Instead of collecting all individual country data,
# just get the distinct country metadata needed for flags
country_metadata <- ds %>%
  dplyr::filter(!is.na(percentage)) %>%
  dplyr::select(iso2c, country) %>%
  dplyr::distinct() %>%
  dplyr::collect() %>%
  dplyr::arrange(country)


# OPTIMIZATION: Get the list of countries that appear in collaborations
# This creates a mapping from iso2c codes to the collaborations they're involved in
collab_countries <- ds %>%
  dplyr::filter(is_collab == TRUE) %>%
  dplyr::select(iso2c) %>%
  dplyr::distinct() %>%
  dplyr::collect()
# OPTIMIZATION: Create a function to generate flag buttons efficiently
create_flag_button <- function(iso, country_name) {
  tags$button(
    class = "btn btn-outline-secondary btn-sm",
    `data-iso` = iso,
    tags$img(
      src = sprintf("https://flagcdn.com/16x12/%s.png", tolower(iso)),
      width = 16,
      height = 12
    ),
    paste0(" ", country_name),
    onclick = sprintf("Shiny.setInputValue('selectedCountry', '%s', {priority: 'event'})", iso)
  )
}

# OPTIMIZATION: Cache for collaboration data by country
collab_cache <- new.env(parent = emptyenv())

# Precompute country metadata for flags

df_global_ind <- ds %>%
  dplyr::filter(!is.na(percentage)) %>%
  dplyr::select(
    iso2c, year, percentage, chemical,
    iso3c, country, lat, lng,
    region, is_collab, cc
  ) %>%
  dplyr::collect()

country_metadata <- df_global_ind %>% # Use df_global_ind as it contains individual countries
  dplyr::distinct(iso2c, country) %>%
  dplyr::arrange(country) # Sort here to ensure consistent order
# View(country_metadata)
# Precompute the flag buttons in the global scope
# Precompute the flag buttons in the global scope
precomputed_flags <- lapply(seq_len(nrow(country_metadata)), function(i) {
  iso <- country_metadata$iso2c[i]
  country_name <- country_metadata$country[i]
  tags$button(
    class = "btn btn-outline-secondary btn-sm",
    `data-iso` = iso,
    tags$img(
      src = sprintf("https://flagcdn.com/16x12/%s.png", tolower(iso)),
      width = 16,
      height = 12
    ),
    paste0(" ", country_name),
    onclick = sprintf("Shiny.setInputValue('selectedCountry', '%s', {priority: 'event'})", iso)
  )
})
names(precomputed_flags) <- country_metadata$iso2c

# View(precomputed_flags)

# collab_data <-  ds %>% 
#     dplyr::filter(is_collab == TRUE) %>%  # Use explicit namespace
#     dplyr::select(iso2c, year, percentage, chemical, iso3c, country, 
#            lat, lng, region, cc) 


# # Collaboration expansion
# # Replace tidyr::unnest with data.table equivalent
# collab_expansion <- df_global_collab[
#   , .(isoSplit = tstrsplit(iso2c, "-")),
#   by = .(iso2c, year, percentage)
# ]


# # Pre-compute the Highchart map data at startup instead of in reactive context
# map_data_cache <- list()

# # Initialize the cache for individual countries
# map_data_cache$individual <- df_global_ind %>%
#   group_by(iso3c, year, region) %>%
#   summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
#   group_by(iso3c) %>%
#   summarise(
#     value     = mean(yearly_avg, na.rm = TRUE),
#     best_year  = year[which.max(yearly_avg)],
#     worst_year = year[which.min(yearly_avg)],
#     region     = first(region),
#     .groups   = "drop"
#   )

# # Initialize the cache for collaborations with split iso3c renamed,
# # ensuring that the original iso3c value is captured separately.
# map_data_cache$collab_expanded <- df_global_collab %>%
#   mutate(
#     orig_iso = iso3c,
#     iso3c = strsplit(iso3c, "-"),
#     country = strsplit(country, "-")
#   ) %>%
#   tidyr::unnest(cols = iso3c) %>%
#   mutate(
#     combo = orig_iso,
#     value = percentage,
#     year = year
#   ) %>%
#   select(-orig_iso)


#   # Get world map data, excluding Antarctica
# world_data <- map_data("world") %>%
#   filter(region != "Antarctica") %>%
#   rename(country = region, lng = long) %>%
#   mutate(
#     country = case_when(
#       country == "USA" ~ "United States",
#       country == "UK" ~ "United Kingdom",
#       TRUE ~ country
#     )
#   )


# Add this to global.R or at the top of app.R
# Pre-compute collaboration data for each country
precompute_country_collaborations <- function(df_global_collab) {
  # Get all unique country codes
  all_countries <- unique(unlist(strsplit(df_global_collab$iso2c, "-")))
  
  # Create a container for pre-computed data
  country_collab_data <- list()
  
  # For each country, pre-compute its collaboration data
  for (iso in all_countries) {
    # Filter records where this country appears
    collab_records <- df_global_collab[grepl(iso, df_global_collab$iso2c), ]
    
    # For each record, determine the partner
    if (nrow(collab_records) > 0) {
      collab_records$partner <- sapply(strsplit(collab_records$iso2c, "-"), function(pair) {
        setdiff(pair, iso)[1]
      })
      
      country_collab_data[[iso]] <- collab_records
    }
  }
  
  return(country_collab_data)
}

# Load the collaboration data
df_global_collab <- ds %>%
  dplyr::filter(is_collab == TRUE) %>%
  dplyr::select(
    iso2c, year, percentage, chemical, iso3c, country,
    lat, lng, region, cc, flags
  ) %>%
  dplyr::collect()

# Pre-compute all collaboration data
country_collab_cache <- precompute_country_collaborations(df_global_collab)



# Add this to your pre-computation section
# Pre-compute collaboration summaries for each country
precompute_collab_summaries <- function(country_collab_cache, country_metadata) {
  country_summaries <- list()
  
  # For each country with collaboration data
  for (iso in names(country_collab_cache)) {
    if (!is.null(country_collab_cache[[iso]])) {
      relevant_data <- country_collab_cache[[iso]]
      
      # Calculate top contributors summary
      contributor_summary <- relevant_data %>%
        group_by(partner) %>%
        summarize(
          total_contribution = sum(percentage, na.rm = TRUE),
          mean_contribution = mean(percentage, na.rm = TRUE),
          peak_year = year[which.max(percentage)],
          peak_value = max(percentage, na.rm = TRUE),
          years_active = paste(sort(unique(year)), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(total_contribution)) %>%
        head(10)  # Get top 10 contributors
      
      # Get the ISO codes for the partners
      partner_iso_codes <- contributor_summary$partner
      
      # Create mapping of country names to match with iso codes
      country_mapping <- setNames(country_metadata$country, country_metadata$iso2c)
      
      # Add country names to the data
      contributor_summary$country_name <- sapply(partner_iso_codes, function(partner_iso) {
        country_mapping[partner_iso] %||% partner_iso
      })
      
      country_summaries[[iso]] <- contributor_summary
    }
  }
  
  return(country_summaries)
}

# Run the pre-computation
country_collab_summaries <- precompute_collab_summaries(country_collab_cache, country_metadata)



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
            actionButton("plotTopCountries", "Top 10 Countries", class = "btn-danger", style = "width: 100%;")
          )
        ),
    br(),
    fluidRow(
          column(
            width = 12,
            actionButton("plotTop100Countries", "Plot All", class = "btn-success", style = "width: 100%;")
          )
        ),
    hr(),
    div(
          style = "margin-bottom: 18rem;",
          accordion(
            id = "countryAccordion",
            open = FALSE,
            accordion_panel(
              "Select Countries 🎌",
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
              withSpinner(plotlyOutput("trendPlot", width = "100%"), color = "#024173"),
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
        tabsetPanel(
          id = "flag_tabs",
          tabPanel("All Countries", uiOutput("collabFlagButtons")),
          tabPanel("Top Collaborators", uiOutput("topCollabFlagButtons"))
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
          choices = c("Expansion of the CS", "China-US in the CS", "Annual growth rate of the GDP", "Number of Researchers",  "Country participation in the CS"),
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
  # Helper function for NA handling
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
  
  active_tab <- reactive(input$selected)

  # Flag to track if user manually cleared selections
  user_cleared <- FALSE

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
      source, condition, region
    ) %>%
    dplyr::collect()  # Remove collect() if ds is already in memory
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
  }) 
  # %>%
  #   bindCache(active_tab())

  # Initialize available countries (run once)
  all_countries <- reactive({
    # Get all available countries based on the active tab
    country_data <- active_dataset() %>%
      dplyr::distinct(country) %>%
      dplyr::collect()

    sort(unique(country_data$country))
  }) 
  # %>% bindCache(active_tab())

  # Get all available regions (regardless of filtering)
  all_regions <- reactive({
    active_dataset() %>%
      dplyr::distinct(region) %>%
      dplyr::filter(!is.na(region)) %>%
      dplyr::arrange(region) %>%
      dplyr::collect() %>%
      dplyr::pull(region)
  }) %>% bindCache(active_tab())

  # Get top countries (without using filtered_data)
  top_countries <- reactive({
    active_dataset() %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(val = sum(percentage, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(val)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::collect() %>%
      dplyr::pull(country)
  }) %>% bindCache(active_tab())

  # Initial country choices setup - don't set selected if user cleared
  observe({
    req(all_countries())

    # Don't auto-select if user explicitly cleared
    selected_countries <- if (user_cleared) {
      character(0)
    } else if (is.null(input$countries) || length(input$countries) == 0) {
      top_countries()
    } else {
      input$countries
    }

    updateCheckboxGroupInput(session, "countries",
      choices = all_countries(),
      selected = selected_countries
    )

    # Reset the flag
    user_cleared <- FALSE
  })

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
    updateCheckboxGroupInput(session, "countries", selected = top_countries())
  })

  # Fixed deselect all button using isolation and priority
  observeEvent(input$deselectAll,
    {
      user_cleared <- TRUE
      updateCheckboxGroupInput(session, "countries", selected = character(0))
    },
    priority = 10
  )

  observeEvent(input$plotTop100Countries, {
    user_cleared <- FALSE
    updateCheckboxGroupInput(session, "countries", selected = all_countries())
  })

  map_data <- reactive({
    req(filtered_data())

    filtered_data() %>%
      group_by(country, year, region) %>%
      summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
      group_by(country, region) %>%
      summarise(
        value = mean(yearly_avg, na.rm = TRUE),
        best_year = year[which.max(yearly_avg)],
        worst_year = year[which.min(yearly_avg)],
        .groups = "drop"
      )
  }) %>%
    bindCache(filtered_data())


  # Only load collab_top20 data when viewing that specific tab
  # collab_top20_data <- reactive({
  #   req(active_tab() == "Collaboration Trends 🤝")

  #   df_global_collab %>%
  #     filter(iso2c %in% c(
  #       "CN-US", "DE-US", "GB-US", "IN-US", "CN-JP", "CA-US",
  #       "DE-RU", "JP-US", "DE-FR", "FR-US", "ES-GB", "IT-US",
  #       # Rest of your codes...
  #     )) %>%
  #     filter(chemical == "All") %>%
  #     collect()
  # }) %>% bindCache(active_tab())



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
  
  # Debug prints
  message("Number of rows in filtered collaboration data: ", nrow(data))
  
  # Process the collaboration data by splitting country pairs
  map_data <- data %>%
    # Step 1: Get yearly averages by collaboration pair
    dplyr::group_by(country, year) %>%  # Use country instead of iso2c
    dplyr::summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
    
    # Step 2: Split the country codes and expand the data
    mutate(
      # Create a list with the pair of countries - FIX: Remove quotes around column name
      country_parts = strsplit(as.character(country), ","),
      # Extract individual countries with proper type handling
      country1 = sapply(country_parts, function(x) if(length(x) > 0) x[1] else NA_character_),
      country2 = sapply(country_parts, function(x) if(length(x) > 1) x[2] else NA_character_)
    ) %>%
    
    # Filter out any rows where splitting didn't work properly
    filter(!is.na(country1), !is.na(country2)) %>%
    
    # Step 3: Create separate rows for each country in the pair
    tidyr::pivot_longer(
      cols = c(country1, country2),
      names_to = "country_position",
      values_to = "individual_country"
    ) %>%
    
    # Step 4: Group by individual country to get aggregated statistics
    dplyr::group_by(individual_country, year) %>%
    dplyr::summarise(
      value = sum(yearly_avg, na.rm = TRUE),  # Sum all collaboration percentages
      collab_pairs = paste(sort(unique(country)), collapse = "; "),
      .groups = "drop"
    ) %>%
    
    # Step 5: Get best/worst years and overall average
    dplyr::group_by(individual_country) %>%
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      best_year = year[which.max(value)],
      worst_year = year[which.min(value)],
      collab_list = first(collab_pairs),  # This ensures collab_list exists
      .groups = "drop"
    ) %>%
    
    # Step 6: Rename to match the expected format for the plot function
    rename(country = individual_country) %>%
    # Add a placeholder for region as it might be needed
    mutate(region = NA_character_)
  
  # Debug print before plotting
  message("Number of countries after processing: ", nrow(map_data))
  
  # Generate the map plot
  createCollabMapPlot(
    df = map_data,
    world_df = world_data,
    fill_var = "value",
    fill_label = "Collaboration Strength",
    main_title = ""
  )
}) %>%
  bindCache(active_tab(), filtered_data())

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Substance Types
  # Substance Plots


  output$substancePlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)

    plot_data <- filtered_data() %>%
      filter(chemical == input$chemicalSelector)

    createTrendPlot(
      data = plot_data,
      label_var = "iso3c", # Use ISO codes for labels
      color_var = "cc",
      group_var = "country",
      label_size = 2.7,
      title = paste("Contribution to", input$chemicalSelector),
      top_n = 15 # Show labels for top 15 countries only
    )
  }) %>%
    bindCache(active_tab(), input$chemicalSelector, filtered_data())

  output$collabSubstancePlot <- renderPlotly({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)

    plot_data <- filtered_data() %>%
      filter(chemical == input$chemicalSelector)

    createTrendPlot(
      data = plot_data,
      label_var = "iso3c", # Use ISO codes for labels
      color_var = "cc",
      region_var = "country",
      group_var = "country",
      label_size = 2.7,
      title = paste("Contribution to", input$chemicalSelector),
      top_n = 10 # Show labels for top 15 countries only
    )
  }) %>%
    bindCache(active_tab(), input$chemicalSelector, filtered_data())


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add to UI section where your flags are displayed
output$collabFlagButtons <- renderUI({
  req(active_tab() == "Collaboration Trends B 🤝")
  
  # Pagination parameters
  page_size <- 20
  current_page <- input$flag_page %||% 1
  search_term <- tolower(input$flag_search %||% "")
  
  # Filter countries based on search
  filtered_countries <- country_metadata
  if (nchar(search_term) > 0) {
    filtered_countries <- filtered_countries[grepl(search_term, tolower(filtered_countries$country)), ]
  }
  
  # Calculate pagination
  total_items <- nrow(filtered_countries)
  total_pages <- ceiling(total_items / page_size)
  start_idx <- (current_page - 1) * page_size + 1
  end_idx <- min(current_page * page_size, total_items)
  
  # Only process flags for the current page
  page_countries <- filtered_countries[start_idx:end_idx, ]
  
  # Generate flags for current page only
  flag_buttons <- lapply(1:nrow(page_countries), function(i) {
    iso <- page_countries$iso2c[i]
    country_name <- page_countries$country[i]
    
    tags$button(
      id = paste0("flag-", iso),
      class = "btn btn-outline-secondary btn-sm m-1",
      `data-iso` = iso,
      tags$img(
        src = sprintf("https://flagcdn.com/16x12/%s.png", tolower(iso)),
        width = 16,
        height = 12,
        alt = country_name
      ),
      paste0(" ", country_name),
      onclick = sprintf("Shiny.setInputValue('selectedCountry', '%s', {priority: 'event'})", iso)
    )
  })
  
  # Wrap in a div with pagination controls
  tagList(
    div(
      class = "mb-3",
      textInput("flag_search", "Search country:", ""),
      span(class = "text-muted", 
           paste("Showing", start_idx, "to", end_idx, "of", total_items, "countries"))
    ),
    div(class = "d-flex flex-wrap gap-2", flag_buttons),
    div(
      class = "d-flex justify-content-between mt-3",
      actionButton("prev_page", "Previous", 
                  class = if(current_page == 1) "btn btn-secondary disabled" else "btn btn-secondary"),
      span(paste("Page", current_page, "of", total_pages)),
      actionButton("next_page", "Next", 
                  class = if(current_page == total_pages) "btn btn-secondary disabled" else "btn btn-secondary")
    )
  )
})

# Add these observers for pagination
observeEvent(input$prev_page, {
  updateNumericInput(session, "flag_page", 
                     value = max(1, (input$flag_page %||% 1) - 1))
})

observeEvent(input$next_page, {
  current_page <- input$flag_page %||% 1
  search_term <- tolower(input$flag_search %||% "")
  
  filtered_countries <- country_metadata
  if (nchar(search_term) > 0) {
    filtered_countries <- filtered_countries[grepl(search_term, tolower(filtered_countries$country)), ]
  }
  
  total_pages <- ceiling(nrow(filtered_countries) / 20)
  updateNumericInput(session, "flag_page", 
                     value = min(total_pages, current_page + 1))
})

observeEvent(input$flag_search, {
  # Reset to first page when searching
  updateNumericInput(session, "flag_page", value = 1)
})

# Update your observeEvent for selectedCountry
# Replace your observeEvent for selectedCountry with this optimized version
observeEvent(input$selectedCountry, {
  sel_iso <- input$selectedCountry
  
  # Use the pre-computed data or filter on demand if not pre-computed
if (!is.null(country_collab_summaries[[sel_iso]])) {
  contributor_summary <- country_collab_summaries[[sel_iso]]
    
# Create the GT table
  output$countryCollabTable <- render_gt({
    contributor_summary %>%
      select(
        country_code = partner,
        country_name,
        total_contribution,
        peak_year,
        peak_value
      ) %>%
      gt() %>%
      tab_header(
        title = paste("Top Collaborators with", country_name),
        subtitle = "Ranked by total contribution percentage"
      ) %>%
      fmt_percent(
        columns = c(total_contribution, peak_value),
        decimals = 2,
        scale_values = TRUE
      ) %>%
      fmt_number(
        columns = peak_year,
        decimals = 0
      ) %>%
      fmt_flag(
        columns = country_code
      ) %>%
      cols_merge(
        columns = c(country_name, country_code),
        pattern = "{2} {1}"
      ) %>%
      cols_label(
        country_name = "Country",
        total_contribution = "Total Contribution",
        peak_year = "Peak Year",
        peak_value = "Peak Value"
      ) %>%
      tab_style(
        style = cell_fill(color = "#f8f9fa"),
        locations = cells_body(rows = seq(1, nrow(contributor_summary), 2))
      ) %>%
      opt_row_striping() %>%
      opt_interactive(
        use_search = TRUE,
        use_filters = TRUE,
        use_resizers = TRUE,
        use_highlight = TRUE,
        use_compact_mode = FALSE
      )
    })
  } else {
    showNotification(paste("No collaboration data found for country code:", sel_iso), 
                    type = "warning")
  }
})

  #################
  # Article Figures
  ##################
  # app.R (server)
output$articlePlot <- renderPlotly({
  req(active_tab() == "Article Figures 📰", input$article_source)

  # First define the y-axis title based on the source
  y_title <- switch(input$article_source,
    "Expansion of the CS" = "Percentage of new substances",
    "China-US in the CS" = "Percentage of national contribution", 
    "Annual growth rate of the GDP" = "GDP per capita growth (annual %)",
    "Number of Researchers" = "Researchers",
    "Country participation in the CS" = "Number of new substances",
    "Value" # default title if none match
  )

  # Call the reactive expression with () to get its value
  article_data <- figure_article() %>% 
    dplyr::filter(source == input$article_source) %>%
    dplyr::mutate(percentage = ifelse(source == "Number of Researchers", percentage/1e6, percentage))

  createArticlePlot(
    data = article_data,
    source_title = input$article_source,
    y_title = y_title
  )
})
  # %>% bindCache(input$article_source)
}

shinyApp(ui, server)
