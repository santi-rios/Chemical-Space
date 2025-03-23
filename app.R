# app.R
library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(tidytable)
library(countrycode)
library(leaflet)
library(highcharter)
# library(viridisLite)
library(glue)
library(ggplot2)
theme_set(theme_light())
library(data.table)
library(shinycssloaders)
library(RColorBrewer)

source("R/plot_function.R")

# Efficient data preparation using Arrow and dplyr
# Efficient data preparation using Arrow and dplyr
ds <- arrow::open_dataset("./data6/df_cc.parquet", format = "parquet") %>%
  as_tidytable()

# Pre-filter at the Arrow level before collecting


ds_filtered <- ds %>%
  select(
    iso2c, year, percentage, chemical,
    iso3c, country, lat, lng,
    region, is_collab, cc
  ) %>%
  filter(!is.na(percentage)) %>%
  filter(!country %in% c("Fiji"))

# Full dataset with only needed columns - collect once
df_global <- ds_filtered %>% dplyr::collect()

# Use data.table directly - faster than tidytable for these operations
setDT(df_global)

# Create indices on frequently filtered columns for faster subsetting
data.table::setindex(df_global, is_collab, country, year, iso2c, iso3c, region, cc) # nolint

# Create views instead of copies - much more memory efficient
df_global_ind <- df_global[is_collab == FALSE]

df_global_collab <- df_global[is_collab == TRUE]

# top20_groups <- df_global_collab %>%
#  group_by(iso2c) %>%
#  summarise(val = sum(percentage, na.rm = TRUE)) %>%
#  arrange(desc(val)) %>%
#  head(25) %>%
#  pull(iso2c)

# print(top20_groups)

df_global_collab_top20 <- df_global_collab %>%
  filter(iso2c %in% c(
    "CN-US", "DE-US", "GB-US", "IN-US", "CN-JP", "DE-RU", "CA-US", "JP-US", "DE-FR",
    "FR-US", "ES-GB", "IT-US", "DE-ES", "ES-FR", "CN-DE", "KR-US",
    "FR-GB", "DE-GB", "ES-IT", "DE-IN", "ES-US"
  ))

# Define the 20 country codes for collaborations
us_codes <- c("DE-US", "GB-US", "IN-US", "CA-US", "JP-US", "FR-US", "IT-US", "KR-US", "ES-US")
other_codes <- c("CN-US", "CN-JP", "CN-DE", "DE-RU", "DE-FR", "ES-GB", "DE-ES", "ES-FR", "FR-GB", "DE-GB", "ES-IT", "DE-IN")

# Generate 10 distinct blue shades (avoid very light hues) for codes containing "US"
us_colors <- colorRampPalette(brewer.pal(9, "Blues"))(length(us_codes))
# For the remaining countries, use a contrasting palette (e.g., "Paired" has 9 distinct colours)
other_colors <- colorRampPalette(brewer.pal(9, "Set1"))(length(other_codes))

# Create a named vector for mapping
collab_color_map <- c(
  setNames(us_colors, us_codes),
  setNames(other_colors, other_codes)
)

# --- Pre-calculate Initial Top 10 Countries (GLOBAL SCOPE) ---
# initial_top_countries_df <- df_global_ind %>%
#   group_by(country) %>%
#   summarise(val = sum(percentage, na.rm = TRUE)) %>%
#   arrange(desc(val)) %>%
#   head(10)

# initial_top_countries <- initial_top_countries_df$country
# -----------------------------------------------------------
# Remove the existing color mapping code,
# and in your ggplot calls simply map the color aesthetic to your new 'cc' column.
# Then use scale_color_identity() so ggplot uses the hex color values directly:

# Precompute country metadata for flags
country_metadata <- df_global_ind %>% # Use df_global_ind as it contains individual countries
  distinct(iso2c, country) %>%
  arrange(country) # Sort here to ensure consistent order

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



# Collaboration expansion
# Replace tidyr::unnest with data.table equivalent
collab_expansion <- df_global_collab[
  , .(isoSplit = tstrsplit(iso2c, "-")), 
  by = .(iso2c, year, percentage)
]


# Pre-compute the Highchart map data at startup instead of in reactive context
map_data_cache <- list()

# Initialize the cache for individual countries
map_data_cache$individual <- df_global_ind %>%
  group_by(iso3c, year, region) %>%
  summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
  group_by(iso3c) %>%
  summarise(
    value     = mean(yearly_avg, na.rm = TRUE),
    best_year  = year[which.max(yearly_avg)],
    worst_year = year[which.min(yearly_avg)],
    region     = first(region),
    .groups   = "drop"
  )

# Initialize the cache for collaborations with split iso3c renamed,
# ensuring that the original iso3c value is captured separately.
map_data_cache$collab_expanded <- df_global_collab %>%
  mutate(
    orig_iso = iso3c,
    iso3c = strsplit(iso3c, "-"),
    country = strsplit(country, "-")
  ) %>%
  tidyr::unnest(cols = iso3c) %>%
  mutate(
    combo = orig_iso,
    value = percentage,
    year = year
  ) %>%
  select(-orig_iso)


  # Get world map data, excluding Antarctica
world_data <- map_data("world") %>%
  dplyr::filter(region != "Antarctica")


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
      "years", "📅 Year Range",
      min = 1996, max = 2022,
      value = c(1996, 2022),
      step = 1, sep = "", animate = FALSE,
      width = "100%"
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
    open = TRUE,
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

  # ------------------------------
  # 1) NATIONAL TRENDS (INDIVIDUAL)
  # ------------------------------
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
            withSpinner(plotlyOutput("trendPlot", width = "100%", height = 800), color = "#024173"),
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
            plotlyOutput("mapPlot")
          ),
          nav_panel(
            "Cartogram🗺️",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Cartogram showing geographic distribution...",
              placement = "left"
            ),
            leafletOutput("geoPlot2", height = 750),
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

  # ------------------------------
  # 2) COLLABORATION TRENDS
  # ------------------------------

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
            withSpinner(plotlyOutput("collabTrendPlot", width = "100%", height = 800), color = "#024173"),
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
          ),
          nav_panel(
            "Collaborative contribution visualization 🌍",
            fluidRow(
              column(
                width = 12,
                tags$h4("Collaborative contribution visualization"),
                tags$img(
                  src = "collabs.gif",
                  width = "30%",
                  style = "display:block; margin:0 auto;"
                ),
                p("Map with lines (routes) connecting countries. The line width represents the percentage value for each year.")
              )
            )
          )
        )
      ),
      fluidPage(
        fluidRow(
          column(
            width = 12,
            tags$h4("All Countries' Flags:"),
            uiOutput("collabFlagButtons")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$h4("Collaboration Partners Trend:"),
            plotlyOutput("collabPartnersPlot", height = "400px")
          )
        )
      )
    )
      ),

      # ------------------------------
      # 3) ARTICLE FIGURES
      # ------------------------------
  nav_panel(
    "Article Figures 📰",
    fluidRow(
      column(
        width = 12,
        selectInput(
          "selected_figure", "Select Figure",
          choices = c("Country participation in the CS" = "figure1a.gif",
                      "China-US in the CS" = "figure1c.gif",
                      "Annual growth rate of the GDP" = "figure1d.gif",
                      "Number of researchers in research" = "figure1e.gif"),
          selected = "figure1a.gif",
          width = "40%"
        )
      )
    ),
    # Display a caption and a popover with additional text
    card_footer(
      uiOutput("figureCaption"),
      popover(
        a("Learn more", href = "#"),
        markdown(uiOutput("figureDescription"))
      )
    ),
    # Display the GIF image using uiOutput wrapped in a spinner
    withSpinner(uiOutput("figureDisplay"), color = "#024173"),
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

      # ------------------------------
      # 5) KNOW MORE
      # ------------------------------
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
  ###################
  # Reactive Values #
  ###################

  # Track active tab
  active_tab <- reactive(input$selected)

  # Precomputed connection data cache
  connection_cache <- reactiveValues()

  # Update region choices (National Trends only)
  observe({
    req(active_tab() == "National Trends 📈")
    region_choices <- unique(df_global_ind$region[!is.na(df_global_ind$region)])
    updateSelectizeInput(session, "region", choices = c("All", sort(region_choices)), selected = "All")
  })

  # Base data reactive with caching
  df <- reactive({
    if (active_tab() == "National Trends 📈") {
      res <- df_global_ind
      if (!is.null(input$region) && !("All" %in% input$region)) {
        res <- res[region %in% input$region]
      }
    } else {
      res <- df_global_collab
    }
    res
  }) %>% bindCache(active_tab(), input$region)

  # Country selection updates
  observe({
    req(df())
    valid_countries <- sort(unique(df()$country))
    top_countries <- df() %>%
      group_by(country) %>%
      summarise(val = sum(percentage, na.rm = TRUE)) %>%
      arrange(desc(val)) %>%
      head(10)
    updateCheckboxGroupInput(session, "countries", choices = valid_countries, selected = top_countries$country)
  }) 

  # Filtered data with caching
  filtered_data <- reactive({
    req(input$countries, input$years)
    df() %>%
      filter(
        year >= input$years[1] & year <= input$years[2] &
          country %in% input$countries
      )
  }) %>% 
  bindCache(active_tab(), input$years, input$countries) %>%
  debounce(300)

  #########
  # Observers #
  #########

  # Observe event for "Top 10 Countries" button
  observeEvent(input$plotTopCountries, {
    current_top <- df() %>%
      group_by(country) %>%
      summarise(total = sum(percentage, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      slice_head(n = 10) %>%
      pull(country)
    updateCheckboxGroupInput(session, "countries", selected = current_top)
  })

  # Observe event for "Deselect All" button
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "countries", selected = character(0))
  })

  # Observe event for "Top 100 Countries" button
  observeEvent(input$plotTop100Countries, {
    all_countries <- sort(unique(df()$country))
    updateSelectizeInput(session, "countries", selected = all_countries)
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
    req(active_tab() == "Collaboration Trends 🤝")

    data <- df_global_collab_top20 %>% filter(chemical == "All")

    p <- ggplot(
      data,
      aes(
        x = year,
        y = percentage,
        color = iso2c,
        group = iso2c,
        text = paste0(
          "<b>Country:</b> ", iso2c,
          "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01, scale = 1),
          "<br><b>Year:</b> ", year,
          "<br><b>Region:</b> ", region
        )
      )
    ) +
      geom_line() +
      geom_point(aes(size = percentage / 100), alpha = 0.4, show.legend = FALSE) +
      geom_text(
        data = data %>% filter(year == max(year)),
        aes(y = percentage, label = iso3c, color = iso2c),
        hjust = -0.2, nudge_x = 0.3, nudge_y = 0.4,
        size = 4, check_overlap = TRUE, show.legend = FALSE
      ) +
      # scale_color_manual(values = collab_color_map) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_blank()
      ) +
      labs(title = "Collaborative Contributions to Chemical Space", y = "% of New Substances")

    ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3
        ),
        margin = list(b = 50)
      ) %>%
      plotly::toWebGL()
  })
  # %>% bindCache(active_tab(), filtered_data())

  # Highchart map - Individual Countries
output$mapPlot <- renderPlotly({
  # req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)
  req(active_tab() == "National Trends 📈")

  # Process data for mapping
  # Prepare your filtered data
  map_data <- df_global_ind %>%
    filter(chemical == "All") %>%
    group_by(iso3c, year, region, country) %>%
    summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop")

  # Join by region name if it matches your data
  joined_data <- world_data %>%
    right_join(map_data, by = c("region" = "country"))

  # Plotly-ready ggplot
  mapa_plot <- ggplot(joined_data, aes(long, lat, 
                                       group = group, 
                                       fill = yearly_avg)) +
    geom_polygon(aes(frame = year), color = "white", size = 0.1) +
    theme_void() +
    viridis::scale_fill_viridis(option = "B", name = "Average (%)") +
    labs(title = "Average Contribution") +
    coord_fixed(ratio = 1.3)

  ggplotly(mapa_plot) %>%
    animation_slider(currentvalue = list(prefix = "Year: ", font = list(color = "orange")))
})
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Substance Types
  # Substance Plots


  output$substancePlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)
    data <- filtered_data() %>%
      filter(chemical == input$chemicalSelector)

    p <- ggplot(data, aes(
      x = year, y = percentage, color = country, group = country,
      text = paste0(
        "<b>Country:</b> ", country,
        "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01, scale = 1),
        "<br><b>Year:</b> ", year,
        "<br><b>Region:</b> ", region
      )
    )) +
      geom_line() +
      geom_point(aes(size = percentage / 100),
        alpha = 0.4, show.legend = FALSE
      ) +
      labs(
        title = "Percentage of new compounds reported by each country in journals",
        x = "Year",
        y = "Percentage of new substances"
      ) +
      geom_text(
        data = data %>% filter(year == max(year)),
        aes(y = percentage, label = iso3c, color = country),
        hjust = -0.2, nudge_x = 0.3, nudge_y = 0.4,
        size = 4, check_overlap = TRUE, show.legend = FALSE
      ) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1))


    ggplotly(p, tooltip = "text") %>%
      plotly::partial_bundle() %>%
      plotly::toWebGL()
  })
  # %>% bindCache(input$chemicalSelector, input$countries, input$years)

  output$collabSubstancePlot <- renderPlotly({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)

    data <- filtered_data() %>%
      filter(chemical == input$chemicalSelectorcollab)

    p <- ggplot(data, aes(
      x = year, y = percentage, color = iso2c, group = iso2c,
      text = paste0(
        "<b>Country:</b> ", iso2c,
        "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01, scale = 1),
        "<br><b>Year:</b> ", year,
        "<br><b>Region:</b> ", region
      )
    )) +
      geom_line() +
      geom_point(aes(size = percentage / 100),
        alpha = 0.4, show.legend = FALSE
      ) +
      labs(
        title = "Percentage of new compounds reported by each country in journals",
        x = "Year",
        y = "Percentage of new substances"
      ) +
      geom_text(
        data = data %>% filter(year == max(year)),
        aes(y = percentage, label = iso3c, color = iso2c),
        hjust = -0.2, nudge_x = 0.3, nudge_y = 0.4,
        size = 4, check_overlap = TRUE, show.legend = FALSE
      ) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1))

    # p <- p + scale_color_manual(values = collab_color_map)

    ggplotly(p, tooltip = "text") %>%
      plotly::partial_bundle() %>%
      plotly::toWebGL()
  })
  # %>% bindCache(active_tab(), input$collabChemicalSelector, filtered_data())
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # -------------------------------
  # Render the pre-calculated flag buttons
  output$collabFlagButtons <- renderUI({
    req(active_tab() == "Collaboration Trends 🤝")
    div(class = "d-flex flex-wrap gap-2", precomputed_flags)
  })

  # -------------------------------
  # Observe flag clicks and update the partners plot (using Plotly)
  observeEvent(input$selectedCountry, {
    sel_iso <- input$selectedCountry

    # Filter the collaboration data from the complete collab dataset (do not apply any region or main trends filters)
    relevant_data <- df_global_collab[grepl(sel_iso, iso2c, country)]

    if (nrow(relevant_data) == 0) {
      showNotification(paste("No collaboration data found for country code:", sel_iso), type = "warning")
      return()
    }

    # For each record, determine the collaborating partner.
    # This assumes iso2c is formatted as "XX-YY" (a two-country code)
    relevant_data <- relevant_data %>%
      mutate(
        partner = sapply(strsplit(country, "-"), function(x) { # Use iso2c here as it contains the collaboration pair
          # Remove the selected country's code and take the first remaining value
          setdiff(x, sel_iso)[1]
        })
      )

    # Create an interactive Plotly plot showing trends for each collaboration partner
    p <- ggplot(relevant_data, aes(
      x = year,
      y = percentage,
      color = partner,
      group = partner,
      text = paste0(
        "Partner: ", partner,
        "<br>Year: ", year,
        "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01, scale = 1)
      )
    )) +
      geom_point(shape = 4) +
      labs(
        title = paste("Collaboration Trends for", sel_iso, "with Partners"),
        x = "Year",
        y = "Percentage",
        color = "Partner"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    # Update the dedicated Plotly output with the new plot
    output$collabPartnersPlot <- renderPlotly({
      ggplotly(p, tooltip = "text") %>%
        plotly::toWebGL()
    })
  })
  #################
  # MAP GIF
  ##################



  #################
  # Article Figures
  ##################

  # Define the texts and captions for each figure
  figure_info <- list(
    "figure1a.gif" = list(
      caption = "Country Contribution to the Chemical Space",
      description = paste(
        "Based on data retrieved from Reaxys®, Dimensions, and OpenAlex for 1996-2022,",
        "this figure shows that China now covers 41% of the chemical space,",
        "dwarfing the US share of 11%. China’s exponential growth led it to become",
        "the leading contributor from 2013 onward."
      )
    ),
    "figure1e.gif" = list(
      caption = "Research Workforce Trends",
      description = paste(
        "Since 2005, China has boasted a considerably larger number of researchers than the US,",
        "a trend that has fueled its rapid expansion in the chemical space."
      )
    ),
    "figure1c.gif" = list(
      caption = "Solo vs Collaborative Contributions",
      description = paste(
        "Over 90% of China’s chemical space contributions come from domestic teams,",
        "while the US experienced a marked decline in solo contributions—from over 95% to less than 80%—",
        "offset by a rise in international collaborations, notably with China."
      )
    ),
    "figure1d.gif" = list(
      caption = "Economic Growth and Financial Dynamics",
      description = paste(
        "This figure highlights China’s thriving economy in contrast to the US,",
        "where a larger national debt and slower economic growth underscore the differences",
        "in financial dynamics between the two nations."
      )
    )
  )
  
  # Render the selected image
  output$figureDisplay <- renderUI({
    tags$img(
      src = input$selected_figure,
      width = "40%",
      style = "display:block; margin:0 auto;"
    )
  })
  
  # Render the caption text for the selected figure
  output$figureCaption <- renderUI({
    tags$p(
      figure_info[[input$selected_figure]]$caption,
      style = "text-align:center; font-weight:bold;"
    )
  })
  
  # Render the detailed description for the selected figure
  output$figureDescription <- renderText({
    figure_info[[input$selected_figure]]$description
  })


}

shinyApp(ui, server)

