# app.R
library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(tidytable)
library(countrycode)
library(leaflet)
library(highcharter)
library(viridisLite)
library(glue)
library(ggplot2)
theme_set(theme_light())
library(data.table)
library(shinycssloaders)
library(RColorBrewer)

# Efficient data preparation using Arrow and dplyr
ds <- arrow::open_dataset("./data6/df.parquet", format = "parquet") %>%
  as_tidytable()

# Pre-filter at the Arrow level before collecting
ds_filtered <- ds %>%
  filter(!is.na(percentage)) %>%
  filter(!country %in% (country %in% c("IN-IQ", "US-UY"))) %>%
  select(
    iso2c, year, percentage, chemical,
    iso3c, country, lat, lng,
    region, is_collab
  )

# Full dataset with only needed columns - collect once
df_global <- ds_filtered %>% dplyr::collect()

# Use data.table directly - faster than tidytable for these operations
setDT(df_global)

# Create indices on frequently filtered columns for faster subsetting
data.table::setindex(df_global, is_collab, country, year, iso2c, iso3c, region, chemical) # nolint

# Create views instead of copies - much more memory efficient
df_global_ind <- df_global[is_collab == FALSE]
df_global_collab <- df_global[is_collab == TRUE]

# top20_groups <- df_global_collab %>%
#   group_by(iso2c) %>%
#   summarise(val = sum(percentage, na.rm = TRUE)) %>%
#   arrange(desc(val)) %>%
#   head(25) %>%
#   pull(iso2c)

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



# Articles: only load columns needed
figure_article <- ds %>%
  filter(!is.na(percentage_x)) %>%
  select(
    percentage = percentage_x,
    country = country_x,
    year = year_x,
    source
  ) %>%
  dplyr::collect()

# Element figures: only load columns needed
df_figures <- ds %>%
  filter(!is.na(percentage_y)) %>%
  select(
    16:40
  ) %>%
  rename(
    percentage = percentage_y,
    year = year_y,
    chemical = chemical_y
  ) %>%
  dplyr::collect()

# --- Pre-calculate Initial Top 10 Countries (GLOBAL SCOPE) ---
initial_top_countries_df <- df_global_ind %>%
  group_by(country) %>%
  summarise(val = sum(percentage, na.rm = TRUE)) %>%
  arrange(desc(val)) %>%
  head(10)

initial_top_countries <- initial_top_countries_df$country
# -----------------------------------------------------------

# Color mappings (in-memory)
all_ctry <- sort(unique(df_global$country))
color_map_all <- setNames(viridisLite::viridis(length(all_ctry)), all_ctry)

override_countries <- c(
  "China", "United States of America", "India", "Germany",
  "Japan", "United Kingdom", "France", "Russia", "Mexico",
  "Colombia", "Brazil", "Ecuador", "Argentina"
)

override_colors <- c(
  "#c5051b", "#0a3161", "#ff671f", "#000000",
  "#995162", "#3b5091", "#000091", "#d51e9b",
  "#006341", "#fcd116", "#009b3a", "#ffdd00", "#74acdf"
)

color_map_all[override_countries] <- override_colors

# Precompute country metadata for flags
country_metadata <- df_global %>%
  distinct(iso2c, country)

precomputed_flags <- lapply(country_metadata$iso2c, function(iso) {
  tags$button(
    class = "btn btn-outline-secondary btn-sm",
    `data-iso` = iso,
    tags$img(
      src = sprintf("https://flagcdn.com/16x12/%s.png", tolower(iso)),
      width = 16,
      height = 12
    ),
    paste0(" ", iso)
  )
})
names(precomputed_flags) <- country_metadata$iso2c

# Collaboration expansion
collab_expansion <- df_global_collab %>%
  select(iso2c, year, percentage) %>%
  mutate(isoSplit = strsplit(iso2c, "-")) %>%
  tidytable::unnest(isoSplit) %>%
  group_by(isoSplit) %>%
  arrange(desc(percentage))


# Pre-compute the Highchart map data at startup instead of in reactive context
map_data_cache <- list()

# Initialize the cache for individual countries
map_data_cache$individual <- df_global_ind %>%
  group_by(iso3c, year, region) %>%
  summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
  group_by(iso3c) %>%
  summarise(
    value      = mean(yearly_avg, na.rm = TRUE),
    best_year  = year[which.max(yearly_avg)],
    worst_year = year[which.min(yearly_avg)],
    region     = first(region),
    .groups    = "drop"
  )

# Initialize the cache for collaborations with split iso3c renamed,
# ensuring that the original iso3c value is captured separately.
map_data_cache$collab_expanded <- df_global_collab %>%
  mutate(
    orig_iso = iso3c,
    iso3c = strsplit(iso3c, "-")
  ) %>%
  tidyr::unnest(cols = iso3c) %>%
  mutate(
    combo = orig_iso,
    value = percentage,
    year = year
  ) %>%
  select(-orig_iso)


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
              "China's Chemical Revolution: From 1996 to 2022...",
              placement = "left"
            ),
            withSpinner(plotlyOutput("trendPlot", width = "100%", height = 800), color = "#024173")
          ),
          nav_panel(
            "Map📌",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Interactive map showing country contributions...",
              placement = "left"
            ),
            highchartOutput("mapPlot")
          ),
          nav_panel(
            "Cartogram🗺️",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Cartogram showing geographic distribution...",
              placement = "left"
            ),
            leafletOutput("geoPlot2", height = 750)
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
            withSpinner(plotlyOutput("substancePlot", width = "100%"), color = "#024173")
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
            withSpinner(plotlyOutput("collabTrendPlot", width = "100%", height = 800), color = "#024173")
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
            withSpinner(plotlyOutput("collabSubstancePlot", width = "100%"), color = "#024173")
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
          width = "85%"
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
    withSpinner(uiOutput("figureDisplay"), color = "#024173")
  ),

      # ------------------------------
      # 4) ELEMENT FIGURES
      # ------------------------------
      nav_panel(
        "Element Figures 🧪",
        fluidPage(
          fluidRow(
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Elemental and compositional spans of the regions of the chemical space. A few other organogenic elements, mainly H followed by N and O, constitute most of the organic compounds. In terms of compositions, most of the organic CS is concentrated on CHNO compounds.", # nolint: line_length_linter.
              placement = "left"
            ),
            column(
              width = 12,
              plotOutput("compositionPlot", height = "600px")
            )
          ),
          fluidRow(
            column(
              width = 8,
              plotOutput("periodicTablePlot", click = "plot_click", height = "600px")
            ),
            column(
              width = 4,
              uiOutput("elementInfo")
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
              h3("China's rise in the chemical space and the decline of US influence"),
              p("This dashboard is based on the study ‘China's rise in the chemical space and the decline of US influence’. Between 1996 and 2022, the research shows that China has emerged as a dominant force in chemical discovery—especially after 2013—mainly through national efforts, while US contributions depend largely on international collaborations."),
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
  # %>% bindCache(active_tab())

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
  })
  # %>% bindCache(active_tab(), input$region)

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
  # %>% bindCache(df())

  # Filtered data with caching
  filtered_data <- reactive({
    req(input$countries, input$years)
    df() %>%
      filter(
        year >= input$years[1] & year <= input$years[2] &
          country %in% input$countries
      )
  })
  # %>% bindCache(active_tab(), input$years, input$countries, input$region) %>%
  # debounce(300)


  # Observe event for "Top 10 Countries" button
  observeEvent(input$plotTopCountries, {
    top_10_countries_df <- df() %>%
      group_by(country) %>%
      summarise(val = sum(percentage, na.rm = TRUE)) %>%
      arrange(desc(val)) %>%
      head(20)
    top_20_countries <- top_20_countries_df$country
    updateCheckboxGroupInput(session, "countries", selected = top_10_countries_df)
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
  output$trendPlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)
    data <- filtered_data() %>% filter(chemical == "All")

    p <- ggplot(
      data,
      aes(
        x = year,
        y = percentage,
        color = country,
        group = country,
        text = paste0(
          "<b>Country:</b> ", country,
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
        aes(y = percentage, label = iso3c, color = country),
        hjust = -0.2, nudge_x = 0.3, nudge_y = 0.4,
        size = 4, check_overlap = TRUE, show.legend = FALSE
      ) +
      scale_color_manual(values = color_map_all) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_blank()
      ) +
      labs(title = "National Contributions to Chemical Space", y = "% of New Substances")

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
      scale_color_manual(values = collab_color_map) +
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
  # National Map
  output$mapPlot <- renderHighchart({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)

    map_data <- filtered_data() %>%
      group_by(iso3c, year, region) %>%
      summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
      group_by(iso3c) %>%
      summarise(
        value      = mean(yearly_avg, na.rm = TRUE),
        best_year  = year[which.max(yearly_avg)],
        worst_year = year[which.min(yearly_avg)],
        region     = first(region),
        .groups    = "drop"
      )

    hcmap("custom/world-robinson-lowres.js",
      data   = map_data,
      joinBy = c("iso-a3", "iso3c"),
      value  = "value",
      name   = "Average Contribution"
    ) %>%
      hc_colorAxis(
        minColor = "#071f33",
        maxColor = "#e8041e",
        labels   = list(format = "{value}%"),
        title    = list(text = "Contribution (%)", style = list(color = "white"))
      ) %>%
      hc_tooltip(pointFormat = paste0(
        "{point.name}: {point.value:.2f}%, <br> Region: {point.region}, <br>",
        "<br> Best Year: {point.best_year}, <br>",
        "Worst Year: {point.worst_year}"
      )) %>%
      hc_mapNavigation(
        enabled = TRUE,
        enableMouseWheelZoom = TRUE,
        enableDoubleClickZoom = TRUE
      ) %>%
      hc_subtitle(
        text = paste0(
          "Mean value of Top Country in current selection = ",
          scales::percent(max(map_data$value, na.rm = TRUE),
            accuracy = 0.01,
            scale = 1
          )
        ),
        style = list(color = "black")
      ) %>%
      hc_title(
        text  = paste0("Top Country (Total Value) = ", map_data$iso3c[which.max(map_data$value)]),
        style = list(color = "black")
      )
  })
  # %>% bindCache(input$data_mode, input$region, input$countries, input$years)


  # collab_data <- reactive({
  #   req(filtered_data(), active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)
  #   dt <- as.data.table(filtered_data())
  #   dt[, Value := percentage]
  #   dt[, Year := year]
  #   dt
  # })


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Cartogram

  # Cartogram (National only)
  carto_data <- reactive({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)
    filtered_data() %>%
      group_by(country, lat, lng) %>%
      summarise(value = mean(percentage, na.rm = TRUE), .groups = "drop")
  })
  # %>% bindCache(active_tab(), filtered_data())

  # Initialize map
  # Initialize map only once and use proxy more effectively
  output$geoPlot2 <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group = "NASA") %>%
      addProviderTiles("CartoDB.Positron", group = "Continents") %>%
      addLayersControl(
        baseGroups = c("NASA", "Continents"),
        overlayGroups = c("Markers"),
        position = "topright"
      ) %>%
      setView(lng = 0, lat = 30, zoom = 2)
  })

  # Update markers and legend whenever carto_data changes
  # Cartogram (National only)
  # Cartogram (National only)
  carto_data <- reactive({
    req(active_tab() == "National Trends 📈")
    filtered_data() %>%
      group_by(country, lat, lng) %>%
      summarise(value = mean(percentage, na.rm = TRUE), .groups = "drop")
  })
  # %>% bindCache(active_tab(), filtered_data())

  observe({
    data <- carto_data()
    pal <- colorNumeric("Reds", domain = data$value)

    leafletProxy("geoPlot2", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~ scales::rescale(value, c(5, 30)),
        color = ~ pal(value),
        fillOpacity = 0.7,
        group = "Markers",
        popup = ~ glue("<b>{country}</b><br>Average: {round(value, 2)}%")
      ) %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("NASA", "Continents"),
        overlayGroups = c("Markers"),
        position = "topright"
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~value,
        title = "Avg %",
        opacity = 0.5
      )
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

    p <- p + scale_color_manual(values = color_map_all)

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

    p <- p + scale_color_manual(values = collab_color_map)

    ggplotly(p, tooltip = "text") %>%
      plotly::partial_bundle() %>%
      plotly::toWebGL()
  })
  # %>% bindCache(active_tab(), input$collabChemicalSelector, filtered_data())
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # -------------------------------
  # Sort the flags by country name
  output$collabFlagButtons <- renderUI({
    req(active_tab() == "Collaboration Trends 🤝")

    # Collect all unique iso2c-country pairs, then sort by country name
    sorted_countries <- df_global_ind %>%
      distinct(iso2c, country) %>%
      arrange(country)

    # Create flag buttons for every sorted country
    flags <- lapply(seq_len(nrow(sorted_countries)), function(i) {
      iso <- sorted_countries$iso2c[i]
      country_name <- sorted_countries$country[i]
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

    div(class = "d-flex flex-wrap gap-2", flags)
  })

  # -------------------------------
  # Observe flag clicks and update the partners plot (using Plotly)
  observeEvent(input$selectedCountry, {
    sel_iso <- input$selectedCountry

    # Filter the collaboration data from the complete collab dataset (do not apply any region or main trends filters)
    relevant_data <- df_global_collab[grepl(sel_iso, iso2c)]

    if (nrow(relevant_data) == 0) {
      showNotification(paste("No collaboration data found for country code:", sel_iso), type = "warning")
      return()
    }

    # For each record, determine the collaborating partner.
    # This assumes iso2c is formatted as "XX-YY" (a two-country code)
    relevant_data <- relevant_data %>%
      mutate(
        partner = sapply(strsplit(country, "-"), function(x) {
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
      width = "100%",
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Element Figures
  selected_element <- reactiveVal(NULL)

  # Create periodic table data
  periodic_data <- reactive({
    df_figures %>%
      group_by(symbol) %>%
      summarise(
        display_row = first(display_row),
        display_column = first(display_column),
        atomic_number = first(atomic_number),
        element = first(element),
        group = first(group),
        period = first(period),
        discoverer = first(discoverer),
        year_of_discovery = first(year_of_discovery),
        type = first(type),
        .groups = "drop"
      ) %>%
      mutate(
        present = symbol %in% unique(df_figures$symbol)
      )
  })
  # %>% bindCache("periodic_data")

  # Handle periodic table clicks
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (!is.null(click)) {
      clicked_element <- periodic_data() %>%
        filter(
          display_column == round(click$x),
          display_row == round(click$y)
        ) %>%
        slice(1)

      if (nrow(clicked_element) > 0) {
        selected_element(clicked_element$symbol)
      }
    }
  })

  # Element information display
  output$elementInfo <- renderUI({
    if (!is.null(selected_element())) {
      element_data <- df_figures %>%
        filter(symbol == selected_element()) %>%
        slice(1) # Get first occurrence for static properties

      tagList( # nolint
        h3(class = "bold-text", element_data$element),
        span(
          class = "chem-badge",
          style = "background:#4d908e; color: white;",
          paste0(element_data$type)
        ),
        hr(),
        h4("Basic Properties"),
        p(span(class = "bold-text", "Symbol: "), element_data$symbol),
        p(span(class = "bold-text", "Atomic Number: "), element_data$atomic_number),
        p(span(class = "bold-text", "Weight: "), round(element_data$atomic_weight, 4)),
        h4("Physical Properties"),
        p(
          span(class = "bold-text", "Density: "),
          ifelse(is.na(element_data$density), "N/A",
            format(element_data$density, scientific = FALSE)
          )
        ),
        p(
          span(class = "bold-text", "Melting Point: "),
          paste(round(element_data$melting_point_k, 1), "K")
        ),
        h4("Chemical Properties"),
        p(
          span(class = "bold-text", "Electronegativity: "),
          round(element_data$electronegativity, 2)
        ),
        p(
          span(class = "bold-text", "Ionization Potential: "),
          round(element_data$first_ionization_potential, 2)
        ),
        hr(),
        h3("Discovered by:"),
        p(element_data$discoverer),
        h3("Year of Discovery:"),
        p(element_data$year_of_discovery)
      )
    } else {
      h4("Click an element in the periodic table to view details")
    }
  })
  # %>% bindCache(selected_element())

  # Composition timeline plot
  output$compositionPlot <- renderPlot({
    # Prepare data for plotting
    plot_data <- df_figures %>%
      group_by(year, chemical) %>%
      summarise(
        total_percentage = mean(percentage, na.rm = TRUE),
        symbol = first(symbol),
        .groups = "drop"
      )

    # Base plot with lines
    base_plot <- ggplot(plot_data, aes(x = year, y = total_percentage)) +
      geom_line(aes(color = chemical), linewidth = 1.2) +
      geom_point(aes(color = symbol), size = 3) +
      scale_color_manual(values = c(
        "Organometallic" = "#4d908e",
        "Rare-Earths" = "#f94144"
      )) +
      labs(
        title = "Elemental Composition Over Time",
        caption = "Line represents total percentage of chemical space",
        x = "Year", y = "Percentage of Chemical Space"
      ) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1, scale = 1))

    # Add element-specific points if an element is selected
    if (!is.null(selected_element())) {
      element_data <- df_figures %>%
        filter(symbol == selected_element())

      base_plot <- base_plot +
        facet_wrap(~chemical) +
        geom_point(
          data = element_data,
          aes(y = percentage),
          color = "#f8961e", size = 4, shape = 18
        ) +
        geom_text(
          data = element_data,
          aes(
            y = percentage,
            label = paste0(symbol),
            vjust = -1, hjust = 0.5, color = "#f8961e", size = 2
          )
        )
    }

    base_plot
  })
  # %>% bindCache(selected_element())

  # Periodic table plot
  output$periodicTablePlot <- renderPlot({
    plot_data <- periodic_data()
    highlight_element <- selected_element()

    p <- ggplot(plot_data, aes(x = display_column, y = display_row)) +
      # Tiles with no data (blank spaces)
      geom_tile(
        data = subset(plot_data, !present),
        fill = "#f8f9fa", color = "white",
        width = 0.95, height = 0.95
      ) +
      # Tiles coloured by "type" for cells with data
      geom_tile(
        data = subset(plot_data, present),
        aes(fill = type),
        color = "white", width = 0.95, height = 0.95
      )

    # Highlight the clicked element with orange color
    if (!is.null(highlight_element)) {
      p <- p + geom_tile(
        data = subset(plot_data, symbol == highlight_element),
        fill = "#f8961e", color = "white",
        width = 0.95, height = 0.95
      )
    }

    p +
      geom_text(aes(label = symbol), size = 5, fontface = "bold") +
      scale_fill_brewer(palette = "Set2", na.value = "#f8f9fa") +
      scale_y_reverse() + # For proper periodic table orientation
      theme_void() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none"
      ) +
      labs(title = "Click elements to explore composition trends")
  })
}

shinyApp(ui, server)
