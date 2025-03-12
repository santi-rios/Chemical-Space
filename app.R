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

# Efficient data preparation using Arrow and dplyr
ds <- arrow::open_dataset("./data6/df.parquet", format = "parquet") %>%
  as_tidytable()

# Pre-filter at the Arrow level before collecting
ds_filtered <- ds %>%
  filter(!is.na(percentage)) %>%
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
    15:39
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
        actionButton("plotTop100Countries", "Top 100 Countries", class = "btn-success", style = "width: 100%;")
      )
    ),
    hr(),
    div(
      style = "margin-bottom: 18rem;",
      selectizeInput(
        "countries", "Select Countries 🎌",
        choices = NULL,
        multiple = TRUE,
        options = list(
          plugins = "remove_button",
          maxItems = 100,
          placeholder = "Select up to 100 countries"
        ),
        width = "100%"
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
      fluidRow(
        column(
          width = 12,
          value_box(
            title = uiOutput("summaryText"),
            value = uiOutput("flagButtons"),
            max_height = "130px",
            full_screen = TRUE,
            fill = TRUE
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
            uiOutput("mapPlot")
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
          value_box(
            title = uiOutput("collabSummaryText"),
            value = uiOutput("collabFlagButtons"),
            max_height = "130px",
            full_screen = TRUE,
            fill = TRUE
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Trends📈",
            withSpinner(plotlyOutput("collabTrendPlot", height = 800), color = "#024173"
          ),
          nav_panel(
            "Connections🌐",
            withSpinner(plotOutput("collabMap", height = 800), color = "#024173"
          ),
          nav_panel(
            "Substance Types🧪",
            fluidRow(
              column(
                width = 12,
                selectInput(
                  "collabChemicalSelector",
                  "Select Chemical Type",
                  choices = c("Organic", "Organometallic", "Rare-Earths"),
                  selected = "Organic",
                  width = "30%"
                )
              )
            ),
            withSpinner(plotlyOutput("collabSubstancePlot", width = "100%"), color = "#024173")
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
          "article_source", "Select Article Source",
          choices = unique(figure_article$source),
          selected = unique(figure_article$source)[1],
          width = "40%"
        )
      )
    ),

    withSpinner(plotlyOutput("articlePlot", height = 600, width = "100%"), color = "#024173"),
    card_footer(
            "Countrywise expansion of the chemical space.",
          popover(
            a("Learn more", href = "#"),
            markdown(
              "This plots show the chemichap space growth, enfatising China's rise in the chemical space (CS) and the decline of US influence."
            )
          )
          ),
    br(),
    br(),
    hr(),
    tags$h3("Original country and collaborations contributions to the chemical space from the original article"),
    tags$p("Here we show, by analysing the chemical space between 1996 and 2022, that the chemical space expansion has been dominated by China ever since 2013. Chinese dominance is mainly the product of the country’s own efforts, rather than the result of international collaboration. Alternatively, the US share of the chemical space is more dependent on international collaboration, which mainly occurs with China."),
    
    fluidRow(
      column(
      width = 6,
      tags$img(
        src = "trends_country.gif",
        width = "100%",
        style = "display:block; margin:0 auto;"
      ),
      p("Country contribution to chemical space", style = "text-align:center;")
      ),
      column(
      width = 6,
      tags$img(
        src = "trends_collab.gif",
        width = "100%",
        style = "display:block; margin:0 auto;"
      ),
      p("Collaborations contributions to chemical space", style = "text-align:center;")
      )
    )
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
)
)

server <- function(input, output, session) {
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
    updateSelectizeInput(session, "countries", choices = valid_countries, 
                        selected = top_countries$country, server = TRUE)
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

  # National Trends Plot
  output$trendPlot <- renderPlotly({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)
    data <- filtered_data() %>% filter(chemical == "All")
    
    p <- ggplot(data, aes(x = year, y = percentage, color = country, text = paste(
      "<b>Country:</b> ", country,
      "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01),
      "<br><b>Year:</b> ", year
    ))) +
      geom_line() +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = color_map_all) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(title = "National Contributions to Chemical Space", y = "% of New Substances")
    
    ggplotly(p, tooltip = "text") %>% 
      plotly::toWebGL()
  }) 
  # %>% bindCache(active_tab(), filtered_data())

# Collaboration Trends Plot
  output$collabTrendPlot <- renderPlotly({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)
    data <- filtered_data() %>% filter(chemical == "All")
    
    p <- ggplot(data, aes(x = year, y = percentage, color = country, text = paste(
      "<b>Collaboration:</b> ", country,
      "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01),
      "<br><b>Year:</b> ", year
    ))) +
      geom_line() +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = color_map_all) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(title = "Collaborative Contributions to Chemical Space", y = "% of New Substances")
    
    ggplotly(p, tooltip = "text") %>% 
      plotly::toWebGL()
  }) 
  # %>% bindCache(active_tab(), filtered_data())

  collab_data <- reactive({
    req(filtered_data(), active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)
    dt <- as.data.table(filtered_data())
    dt[, Value := percentage]
    dt[, Year := year]
    dt
  })

# Connection Map for Collaborations
  output$collabMap <- renderPlot({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)
    
    # Check cache first
    cache_key <- digest::digest(list(input$years, input$countries))
    if (!is.null(connection_cache[[cache_key]])) {
      return(connection_cache[[cache_key]])
    }
    
    # Process collaboration pairs
    collab_pairs <- filtered_data() %>%
      mutate(country_pairs = strsplit(iso2c, "-")) %>%
      tidyr::unnest(country_pairs) %>%
      left_join(
        df_global_ind %>% distinct(iso2c, lat, lng),
        by = c("country_pairs" = "iso2c")
      ) %>%
      group_by(iso2c) %>%
      mutate(
        pair_type = ifelse(row_number() == 1, "source", "target")
      ) %>%
      pivot_wider(names_from = pair_type, values_from = c(country_pairs, lat, lng)) %>%
      filter(!is.na(lat_source) & !is.na(lat_target))
    
    # Create connection lines
    connections <- collab_pairs %>%
      group_by(source = country_pairs_source, target = country_pairs_target) %>%
      summarise(
        total = sum(percentage, na.rm = TRUE),
        source_lon = first(lng_source),
        source_lat = first(lat_source),
        target_lon = first(lng_target),
        target_lat = first(lat_target),
        .groups = "drop"
      )
    
    # Generate great circle routes
    routes <- purrr::map2_df(
      connections$source_lon, connections$source_lat,
      connections$target_lon, connections$target_lat,
      ~{
        gcIntermediate(c(.x, .y), c(.x2, .y2), n = 50, addStartEnd = TRUE, sp = TRUE) %>%
          sf::st_as_sf() %>%
          mutate(total = .$total)
      }
    )
    
    # Create plot
    p <- ggplot() +
      geom_sf(data = routes, aes(alpha = total, color = total), 
              linewidth = 0.7, show.legend = FALSE) +
      scale_alpha_continuous(range = c(0.1, 0.7)) +
      scale_color_gradient(low = "#FFA500", high = "#FF4500") +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "#02152b", colour = NA),
        plot.background = element_rect(fill = "#02152b", colour = NA)
      )
    
    # Cache and return
    connection_cache[[cache_key]] <- p
    p
  }) 
  # %>% bindCache(active_tab(), filtered_data())

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Map UI

  # Highchart map - Individual Countries
# National Map
  output$geoPlot <- renderHighchart({
    req(active_tab() == "National Trends 📈", nrow(filtered_data()) > 0)
    
    map_data <- filtered_data() %>%
      group_by(iso3c) %>%
      summarise(value = mean(percentage, na.rm = TRUE))
    
    hcmap("custom/world-robinson-lowres", data = map_data) %>%
      hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10))) %>%
      hc_tooltip(pointFormat = "{point.name}: {point.value:.2f}%")
  }) 
  # %>% bindCache(active_tab(), filtered_data())
  # Collaboration Map
  output$collabMap <- renderHighchart({
    req(active_tab() == "Collaboration Trends 🤝", nrow(filtered_data()) > 0)
    map_data_by_pair <- collab_data()[, .(
      iso3c_combo = iso3c,
      Value, Year
    )]

    map_data_by_pair <- map_data_by_pair[, .(
      value      = mean(Value, na.rm = TRUE),
      best_year  = Year[which.max(Value)],
      worst_year = Year[which.min(Value)]
    ), by = iso3c_combo]

    # Expand each iso3c_combo into individual iso codes
    map_expanded <- map_data_by_pair[, .(
      splitted_iso     = unlist(strsplit(iso3c_combo, "-")),
      combo            = rep(iso3c_combo, sapply(strsplit(iso3c_combo, "-"), length)),
      combo_value      = rep(value, sapply(strsplit(iso3c_combo, "-"), length)),
      combo_best_year  = rep(best_year, sapply(strsplit(iso3c_combo, "-"), length)),
      combo_worst_year = rep(worst_year, sapply(strsplit(iso3c_combo, "-"), length))
    )]

    map_data <- map_expanded[, .(
      value = mean(combo_value, na.rm = TRUE),
      best_year = combo_best_year[which.max(combo_value)],
      worst_year = combo_worst_year[which.min(combo_value)],
      collab_list = paste0(
        unique(paste0(combo, " (best year: ", combo_best_year, ")")),
        collapse = "; "
      )
    ), by = splitted_iso]

    setnames(map_data, "splitted_iso", "iso3c")
    max_val <- max(map_data$value, na.rm = TRUE)

    hcmap("custom/world-robinson-lowres",
      data   = map_data,
      joinBy = c("iso-a3", "iso3c"),
      value  = "value",
      name   = "Collaboration"
    ) %>%
      hc_colorAxis(
        minColor = "#04131f",
        maxColor = "#c5051b",
        labels   = list(format = "{value:.2f}%"),
        title    = list(text = "Collaboration (%)", style = list(color = "black"))
      ) %>%
      hc_tooltip(pointFormat = paste0(
        "{point.name}: {point.value:.2f}%,<br>",
        "Pairs: {point.collab_list}<br>",
        "Best Year: {point.best_year},<br>",
        "Worst Year: {point.worst_year}"
      )) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_subtitle(
        text  = paste0("Top collaboration = ", scales::percent(max_val, accuracy = 0.01, scale = 1)),
        style = list(color = "black")
      ) %>%
      hc_title(
        text  = "Percentage of new substances with participation of country pairs",
        style = list(color = "black")
      )
  })

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
      setView(lng = 0, lat = 30, zoom = 1)
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
    pal <- colorNumeric("viridis", domain = data$value)
    
    leafletProxy("geoPlot2", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~scales::rescale(value, c(5, 30)),
        color = ~pal(value),
        popup = ~glue("{country}<br>Avg: {round(value, 2)}%")
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
    # Collaboration version of substance plot
  }) 
  # %>% bindCache(active_tab(), input$collabChemicalSelector, filtered_data())

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # Flag Buttons
  output$flagButtons <- renderUI({
    req(filtered_data())

    # Get ordered ISO codes using data.table
    iso_data <- if (input$data_mode == "Collaborations") {
      filtered_data()[iso2c %in% input$countries][
        collab_expansion,
        on = .(iso2c),
        allow.cartesian = TRUE
      ][, .(totVal = sum(percentage)), by = .(isoSplit = isoSplit)]
    } else {
      filtered_data()[, .(totVal = sum(percentage)), by = .(isoSplit = iso2c)]
    }

    ordered_iso <- iso_data[
      order(-totVal),
      head(isoSplit, 100) # Limit to top 100
    ]

    # Get precomputed flags and add click handlers
    flags <- precomputed_flags[ordered_iso]
    flags <- lapply(flags, function(flag) {
      iso <- flag$attribs$`data-iso`
      flag$attribs$onclick <- sprintf(
        "Shiny.setInputValue('selectedCountry', '%s', {priority: 'event'})",
        iso
      )
      flag
    })

    div(flags)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Modal
# Modal when flag is clicked
observeEvent(input$selectedCountry, {
  sel_iso <- input$selectedCountry

  # First try to get data from filtered_data() - this respects current filters
  relevant_data <- filtered_data()[grepl(sel_iso, iso2c)]
  
  # If no data, try broader df() scope which respects the data mode and region filters
  if (nrow(relevant_data) == 0) {
    relevant_data <- df()[grepl(sel_iso, iso2c)]
  }
  
  if (nrow(relevant_data) == 0) {
    showNotification(paste("No data found for country code:", sel_iso), type = "warning")
    return()
  }
  
  # If in Individual Countries mode, switch to Collaborations
  if (input$data_mode == "Individual Countries") {
    # Switch to 'Collaborations' mode
    updateNavbarPage(session, "selected", selected = "Collaboration Trends 🤝")
    
    # Find all countries that collaborate with this country
    collab_partners <- df_global %>%
      filter(is_collab == TRUE, grepl(sel_iso, iso2c)) %>% 
      pull(country) %>% 
      unique()
    
    # Update the selected countries to show these collaborations
    updateSelectizeInput(session, "countries", selected = collab_partners)
  }
  
  # Extract collaboration info using vectorized operations instead of lapply
  collab_codes <- unique(unlist(strsplit(relevant_data$iso2c, "-")))
  collab_codes <- collab_codes[collab_codes != sel_iso]
  
  # Get country names for the modal title
  selected_country_name <- country_metadata[country_metadata$iso2c == sel_iso, "country"]

  # Create flag icons using precomputed flags (more efficient)
  flag_icons <- NULL
  if (length(collab_codes) > 0) {
    valid_codes <- collab_codes[collab_codes %in% names(precomputed_flags)]
    flags <- precomputed_flags[valid_codes]
    
    if (length(flags) > 0) {
      # Remove click handlers from these flags since they're just for display
      flags <- lapply(flags, function(flag) {
        flag$attribs$onclick <- NULL
        flag
      })
      flag_icons <- div(class = "d-flex flex-wrap gap-2 mb-3", flags)
    }
  }

  # Create a summary table of collaboration data
  summary_data <- relevant_data %>%
    group_by(year) %>%
    summarise(
      avg_percentage = mean(percentage, na.rm = TRUE),
      total_percentage = sum(percentage, na.rm = TRUE),
      collaborations = n_distinct(iso2c),
      .groups = "drop"
    )
  
  # Create a small trend plot to show in the modal
  trend_plot <- renderPlot({
    ggplot(relevant_data, aes(x = year, y = percentage, color = iso2c, group = iso2c)) +
      geom_line(linewidth = 1) + 
      geom_point() +
      labs(
        title = paste("Collaboration trends for", selected_country_name),
        x = "Year",
        y = "Percentage",
        color = "Collaboration"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Assemble the modal content
  content <- tagList(
    h4("Collaboration Partners:"),
    if (!is.null(flag_icons)) {
      flag_icons
    } else {
      p("No collaborations found in the current filter selection.")
    },
    
    h4("Trend Analysis:"),
    plotOutput("modalPlot", height = "300px"),
    
    h4("Summary Statistics:"),
    renderTable({
      summary_data
    }),
    
    p("NOTE: For a full view of all collaborations, use the 'Select Countries' filter to explore more countries.")
  )

  # Show the modal with proper title and the trend plot
  showModal(modalDialog(
    title = paste("Country Details:", selected_country_name, "(", sel_iso, ")"),
    content,
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  
  # Render the trend plot inside the modal after it's created
  output$modalPlot <- trend_plot
})

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Article Figures
  output$articlePlot <- renderPlotly({
    req(input$article_source)

    # Set y-axis title based on article_source
    y_title <- switch(input$article_source,
      "CS Growth" = "Percentage of new substances",
      "China-US collaboration" = "Percentage of national contribution",
      "Growth rate of GDP" = "GDP per capira growth (annual %)",
      "Number of Researchers" = "Researchs",
      "Expansion of the CS" = "Number of new substances",
      "Value" # default title if none match
    )

    article_data <- subset(figure_article, source == input$article_source)

    p <- plot_ly(
      article_data,
      x = ~year,
      y = ~percentage,
      type = "scatter",
      mode = "markers",
      color = ~country,
      colors = "Set1",
      alpha = 0.9,
      size = ~percentage,
      marker = list(sizemode = "diameter"),
      text = ~ paste("Country: ", country, "<br>Year: ", year, "<br>Value: ", percentage),
      frame = ~year
    ) %>%
      layout(
        title = paste("Article Figures - Source:", input$article_source),
        xaxis = list(title = "Year"),
        yaxis = list(title = y_title),
        plot_bgcolor = "rgb(199, 204, 204)"
      ) %>%
      animation_opts(
        frame = 300,
        transition = 0,
        redraw = FALSE
      )

    p %>%
      plotly::partial_bundle() %>%
      plotly::toWebGL()
  }) 
  # %>% bindCache(input$article_source)

  observe({
    if (input$article_source %in% c("China-US collaboration")) {
      showNotification("Explore the Original Paper Figures. More information in this link: https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6", type = "warning")
    }
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
