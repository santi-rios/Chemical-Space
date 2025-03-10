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
library(ggplot2); theme_set(theme_light())
library(data.table)
library(shinycssloaders)

# Efficient data preparation using Arrow and dplyr
ds <- arrow::open_dataset("./data6/df.parquet", format = "parquet") %>%
  as_tidytable()

# Pre-filter at the Arrow level before collecting
ds_filtered <- ds %>% filter(!is.na(percentage)) 

# Full dataset with only needed columns - collect once
df_global <- ds_filtered %>% dplyr::collect()

# Use data.table directly - faster than tidytable for these operations
setDT(df_global)

# Create indices on frequently filtered columns for faster subsetting
data.table::setindex(df_global, is_collab, country, year, iso2c, iso3c, region, chemical)

# Create views instead of copies - much more memory efficient
df_global_ind <- df_global[is_collab == FALSE]
df_global_collab <- df_global[is_collab == TRUE]

# Articles: only load columns needed
figure_article <- ds %>%
  select(-percentage, -country, -year) %>%
  filter(!is.na(percentage_x)) %>%
  rename(
    percentage = percentage_x,
    country = country_x,
    year = year_x
  ) %>%
  dplyr::collect()

# Element figures: only load columns needed
df_figures <- ds %>%
  select(-percentage, -year, -chemical) %>%
  filter(!is.na(percentage_y)) %>%
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

# Initialize the cache for collaborations with splitted iso3c renamed,
# ensuring that the original iso3c value is captured separately.
map_data_cache$collab_expanded <- df_global_collab %>%
  mutate(orig_iso = iso3c,
         iso3c = strsplit(iso3c, "-")) %>%
  tidyr::unnest(cols = iso3c) %>%
  mutate(combo = orig_iso,
         value = percentage,
         year = year) %>%
  select(-orig_iso)


ui <- page_navbar(
  selected = "National Trends 📈",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  header = NULL,
  navbar_options = navbar_options(collapsible = TRUE, underline = TRUE),
  sidebar = sidebar(
    title = "Country and Region Filters 🌍",
    width = "14rem",
    tooltip(
      fontawesome::fa("info-circle", a11y = "sem", title = "Warnings"),
      "Filters only apply to the 'National Trends' section. \nUse the 'Select Countries' filter to explore more."
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
        actionButton("plotTopCountries", "Top 20 Countries", class = "btn-danger", style = "width: 100%;")
      )
    ),
    hr(),
div(
      style = "margin-bottom: 18rem;", # Use CSS to create space underneath
      selectizeInput(
        "countries", "Select Countries 🎌",
        choices = NULL,
        multiple = TRUE,
        options = list(plugins = "remove_button", maxItems = 30, placeholder = "Please select up to 100 countries"),
        width = "100%"
  )
)
  ),

  # ------------------------------
  # 1) NATIONAL TRENDS NAV PANEL
  # ------------------------------
  nav_panel(
    "National Trends 📈",

    # Wrap everything in a fluidPage so you can arrange the new controls at the top:
    fluidPage(
      fluidRow(
        column(
          width = 12,

          # Data Mode
          div(
            style = "display:inline-block; margin-right:20px;",
            radioButtons(
              "data_mode", "Data Mode 📊",
              choices = c("Individual Countries", "Collaborations"),
              selected = "Individual Countries",
              inline = TRUE
            )
          ),

          # Region Filter
          div(
            style = "display:inline-block;",
            conditionalPanel(
              condition = "input.data_mode == 'Individual Countries'",
              selectizeInput(
                "region",
                "🗾Region Filter🍁",
                choices = "All",
                multiple = FALSE,
                options = list(plugins = "remove_button")
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          # Summary box with the flags
          value_box(
            title = uiOutput("summaryText"),
            value = uiOutput("flagButtons"),
            max_height = "100px",
            full_screen = TRUE,
            fill = TRUE
          )
        )
      ),

      # Existing card with sub-tabs
      card(
        navset_card_tab(
          nav_panel(
            "Trends📈",
            withSpinner(plotlyOutput("trendPlot", width = "100%"), color = "#024173")
          ),
          nav_panel("Map📌", uiOutput("mapPlot")),
          nav_panel(
            "Cartogram🗺️",
            tooltip(
              fontawesome::fa("info-circle", a11y = "sem", title = "Warning"),
              "Cartogram only available for Individual Countries (Select in Main panel). \nClick 'Reload Map' to see the markers.\nClick on the markers for more details.\nData depicts the average contribution of the years selected."
            ),
            conditionalPanel(
              condition = "input.data_mode == 'Individual Countries'",
              actionButton("map2_reload", "Reload Map. Must be clicked every time you reload data or filters.", class = "btn-danger", style = "width: 100%;"),
              leafletOutput("geoPlot2", height = 600)
            )
          ),
          nav_panel(
            "Substance Types🧪",
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
    withSpinner(plotlyOutput("articlePlot", height = 600, width = "100%"), color = "#024173")
  ),

  # ------------------------------
  # 4) ELEMENT FIGURES
  # ------------------------------
  nav_panel(
    "Element Figures 🧪",
    fluidPage(
      fluidRow(
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
  # Dynamically update Region choices and show initial top countries
  observe({
    req(df_global_ind, df_global_collab)

    # Update region selectize input based on current data mode
    region_choices <- if (input$data_mode == "Individual Countries") {
      unique(df_global_ind$region[!is.na(df_global_ind$region)])
      unique(df_global_ind$region[!is.na(df_global_ind$region)])
    } else {
      unique(df_global_collab$region[!is.na(df_global_collab$region)])
    }
    updateSelectizeInput(session, "region", choices = c("All", sort(region_choices)), selected = "All")

    # Set initial top countries (using pre-calculated values)
    updateSelectizeInput(session, "countries", selected = initial_top_countries, choices = all_ctry, server = TRUE)
  })

  # Build the base data once, filtering by collab setting and region.
  df <- reactive({
    if (input$data_mode == "Individual Countries") {
      res <- df_global_ind
      if (!is.null(input$region) && !("All" %in% input$region)) {
        res <- res[region %in% input$region]
      }
    } else {
      res <- df_global_collab
    }
    res
  })
  # bindCache(input$data_mode, input$region)

  # Update available country choices based on df.
  observe({
    req(df())
    valid_countries <- sort(unique(df()$country))
    top_countries <- df() %>% # Use df() here to recalculate top countries based on current filters
      group_by(country) %>%
      summarise(val = sum(percentage, na.rm = TRUE)) %>%
      arrange(desc(val)) %>%
      head(10)
    updateSelectizeInput(session, "countries", choices = valid_countries, selected = top_countries$country, server = TRUE)
  })

  # Observe event for "Top 20 Countries" button
  observeEvent(input$plotTopCountries, {
    top_20_countries_df <- df() %>%
      group_by(country) %>%
      summarise(val = sum(percentage, na.rm = TRUE)) %>%
      arrange(desc(val)) %>%
      head(20)
    top_20_countries <- top_20_countries_df$country
    updateSelectizeInput(session, "countries", selected = top_20_countries)
  })

  # Observe event for "Deselect All" button
  observeEvent(input$deselectAll, {
    updateSelectizeInput(session, "countries", selected = character(0))
  })

  # Filtered data reactive: by years and selected countries.
  filtered_data <- reactive({
    req(input$countries, input$years)
    df() %>% # use df() here to apply current data filtering
      filter(
        year >= input$years[1] & year <= input$years[2] &
          country %in% input$countries
      )
  })
  # bindCache(input$years, input$countries, input$data_mode, input$region) %>%
  # debounce(300)

  # Trend plot
  output$trendPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
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
        size = 3, check_overlap = TRUE, show.legend = FALSE
      ) +
      scale_color_manual(values = color_map_all) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size= 8, face="bold"),                                 
        legend.title = element_blank(),
        axis.title.x = element_blank()
        )
    
    if (input$data_mode == "Collaborations") {
      p <- p + labs(
        title = "Percentage of new compunds by Collaborations",
        y = "% of new substances",
        colour = ""
        )
    } else {
      p <- p + labs(
        title = "Percentage of new compunds reported by each country in journals",
        y = "% of new substances",
        colour = ""
        )
    }

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
  # bindCache(filtered_data)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Map UI
  output$mapPlot <- renderUI({
    if (input$data_mode == "Individual Countries") {
      withSpinner(highchartOutput("geoPlot", height = 600), color = "#024173")
    } else {
      withSpinner(highchartOutput("collabMap", height = 600), color = "#024173")
    }
  })

  # Highchart map - Individual Countries
  output$geoPlot <- renderHighchart({
    req(nrow(filtered_data()) > 0, input$data_mode == "Individual Countries")
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
  }) %>% 
  bindCache(list(
    input$data_mode, 
    paste(sort(unique(filtered_data()$iso3c)), collapse=","),
    input$years[1], 
    input$years[2]
  ))
  # Highchart map - Collaborations
# Replace the collab_data reactive with a more optimized version
collab_data <- reactive({
  req(filtered_data(), input$data_mode == "Collaborations")
    dt <- as.data.table(filtered_data())
    dt[, Value := percentage]
    dt[, Year := year]
    dt
  })

  output$collabMap <- renderHighchart({
    req(collab_data())
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

  # Automatically process data whenever filters change
  carto_data <- reactive({
    req(nrow(filtered_data()) > 0, input$data_mode == "Individual Countries")
    filtered_data() %>%
      group_by(country, lat, lng) %>%
      summarise(value = mean(percentage, na.rm = TRUE), .groups = "drop")
  })

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

# Update markers using observeEvent to control when it happens
observeEvent(input$map2_reload, {
  req(carto_data())
  data <- carto_data()
  pal <- colorNumeric("Reds", domain = data$value)

  leafletProxy("geoPlot2", data = data) %>%
    clearMarkers() %>%
    clearControls() %>%
    addCircleMarkers(
      lng = ~lng, lat = ~lat,
      radius = ~ scales::rescale(value, c(5, 30)),
      color = ~ pal(value),
      fillOpacity = 0.7,
      group = "Markers",
      popup = ~ glue("<b>{country}</b><br>Average: {round(value, 2)}%")
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~value,
      title = "Avg %",
      opacity = 0.5
    )
}, ignoreInit = TRUE)
  observe({
    if (input$data_mode == "Individual Countries") {
      showNotification("Country and Region Filters refresh Plots automatically except for Cartogram which requires manual reload.", type = "warning")
    }
  })

  observe({
    if (input$data_mode == "Collaborations") {
      showNotification("Cartogram is only available for Individual Countries data", type = "warning")
    }
  })
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Substance Types
  output$substancePlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
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

    p <- p
    # + scale_color_manual(values = color_map_all)

    ggplotly(p, tooltip = "text") %>%
      plotly::partial_bundle() %>%
      plotly::toWebGL()
  }) %>% bindCache(input$chemicalSelector, input$countries, input$years)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Value boxes
  output$summaryText <- renderUI({
    data_subset <- filtered_data()
    if (nrow(data_subset) == 0) {
      return("No data for this selection.")
    }
    HTML(glue(
      "Click on flag to see collaboration details for a specific country."
    ))
  })

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
  observeEvent(input$selectedCountry, {
    req(df_global)
    sel_iso <- input$selectedCountry

    if (input$data_mode == "Individual Countries") {
      # Switch to 'Collaborations' automatically and update countries accordingly:
      updateRadioButtons(session, "data_mode", selected = "Collaborations")

      collab_rows <- df_global %>%
        filter(is_collab == TRUE, grepl(sel_iso, iso2c))
      updateSelectizeInput(session, "countries", selected = unique(collab_rows$country))
    }

    # For collaborations, filter using df_global so that we get only rows
    # where iso2c contains the selected country code.
    flag_data <- df_global %>%
      filter(is_collab == TRUE, grepl(sel_iso, iso2c))
    if (nrow(flag_data) == 0) {
      return()
    }

    collab_countries <- unique(unlist(lapply(flag_data$iso2c, function(x) strsplit(x, "-")[[1]])))
    collab_countries <- collab_countries[collab_countries != sel_iso]

    # Create flags HTML for collaboration countries
    flag_icons <- NULL
    if (length(collab_countries) > 0) {
      flag_icons <- tags$div(
        lapply(collab_countries, function(iso) {
          tags$span(
            tags$img(
              src = paste0("https://flagcdn.com/16x12/", tolower(iso), ".png"),
              width = 16, height = 12,
              style = "margin-right: 4px;"
            )
          )
        })
      )
    }

    content <- tagList(
      if (!is.null(flag_icons)) {
        p("Collaborations from current country include:", flag_icons)
      } else {
        p("No other collaborations found.")
      },
      # Placeholder text for additional explanation
      p("NOTE: Default Collaboration plot shows the top overall collaborations. Use the 'Select Countries' filter to explore more.")
    )

    showModal(modalDialog(
      title = paste("Country Details:", sel_iso),
      content,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
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
      text = ~ paste("Country: ", country, "<br>Year: ", year, "<br>Percentage: ", percentage),
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
  }) %>% bindCache(input$article_source)

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
  }) %>% bindCache("periodic_data")

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
  }) %>% bindCache(selected_element())

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
  }) %>% bindCache(selected_element())

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
