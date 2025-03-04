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
library(data.table)
library(shinycssloaders)

# Convert Parquet to data.table-backed objects on the fly.
df_global <- as_tidytable(arrow::read_parquet("./data/df.parquet"))
figure_article <- as_tidytable(arrow::read_parquet("./data/data_article.parquet"))
df_figures <- as_tidytable(arrow::read_parquet("./data/supplements_data.parquet")) |>
  na.omit()



ui <- page_navbar(
  title = a(
    "Country contribution to the expansion of the chemical space",
    href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6",
    target = "_blank"
  ),
  selected = "🗺️National Trends",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  header = tagList(
    # Top info box moved to header
    div(
      style = "width: 100%; text-align: center; padding: 10px 0;",
      value_box(
        title = uiOutput("summaryText"),
        value = uiOutput("flagButtons"),
        max_height = "80px",
        fill = TRUE
      )
    ),
    # Data Mode relocated to header
    div(
      style = "display:inline-block;margin-right:20px;",
      radioButtons(
        "data_mode", "Data Mode",
        choices = c("Individual Countries", "Collaborations"),
        selected = "Individual Countries",
        inline = TRUE
      )
    ),
    # Region Filter relocated (shown conditionally)
    div(
      style = "display:inline-block;",
      conditionalPanel(
        condition = "input.data_mode == 'Individual Countries'",
        selectizeInput(
          "region",
          "Region Filter 🗾",
          choices = "All",
          multiple = FALSE,
          options = list(plugins = "remove_button")
        )
      )
    )
  ),
  navbar_options = navbar_options(collapsible = TRUE),
  sidebar = sidebar(
    title = "Country and Region Filters 🌍",
    width = "14rem", 
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
        actionButton("selectAll", "Select All", class = "btn-success", style = "width: 100%;")
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton("plotTopCountries", "Top 20 Countries", class = "btn-danger", style = "width: 100%;")
      )
    ),
    selectizeInput(
      "countries", "Select Countries 🎌",
      choices = NULL,
      multiple = TRUE,
      options = list(plugins = "remove_button"),
      width = "100%"
    ),
    hr()
  ),
  nav_panel(
    "🗺️National Trends",
    card(
      navset_card_tab(
        nav_panel(
          "Trends📈",
          withSpinner(plotlyOutput("trendPlot", width = "100%"), color = "#024173")
        ),
        nav_panel("Map📌", uiOutput("mapPlot")),
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
                width = "100%"
              )
            )
          ),
          withSpinner(plotlyOutput("substancePlot", width = "100%"), color = "#024173")
        )
      )
    )
  ),
  nav_panel(
    "Cartogram🗺️",
    conditionalPanel(
      condition = "input.data_mode == 'Individual Countries'",
      tooltip(
        fontawesome::fa("info-circle", a11y = "sem", title = "Warning"),
        "Cartogram only available for Individual Countries.\nClick 'Reload Map' to see the markers.\nClick on the markers for more details.\nData depicts the average contribution of the years selected."
      ),
      actionButton("map2_reload", "Reload Map", class = "btn-danger", style = "width: 100%;"),
      leafletOutput("geoPlot2", height = 600)
    )
  ),
  nav_panel(
    "Article Figures",
    fluidRow(
      column(
        width = 12,
        selectInput(
          "article_source", "Select Article Source",
          choices = unique(figure_article$source),
          selected = unique(figure_article$source)[1],
          width = "100%"
        )
      )
    ),
    withSpinner(plotlyOutput("articlePlot", height = 600, width = "100%"), color = "#024173")
  ),
  nav_panel(
    "Element Figures",
      tabsetPanel(
        tabPanel("elementInfo",
                 uiOutput("elementInfo")),
        tabPanel("Composition Timeline",
                 plotOutput("compositionPlot", height = "600px")),
        tabPanel("Periodic Table",
                 plotOutput("periodicTablePlot", 
                           click = "plot_click",
                           height = "600px"))
      )
  ),
    div(
    class = "container-fluid",
    style = "display: flex; justify-content: center; align-items: center; gap: 10px; padding: 10px 0;", # nolint: line_length_linter.
    tags$a(
      href = "https://tec.mx/es", target = "_blank",
      tags$img(src = "tec.png", style = "max-width: 40px; height: auto;")
    ),
    tags$a(
      href = "https://www.mpg.de/institutes", target = "_blank",
      tags$img(src = "logo.png", style = "max-width: 150px; height: auto;")
    ),
    tags$a(
      href = "https://www.uam.mx/", target = "_blank",
      tags$img(src = "uam.png", style = "max-width: 50px; height: auto;")
    ),
    tags$a(
      href = "https://www.uni-leipzig.de/en", target = "_blank",
      tags$img(src = "leipzig.png", style = "max-width: 100px; height: auto;")
    ),
    tags$a(
      href = "https://www.santafe.edu/", target = "_blank",
      tags$img(src = "santafe.png", style = "max-width: 60px; height: auto;")
    ),
    tags$a(
      href = "https://www.jvi.org/home.html", target = "_blank",
      tags$img(src = "vienna.png", style = "max-width: 60px; height: auto;")
    )
  )
)


server <- function(input, output, session) {
  # -- 1) Base data reactive using Arrow dataset --
df <- reactive({
  d <- if (input$data_mode == "Individual Countries") {
    df_global %>% filter(is_collab == FALSE)
  } else {
    df_global %>% filter(is_collab == TRUE)
  }
  
  if (input$data_mode == "Individual Countries" &&
      !is.null(input$region) &&
      !("All" %in% input$region)) {
    d <- d %>% filter(region %in% input$region)
  }
  
  d # Ensure data is in R data frame
})

  # -- 2) region choices
  observe({
    req(input$data_mode == "Individual Countries")

    # Get non-collab data to determine available regions
    non_collab_data <- df_global %>% filter(is_collab == FALSE)
    regions <- sort(unique(non_collab_data$region))

    # Start with the user’s current selection
    new_selection <- input$region

    # If nothing is selected, default to "All"
    if (is.null(new_selection) || length(new_selection) == 0) {
      new_selection <- "All"
    }

    # Retain only valid regions and "All"
    new_selection <- intersect(new_selection, c("All", regions))

    # If the intersection is empty, revert to "All"
    if (length(new_selection) == 0) {
      new_selection <- "All"
    }

    updateSelectizeInput(
      session, "region",
      choices = c("All", regions),
      selected = new_selection
    )
  })

  # -- 3) Dynamic update of countries
  observe({
    req(df())
    valid_countries <- df() %>%
      pull(country) %>%
      unique()

    # Keep intersection with previously selected (if any)
    current_selections <- isolate(input$countries)
    new_selection <- intersect(current_selections, valid_countries)

    # If user has nothing selected or we filtered out everything,
    # pick top 8 from the current (filtered) df
    if (length(new_selection) == 0) {
      top_countries <- df() %>%
        group_by(country) %>%
        summarise(total = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
        slice_max(total, n = 8) %>%
        pull(country)

      new_selection <- intersect(top_countries, valid_countries)
    }

    updateSelectizeInput(
      session, "countries",
      choices = valid_countries,
      selected = new_selection,
      server = TRUE
    )
  })

  # -- 3b) "Plot Top Countries" button
  observeEvent(input$plotTopCountries, {
    req(df())
    valid_countries <- df() %>%
      pull(country) %>%
      unique()

    # Compute top from the currently filtered data (i.e. matching region/data_mode)
    top_countries <- df() %>%
      group_by(country) %>%
      summarise(total = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
      slice_max(total, n = 20) %>%
      pull(country)

    new_selection <- intersect(top_countries, valid_countries)

    updateSelectizeInput(session, "countries", selected = new_selection)
  })

  # -- Filtering
  filtered_data_raw <- reactive({
    req(df())
    df() %>%
      filter(
        year >= input$years[1],
        year <= input$years[2],
        country %in% input$countries
      )
  })

  filtered_data_debounced <- filtered_data_raw %>% debounce(300)

  filtered_data <- reactive({
    filtered_data_raw()
  }) %>% bindCache(input$years, input$countries, input$data_mode, input$region)

  # "Deselect All" button
  observeEvent(input$deselectAll, {
    updateSelectizeInput(session, "countries", selected = character(0))
  })

  # "Select All" button
  observeEvent(input$selectAll, {
    req(df())
    valid_countries <- df() %>%
      pull(country) %>%
      unique()
    updateSelectizeInput(session, "countries", selected = valid_countries)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Trend Plot
  output$trendPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data() %>% filter(chemical == "All")
    p <- ggplot(data, aes(
      x = year, y = percentage, group = country,
      text = paste0(
        "<b>Country:</b> ", country,
        "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.001, scale = 1),
        "<br><b>Year:</b> ", year
      )
    )) +
      geom_line(aes(color = country), show.legend = TRUE) +
      geom_point(aes(size = percentage / 100, color = country), alpha = 0.4, show.legend = FALSE) +
      geom_text(
        data = data %>% filter(year == max(year)),
        aes(y = percentage, label = iso3c, color = country),
        hjust = -0.2, nudge_x = 0.3, nudge_y = 0.4,
        size = 4, check_overlap = TRUE, show.legend = FALSE
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    if (input$data_mode == "Individual Countries") {
      all_ctry <- unique(data$country)
      color_map <- setNames(viridisLite::viridis(length(all_ctry)), all_ctry)
      color_map[c(
        "China", "United States", "India", "Germany", "Japan",
        "United Kingdom", "France", "Russia", "Mexico", "Colombia",
        "Brazil", "Ecuador", "Argentina"
      )] <-
        c(
          "#c5051b", "#0a3161", "#ff671f", "#000000",
          "#995162", "#3b5091", "#000091", "#d51e9b",
          "#006341", "#fcd116", "#009b3a", "#ffdd00", "#74acdf"
        )
      p <- p +
        scale_color_manual(values = color_map) +
        labs(
          title = "Country participation in the growth of the Chemical Space",
          x = "Year",
          y = "% of new substances",
          colour = "", size = "Country"
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1))
    } else {
      all_ctry <- unique(data$country)
      color_map <- setNames(viridisLite::viridis(length(all_ctry)), all_ctry)
      p <- p +
        scale_color_manual(values = color_map) +
        labs(
          title = "International collaborations in the Chemical Space",
          x = "Year",
          y = "% of new substances",
          colour = "", size = "Collaboration"
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1, scale = 1, trim = TRUE))
    }

    ggplotly(p, tooltip = "text")
  })

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
        minColor = "#0c2a42",
        maxColor = "#c5051b",
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
      hc_title(text = "World Map", style = list(color = "Black")) %>%
      hc_subtitle(
        text  = "Values represent the average contribution of the years selected",
        style = list(color = "black")
      )
  })

  # Highchart map - Collaborations
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

# Process data
  map_data_by_pair <- collab_data()[, .(
    value      = mean(Value, na.rm = TRUE),
    best_year  = fcoalesce(na.omit(Year[which.max(Value)]), as.numeric(NA)),
    worst_year = fcoalesce(na.omit(Year[which.min(Value)]), as.numeric(NA))
  ), by = iso3c_combo]

    # Expand each iso3c_combo into individual iso codes
# Split into individual ISO codes
  map_expanded <- map_data_by_pair %>%
    separate_rows(iso3c_combo, sep = "-", convert = TRUE) %>%
    rename(splitted_iso = iso3c_combo)

# Early return if no expanded data
  if (nrow(map_expanded) == 0) {
    return(highchart() %>% hc_title(text = "No collaborations to display"))
  }

# Summarize by splitted_iso
  map_data <- map_expanded[, .(
    value       = mean(value, na.rm = TRUE),
    best_year   = fcoalesce(na.omit(best_year[which.max(value)]), as.numeric(NA)),
    worst_year  = fcoalesce(na.omit(worst_year[which.min(value)]), as.numeric(NA)),
    collab_list = paste(unique(iso3c_combo), collapse = "; ")
  ), by = splitted_iso] %>%
    rename(iso3c = splitted_iso)

    setnames(map_data, "splitted_iso", "iso3c")
    max_val <- max(map_data$value, na.rm = TRUE)

    hcmap("custom/world-robinson-lowres",
      data   = map_data,
      joinBy = c("iso-a3", "iso3c"),
      value  = "value",
      name   = "Collaboration"
    ) %>%
      hc_colorAxis(
        minColor = "#0c2a42",
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
  output$geoPlot2 <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group = "NASA") %>%
      addProviderTiles("CartoDB.Positron", group = "Continents") %>%
      setView(lng = 0, lat = 30, zoom = 2)
  })

  observeEvent(input$map2_reload, {
    req(nrow(filtered_data()) > 0, input$data_mode == "Individual Countries")
    data <- filtered_data() %>%
      group_by(country, lat, lng) %>%
      summarise(value = mean(percentage, na.rm = TRUE), .groups = "drop")

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
      clearControls() %>% # Clear existing controls
      addLayersControl(
        baseGroups = c("NASA", "Continents"),
        overlayGroups = c("Markers"),
        position = "topright"
      ) %>%
      addLegend(
        "bottomright", pal = pal,
        values = ~value,
        title = "Avg %",
        opacity = 0.5
      )
  })

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
      x = year, y = percentage, group = country, fill = region,
      text = paste0(
        "<b>Country:</b> ", country,
        "<br><b>Percentage:</b> ", scales::percent(percentage, accuracy = 0.01, scale = 1),
        "<br><b>Year:</b> ", year,
        "<br><b>Region:</b> ", region
      )
    )) +
      geom_line(aes(color = country), show.legend = TRUE) +
      geom_point(aes(size = percentage / 100, color = country),
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
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1))

    all_ctry <- unique(data$country)
    color_map <- setNames(viridisLite::viridis(length(all_ctry)), all_ctry)
    color_map[c(
      "China", "United States", "India", "Germany", "Japan",
      "United Kingdom", "France", "Russia", "Mexico", "Colombia",
      "Brazil", "Ecuador", "Argentina"
    )] <-
      c(
        "#c5051b", "#0a3161", "#ff671f", "#000000",
        "#995162", "#3b5091", "#000091", "#d51e9b",
        "#006341", "#fcd116", "#009b3a", "#ffdd00", "#74acdf"
      )
    p <- p + scale_color_manual(values = color_map)

    ggplotly(p, tooltip = "text")
  })

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
  output$flagButtons <- renderUI({
    req(filtered_data())
    # Summarize total by iso2c
    if (input$data_mode == "Collaborations") {
      # expand combos
iso_values <- filtered_data() %>%
  mutate(isoSplit = strsplit(iso2c, "-")) %>%
  unnest(isoSplit) %>%  # Use tidytable's unnest syntax
  group_by(isoSplit) %>%
  summarise(totVal = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totVal))
all_iso <- iso_values$isoSplit
    } else {
      iso_values <- filtered_data() %>%
        group_by(iso2c) %>%
        summarise(totVal = sum(percentage, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(totVal))
      all_iso <- iso_values$iso2c
    }

    if (length(all_iso) == 0) {
      return(NULL)
    }

    btns <- lapply(all_iso, function(iso) {
      tags$button(
        class = "btn btn-outline-secondary btn-sm",
        onclick = paste0("Shiny.setInputValue('selectedCountry', '", iso, "', {priority: 'event'})"),
        tags$img(
          src = paste0("https://flagcdn.com/16x12/", tolower(iso), ".png"),
          width = 16, height = 12
        ),
        " ", iso
      )
    })
    do.call(tags$div, btns)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Modal
  observeEvent(input$selectedCountry, {
    req(filtered_data())
    sel_iso <- input$selectedCountry

    if (input$data_mode == "Individual Countries") {
      # Switch to 'Collaborations' automatically
      updateRadioButtons(session, "data_mode", selected = "Collaborations")

      # Optionally set 'countries' to show collaborations for the clicked country:
      # (Assuming df_global contains is_collab=TRUE for pairs; adjust as needed)
      collab_rows <- df_global %>%
        filter(is_collab == TRUE, grepl(sel_iso, iso3c))
      updateSelectizeInput(session, "countries", selected = unique(collab_rows$country))
    }

    if (input$data_mode == "Collaborations") {
      flag_data <- filtered_data() %>% filter(grepl(sel_iso, iso3c))
    } else {
      flag_data <- filtered_data() %>% filter(iso2c == sel_iso)
    }
    if (nrow(flag_data) == 0) {
      return()
    }

    max_value <- max(flag_data$percentage, na.rm = TRUE)
    min_value <- min(flag_data$percentage, na.rm = TRUE)
    collab_countries <- unique(unlist(lapply(flag_data$iso3c, function(x) strsplit(x, "-")[[1]])))
    collab_countries <- collab_countries[collab_countries != sel_iso]

    content <- tagList(
      h4(paste("Information for", sel_iso)),
      p(paste("Max Percentage:", scales::percent(max_value, accuracy = 0.001, scale = 1))),
      p(paste("Min Percentage:", scales::percent(min_value, accuracy = 0.001, scale = 1))),
      if (length(collab_countries) > 0) {
        p(paste("Other Collaborations from current selection include:", paste(collab_countries, collapse = ", ")))
      } else {
        p("No other collaborations found.")
      }
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
    article_data <- subset(figure_article, source == input$article_source)

    p <- plot_ly(
      article_data,
      x = ~year,
      y = ~percentage,
      type = "scatter",
      mode = "markers",
      color = ~country,
      colors = "Dark2",
      alpha = 0.6,
      size = ~percentage,
      marker = list(sizemode = "diameter"),
      text = ~ paste("Country: ", country, "<br>Year: ", year, "<br>Percentage: ", percentage),
      frame = ~year
    ) %>%
      layout(
        title = paste("Article Figures - Source:", input$article_source),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value")
      ) %>%
      animation_opts(
        frame = 300,
        transition = 0,
        redraw = FALSE
      )

    p
  })

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
        .groups = 'drop'
      ) %>%
      mutate(
        present = symbol %in% unique(df_figures$symbol)
      )
  })
  
  # Handle periodic table clicks
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (!is.null(click)) {
      clicked_element <- periodic_data() %>%
        filter(display_column == round(click$x),
               display_row == round(click$y)) %>%
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
        slice(1)  # Get first occurrence for static properties
      
      tagList(
        h3(class = "bold-text", element_data$element),
        span(class = "chem-badge", 
             style = paste0("background:", 
                           ifelse(element_data$chemical == "Organometallic", 
                                 "#4d908e", "#f94144"), 
                           "; color: white;"),
             element_data$chemical),
        hr(),
        h4("Basic Properties"),
        p(span(class = "bold-text", "Symbol: "), element_data$symbol),
        p(span(class = "bold-text", "Atomic Number: "), element_data$atomic_number),
        p(span(class = "bold-text", "Weight: "), round(element_data$atomic_weight, 4)),
        
        h4("Physical Properties"),
        p(span(class = "bold-text", "Density: "), 
          ifelse(is.na(element_data$density), "N/A", 
                 format(element_data$density, scientific = FALSE))),
        p(span(class = "bold-text", "Melting Point: "), 
          paste(round(element_data$melting_point_k, 1), "K")),
        
        h4("Chemical Properties"),
        p(span(class = "bold-text", "Electronegativity: "), 
          round(element_data$electronegativity, 2)),
        p(span(class = "bold-text", "Ionization Potential: "), 
          round(element_data$first_ionization_potential, 2))
      )
    } else {
      h4("Click an element in the periodic table to view details")
    }
  })
  
  # Composition timeline plot
  output$compositionPlot <- renderPlot({
    # Prepare data for plotting
    plot_data <- df_figures %>%
      group_by(year, chemical) %>%
      summarise(total_percentage = mean(percentage, na.rm = TRUE),
    symbol = first(symbol),
                .groups = 'drop')
    
    # Base plot with lines
    base_plot <- ggplot(plot_data, aes(x = year, y = total_percentage)) +
      geom_line(aes(color = chemical), linewidth = 1.2) +
      geom_point(aes(color = symbol), size = 3) +
      scale_color_manual(values = c("Organometallic" = "#4d908e", 
                                  "Rare-Earths" = "#f94144")) +
      labs(title = "Elemental Composition Over Time",
           x = "Year", y = "Percentage Composition") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
    
    # Add element-specific points if an element is selected
    if (!is.null(selected_element())) {
      element_data <- df_figures %>%
        filter(symbol == selected_element())
      
      base_plot <- base_plot +
        facet_wrap(~ chemical) +
        geom_point(data = element_data,
                   aes(y = percentage),
                   color = "#f8961e", size = 4, shape = 18) +
        geom_text(data = element_data,
                  aes(y = percentage, 
                      label = paste0(symbol, ": ", round(percentage, 1)),
                  vjust = -1, hjust = 0.5, color = "#f8961e", size = 4)
        )
    }
    
    base_plot
  })
  
  # Periodic table plot
  output$periodicTablePlot <- renderPlot({
    plot_data <- periodic_data()
    highlight_element <- selected_element()
    
    ggplot(plot_data, aes(x = display_column, y = display_row)) +
      geom_tile(aes(fill = present), 
                color = "white", 
                width = 0.95, height = 0.95) +
      {
        if (!is.null(highlight_element))
          geom_tile(data = filter(plot_data, symbol == highlight_element),
                    fill = "#f8961e", color = "white", 
                    width = 0.95, height = 0.95)
      } +
      geom_text(aes(label = symbol), size = 6, fontface = "bold") +
      scale_fill_manual(values = c("TRUE" = "#43aa8b", "FALSE" = "#f8f9fa")) +
      scale_y_reverse() +  # For proper periodic table orientation
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            legend.position = "none") +
      labs(title = "Click elements to explore composition trends")
  })

}

shinyApp(ui, server)
