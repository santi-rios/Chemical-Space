library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(dplyr)
library(duckplyr)
library(ggplot2)
library(DT)
library(scales)
library(sf)
library(rnaturalearth)
library(shinycssloaders)

# Source helper functions
source("R/functions.R")

# Load data
data_objects <- load_country_data("./data/data.parquet")
ds <- data_objects$data
country_list <- data_objects$country_list
chemical_categories <- data_objects$chemical_categories
regions <- data_objects$regions

# UI Definition
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Title bar with info button
  div(
    class = "d-flex justify-content-between align-items-center bg-light p-2",
    h2("Chemical Space Explorer", class = "m-0"),
    actionButton("show_info", "About", icon = icon("info-circle"), 
                class = "btn-outline-primary")
  ),
  
  # Main layout
  fluidRow(
    # Left column - Map and controls
    column(
      width = 5,
      card(
        full_screen = TRUE,
        height = "600px",
        card_header("Interactive Country Selection"),
        withSpinner(leafletOutput("selection_map", height = "400px")),
        
        # Essential controls below map
        div(
          class = "mt-3",
          fluidRow(
            column(6, 
                  selectInput("chemical_filter", "Chemical Category:",
                            choices = c("All", chemical_categories),
                            selected = "All")
            ),
            column(6,
                  selectInput("region_filter", "Region Filter:",
                            choices = regions,
                            selected = "All")
            )
          ),
          sliderInput("year_range", "Year Range:",
                     min = 1996, max = 2022,
                     value = c(1996, 2022),
                     step = 1, sep = "")
        )
      )
    ),
    
    # Right column - Visualizations
    column(
      width = 7,
      # Top contributors card
      uiOutput("top_contributors_ui"),
      
      # Main visualization card
      card(
        full_screen = TRUE,
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            h4("Contribution Analysis", class = "m-0"),
            div(
              class = "btn-group",
              actionButton("show_individual", "Individual Data", 
                         class = "btn-sm btn-outline-primary active"),
              actionButton("show_collabs", "Collaborations",
                         class = "btn-sm btn-outline-primary")
            )
          )
        ),
        withSpinner(plotlyOutput("main_plot", height = "400px"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    selected_country = NULL,
    view_mode = "individual"
  )
  
  # Create initial map
  output$selection_map <- renderLeaflet({
    create_selection_map(country_list = country_list)
  })
  
  # Handle map clicks
  observeEvent(input$selection_map_shape_click, {
    clicked <- input$selection_map_shape_click$id
    
    if (clicked == rv$selected_country) {
      # Deselect if clicking same country
      rv$selected_country <- NULL
      collaborating_countries <- NULL
    } else {
      # Select new country and find collaborators
      rv$selected_country <- clicked
      collaborating_countries <- find_collaborating_countries(
        ds,
        clicked,
        input$year_range,
        input$chemical_filter
      )
    }
    
    # Update map
    leafletProxy("selection_map") %>%
      clearShapes() %>%
      addPolygons(
        data = create_selection_map(
          selected_country = rv$selected_country,
          collaborating_countries = collaborating_countries,
          country_list = country_list
        )$x$calls[[2]]$args[[1]]
      )
  })
  
  # Update view mode
  observeEvent(input$show_individual, {
    rv$view_mode <- "individual"
    updateButton(session, "show_individual", class = "btn-sm btn-outline-primary active")
    updateButton(session, "show_collabs", class = "btn-sm btn-outline-primary")
  })
  
  observeEvent(input$show_collabs, {
    rv$view_mode <- "collaborations"
    updateButton(session, "show_collabs", class = "btn-sm btn-outline-primary active")
    updateButton(session, "show_individual", class = "btn-sm btn-outline-primary")
  })
  
  # Render main visualization
  output$main_plot <- renderPlotly({
    req(rv$selected_country)
    
    if (rv$view_mode == "individual") {
      # Get individual country data
      data <- process_visualization_data(
        ds,
        rv$selected_country,
        input$year_range,
        input$chemical_filter,
        input$region_filter
      )
      
      create_contributions_plot(data, input$chemical_filter)
    } else {
      # Get collaboration data
      collaborating_countries <- find_collaborating_countries(
        ds,
        rv$selected_country,
        input$year_range,
        input$chemical_filter
      )
      
      if (nrow(collaborating_countries) == 0) {
        plot_ly() %>%
          add_annotations(
            text = "No collaborations found for the selected criteria",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 16)
          )
      } else {
        plot_ly(collaborating_countries) %>%
          add_bars(
            x = ~reorder(partners, total_contribution),
            y = ~total_contribution,
            text = ~paste(
              "Country:", partners,
              "<br>Total Contribution:", round(total_contribution, 2), "%",
              "<br>Number of Collaborations:", collaboration_count
            ),
            hoverinfo = "text"
          ) %>%
          layout(
            title = "Collaboration Strength",
            xaxis = list(title = "Collaborating Countries"),
            yaxis = list(title = "Total Contribution (%)")
          )
      }
    }
  })
  
  # Show top contributors
  output$top_contributors_ui <- renderUI({
    # Only show when no country is selected
    req(is.null(rv$selected_country))
    
    top_data <- get_top_contributors(
      ds,
      input$year_range,
      input$chemical_filter,
      input$region_filter
    )
    
    card(
      card_header("Top Contributors"),
      div(
        class = "p-2",
        renderTable({
          top_data %>%
            select(
              Country = country,
              `Total Contribution (%)` = total_contribution,
              `Years Active` = years_active,
              `Average Contribution (%)` = avg_contribution
            ) %>%
            mutate(
              across(ends_with("(%)"), ~round(., 2))
            )
        })
      )
    )
  })
  
  # Info modal
  observeEvent(input$show_info, {
    showModal(modalDialog(
      title = "About Chemical Space Explorer",
      "This application visualizes country contributions to the chemical space across different years and categories. 
      Select countries on the map to explore their individual contributions and collaborations.",
      size = "m",
      easyClose = TRUE
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)