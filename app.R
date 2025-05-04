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

# ... (library loading, conflict resolution, theme setting) ...

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
ui <- page_navbar(
  title = "Chemical Space Explorer", # Added a title
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE, # Prevent content from filling height excessively

  # --- REMOVED sidebar argument ---

  # --- Top Right Menu ---
  nav_menu(
    # icon = bsicons::bs_icon("patch-exclamation-fill"), # Optional icon
    title = "Useful Links",
    align = "right",
    # --- MOVED input_dark_mode ---
    nav_item(link_github)
    # Add other links/items here if needed
  ),

  # --- Explorer Tab ---
  nav_panel(
    title = "Chemical Space Explorer",
    # --- Row 1: Map and Filters ---
    layout_columns(
      col_widths = c(9, 3), # Adjust column widths as needed (Map | Filters)
      # --- Map Card ---
      card(
        full_screen = TRUE,
        tooltip(
          bsicons::bs_icon("question-circle"),
          "Click on a country to select it. Click again to deselect. Data will be shown for the selected countries."
        ),
        card_header("Country Selection Map 🌎"),
        leafletOutput("selection_map", height = "450px") # Increased height slightly
        # Removed footer for cleaner look
      ),
      # --- Filters Card ---
      card(
        card_header("Filters 🔍"),
        card_body( # Added card_body for padding
          # --- MOVED Filter Controls Here ---
          sliderInput(
            "years", "Year Range 📅:",
            min = min_year_data,
            max = max_year_data,
            value = c(max(min_year_data, 1996, na.rm = TRUE), min(max_year_data, 2022, na.rm = TRUE)),
            step = 1, sep = ""
          ),
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Chemical Space was divided into three subspaces: Organic, Organometallic, and Rare-Earths. Plots default to 'All' subspaces.",
          ),
          radioButtons(
            "chemical_category", "Chemical Space Category 🧪:",
            choices = chemical_categories,
            selected = "All"
          ),
          uiOutput("region_filter_ui"), # Filter Map by Region (conditionally shown via server)
          actionButton(
            "clear_selection", "Clear Selection",
            icon = icon("trash-alt"),
            class = "btn-outline-danger btn-sm mt-3", # Added margin-top
            width = "100%"
          )
        )
      )
    ), # End layout_columns for Map & Filters

    # --- Row 2: Plots and Table ---
    navset_card_pill(
      id = "plot_tabs", # Added an ID for potential future use
      full_screen = TRUE,
      title = uiOutput("plot_header_ui"), # Dynamic header as title
      # --- Main Plot Panel ---
      nav_panel(
        title = "Trends 📈", # Simplified title
        value = "trends",   # Added value for identification
        uiOutput("display_mode_ui"), # Conditional UI for multi-country selection
        withSpinner(plotlyOutput("main_plot", height = "400px"))
      ),
      # --- Contribution Map Panel ---
      nav_panel(
        title = "Contribution Map 📌", # Simplified title
        value = "contribution_map",    # Added value
        helpText("Shows the average contribution percentage over the selected period for the currently displayed countries."),
        withSpinner(plotlyOutput("contributionMapPlot", height = "400px"))
      ),
      # --- Data Table Panel ---
      nav_panel(
        title = "Data Table",
        value = "data_table", # Added value
        DTOutput("summary_table")
      ),
      card_footer(
          "About these percentages. ",
          popover(
          a("Learn more", href = "#"),
          markdown("These percentages represent the total proportion of chemical space discovered through individual country participation or collaborations between the selected countries. Higher percentages indicate stronger and more productive scientific partnerships in terms of novel chemical discoveries."),
          )
      )
    ), # End navset_card_pill

    # --- Row 3: Value Boxes ---
    # This layout_columns will contain the value boxes
    # We render this UI dynamically from the server
    uiOutput("summary_value_boxes_ui")

  ), # End Explorer nav_panel

  # --- Article Figures Tab ---
  nav_panel(
    title = "Article Figures",
    # Add the descriptive text here
    p(
      class = "text-muted", # Optional: style the text slightly
      "China's Chemical Revolution: From 1996 to 2022, China surged to claim the chemical discoveries—far outpacing the US’s share—driven almost entirely by domestic research. In contrast, US solo contributions has steadily dropped, with rising international collaboration."
    ),
    helpText("These plots replicate key figures from the source article."),
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
  ), # End Article Figures nav_panel

  # --- Article Tab ---
  nav_panel(
    title = "Original Article 📄",
    # Use tags$iframe to embed the PDF
    # Make sure 'original_article.pdf' is inside the 'www' folder
    tags$iframe(
      style = "height:80vh; width:100%; scrolling:yes; border: none;", # Adjust height as needed (e.g., 800px or 80vh)
      src = "original_article.pdf" # The path relative to the www directory
    )
  ), # End nav_panel


  # --- Legal Notice Tab ---
  nav_panel(
    title = "Legal Notice", # Updated title
    # fluidPage( # fluidPage might not be necessary within nav_panel + card
      card(
        card_header("Legal Notice / Provider Identification"),
        card_body(
          markdown(
            "The following provides mandatory data concerning the provider of this Website, obligations with regard to data protection, as well as other important legal references involving the Website of the Max Planck Institute for Physics (Werner-Heisenberg Institute) Munich (http://www.mpp.mpg.de) as required by German law."
          ),
          h4("Provider"),
          markdown(
            "The provider of this Internet site within the legal meaning of the term is the registered association Max Planck Society for the Advancement of Science e.V."
          ),
          h4("Address"),
          markdown(
            "Max-Planck-Gesellschaft zur Förderung der Wissenschaften e.V.  
            Hofgartenstrasse 8  
            D-80539 Munich  
            +49 89 2108-0  
            [http://www.mpg.de](http://www.mpg.de)"
          ),
          h4("Register of Societies and Associations"),
          markdown(
            "The Max Planck Society is registered in the Official Register of Societies and Associations at Berlin-Charlottenburg Local Court under the register number VR 13378 B."
          ),
          h4("Representatives"),
          markdown(
            "The Max Planck Society is legally represented by its Board of Directors which, in turn, is represented by the President of the Society, Prof. Dr. Patrick Cramer, and by Secretary General Simone Schwanitz."
          ),
          h4("Value added tax identification number"),
          markdown(
            "The value added tax identification number of the Max Planck Society is DE 129517720."
          ),
          h4("Editor"),
          markdown(
            "Responsible editor for the contents of the website of the Max Planck Institute for Physics  
            ([http://www.mpp.mpg.de](http://www.mpp.mpg.de)) with regard to media law is press officer:  
            Barbara Wankerl  
            Max Planck Institute for Physics  
            Boltzmannstr. 8  
            85748 Garching  
            Germany  
            +49 89 32354-292  
            barbara.wankerl@mpp.mpg.de"
          ),
          h4("Technically responsible"),
          markdown(
            "Technically responsible for the website of the Max Planck Institute for Physics ([http://www.mpp.mpg.de](http://www.mpp.mpg.de)) are Thomas Hahn (hahn@mpp.mpg.de) and Uwe Leupold (webmaster@mpp.mpg.de). Support, maintainance and upgrades of the TYPO3 domain are handled by the web company metapublic GbR (Baldestr. 14, 80469 München)."
          ),
          h4("Legal Structure"),
          markdown(
            "The Max Planck Society is a non-profit research facility which is organized as a registered association. All of the institutes and facilities of the Max Planck Society are largely autonomous in terms of organization and research, but as a rule have no legal capacity of their own."
          ),
          h4("Liability for Contents of Online Information"),
          markdown(
            "As the provider of contents in accordance with Section 7 Paragraph 1 of the Tele-Media Law, the Max Planck Society shall be responsible for any contents which it makes available for use in accordance with general legal provisions. The Max Planck Society makes every effort to provide timely and accurate information on this Web site. Nevertheless, errors and inaccuracies cannot be completely ruled out. Therefore, the Max Planck Society does not assume any liability for the relevance, accuracy, completeness or quality of the information provided. The Max Planck Society shall not be liable for damage of a tangible or intangible nature caused directly or indirectly through the use or failure to use the information offered and/or through the use of faulty or incomplete information unless it is verifiably culpable of intent or gross negligence. The same shall apply to any downloadable software available free of charge. The Max Planck Society reserves the right to modify, supplement, or delete any or all of the information offered on its Internet site, or to temporarily or permanently cease publication thereof without prior and separate notification."
          ),
          h4("Copyright"),
          markdown(
            "The layout, graphics employed and any other contents on the homepage of the Max Planck Society Internet site are protected by copyright law.  
            © Max-Planck-Gesellschaft zur Förderung der Wissenschaften e.V., Munich. All rights reserved."
          )
        )
      )
    # ) # End fluidPage if used
  ), # End Legal Notice nav_panel

  # --- Privacy Policy Tab ---
  nav_panel(
    title = "Privacy Policy", # New title
    # fluidPage( # fluidPage might not be necessary within nav_panel + card
      card(
        card_header("Privacy Policy"),
        card_body(
          markdown(
            "The Max-Planck-Gesellschaft zur Förderung der Wissenschaften e.V. (MPG) takes the protection of your personal data very seriously. Here we provide you with information concerning the main aspects of data processing in the context of our application and recruitment procedures."
          ),
          h4("1. Contact details of the data controller"),
          markdown(
            "The controller as defined by the EU General Data Protection Regulation (GDPR) and other provisions under data protection law is:  
            
            Max-Planck-Gesellschaft zur Förderung der Wissenschaften e.V. (MPG)  
            Hofgartenstraße 8, 80539 Munich  
            Phone: +49 89 2108 -0  
            [www.mpg.de](www.mpg.de)"
          ),
          h4("2. Contact details of the Data Protection Officer"),
          markdown(
            "The Data Protection Officer of the controller is Heidi Schuster, Hofgartenstraße 8, D-80539 Munich, telephone: +49 (89) 2108-1554, email address: datenschutz@mpg.de"
          ),
          h4("3. Purpose and legal basis of data processing"),
          markdown(
            "The personal data entered by you in our application system is saved and processed solely for the application and recruitment processes of the Max Planck Institute for Mathematics in the Sciences, Leipzig.  
            
            Fields marked with an * are mandatory fields that are required for the application process. If you do not provide these details, you cannot take part in the application process. The legal basis for processing is Article 6, para. 1, lit b GDPR (contractual basis) and § 26 Federal Data Protection Act 2018 (data processing as the basis for establishing an employment contract).  
            
            Fields not marked with an * are fields which you can complete voluntarily. By completing the voluntary fields, you give us your permission to store and process this data exclusively for the purpose of the application and recruitment process. The legal basis for processing in these instances is Article 6, para. 1, lit. a GDPR (consent of the data subject)."
          ),
          h4("4. Data recipients and categories of data recipients"),
          markdown(
            "Processing of your personal details is carried out by way of order processing on Haufe-umantis AG systems. The Max-Planck-Gesellschaft as the responsible body only has access to your data insofar as this is necessary for the application and recruitment process in line with the internal allocation of responsibilities. Personnel management staff have access to the data. They transfer the data to the selection committee assembled for the respective application procedure, as well as to the Gender Equality Officer and the Works Council. The selection committee may also include external experts with whom the data is then shared. The data is not shared with any other third parties."
          ),
          h4("5. Duration of storage"),
          markdown(
            "Your data will be stored until the end of the recruitment process or, in the case of a speculative application, until the end of the exploratory process or the appointment process, as the case may be. We only save your data for longer if you have given us your consent to do so."
          ),
          h4("6. Your rights"),
          markdown(
            "You can view, alter or delete the data you have entered at any time using your personal access to the application system. Your access is protected using your email address as a personal login name and a password of your choice.  
            
            You are fundamentally entitled to the rights of access (Article 15 GDPR), rectification (Article 16 GDPR), erasure (Article 17, para. 1 GDPR), restriction of processing (Article 18 GDPR), data portability (Article 20 GDPR) and withdrawal of consent (Article 7, para. 3 GDPR).  
            
            In order to assert your rights, please contact:  
            Max Planck Institute for Mathematics in the Sciences  
            Inselstrasse 22  
            04103 Leipzig  
            
            If you believe that processing of your personal data is in breach of data protection law or your claims under data protection law are being violated in any other way, please contact the Max-Planck-Gesellschaft Data Protection Officer at datenschutz@mpg.de. The supervisory authority responsible for the Max-Planck-Gesellschaft is:  
            BayLDA (Bavarian Data Protection Authority)  
            Postfach 606, 91511 Ansbach."
          )
        )
      )
    # ) # End fluidPage if used
  ), # End Privacy Policy nav_panel

  # --- Footer ---
  footer = div(
    # ... (Footer content remains the same) ...
     style = "text-align: center; padding: 10px; background: #f8f9fa;",
    "Source: Bermúdez-Montaña, M., et al. (2025). China's rise in the chemical space and the decline of US influence.",
    a("Working Paper", href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6")
  )
) # End page_navbar

# --- Server Logic ---
server <- function(input, output, session) {
  # --- Reactive Values ---
  selected_countries_immediate <- reactiveVal(c()) # Stores ISO codes immediately on click
  # Debounce still needed for downstream calculations (plots, table, value boxes)
  # Let's set it back to 3 seconds for testing, adjust as needed (e.g., 1000-1500ms might be good)
  selected_countries <- debounce(selected_countries_immediate, 3000)
  display_mode <- reactiveVal("compare_individuals") # Default mode for >1 selection

  # --- Constants ---
  available_color <- "lightgray" # Define default color once

  # --- Prepare Map Data (sf object) ---
  # Create this once for use in proxy updates
  world_map_prep <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world_map_prep %>%
      select(iso_a2, name_long, geometry) %>%
      left_join(country_list, by = c("iso_a2" = "iso2c")) %>%
      filter(!is.na(country)) %>% # Keep only countries present in our dataset
      mutate(
          region = factor(region, levels = regions),
          # Provide a default color if 'cc' is missing for some reason
          cc = ifelse(is.na(cc) | cc == "", "#808080", cc) # Default to gray if cc is missing
      )
  rm(world_map_prep) # Clean up intermediate object

  # --- Map Interaction ---

  # Render initial map
  output$selection_map <- renderLeaflet({
    # Use the pre-calculated map_data here if create_selection_map can accept it
    # Or keep create_selection_map as is, it recalculates map_data internally
    create_selection_map(selected_countries_immediate(), country_list, regions)
  })

  # Handle map clicks - Remove and re-add the clicked shape
  observeEvent(input$selection_map_shape_click, {
    clicked_iso <- input$selection_map_shape_click$id
    req(clicked_iso)
    current_selection <- selected_countries_immediate()

    # Get data for the specific country clicked
    country_sf_data <- map_data %>% filter(iso_a2 == clicked_iso)
    # Ensure we found the country data
    req(nrow(country_sf_data) == 1)

    new_selection <- current_selection
    new_color <- available_color

    if (clicked_iso %in% current_selection) {
      # Deselect
      new_selection <- setdiff(current_selection, clicked_iso)
      new_color <- available_color
    } else {
      # Select
      new_selection <- c(current_selection, clicked_iso)
      # Use the color stored in country_sf_data$cc
      new_color <- country_sf_data$cc[1]
    }

    # Update the reactive value
    selected_countries_immediate(new_selection)

    # --- Remove and Re-add the specific polygon ---
    leafletProxy("selection_map") %>%
      removeShape(layerId = clicked_iso) %>% # Remove the old shape
      addPolygons( # Re-add it with the new color
          data = country_sf_data,
          layerId = ~iso_a2,
          group = ~region, # Ensure it's added to the correct group
          fillColor = new_color, # Use the determined color
          # --- Copy other style options from create_selection_map ---
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.8,
              bringToFront = TRUE
          ),
          label = ~paste(country),
          labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
          )
          # --- End copied style options ---
      )
  })

  # --- Region Filter Map Control (Handles showing/hiding layers) ---
  # This observer remains the same
  observe({
    current_region_filter <- input$region_filter
    req(!is.null(current_region_filter))

    proxy <- leafletProxy("selection_map")
    if (current_region_filter == "All") {
      proxy %>% showGroup(regions)
    } else {
      proxy %>%
        hideGroup(setdiff(regions, current_region_filter)) %>%
        showGroup(current_region_filter)
    }
  })


  # Clear selection button - Remove and re-add previously selected shapes
  observeEvent(input$clear_selection, {
    previously_selected <- selected_countries_immediate()
    selected_countries_immediate(c())

    if (length(previously_selected) > 0) {
        proxy <- leafletProxy("selection_map")
        for (iso in previously_selected) {
            # Get data for the specific country
            country_sf_data <- map_data %>% filter(iso_a2 == iso)
            if(nrow(country_sf_data) == 1) { # Check if found
                proxy %>%
                  removeShape(layerId = iso) %>% # Remove the old shape
                  addPolygons( # Re-add it with the available color
                      data = country_sf_data,
                      layerId = ~iso_a2,
                      group = ~region,
                      fillColor = available_color, # Reset color
                      # --- Copy other style options ---
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      highlightOptions = highlightOptions(
                          weight = 2,
                          color = "#666",
                          dashArray = "",
                          fillOpacity = 0.8,
                          bringToFront = TRUE
                      ),
                      label = ~paste(country),
                      labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "12px",
                          direction = "auto"
                      )
                      # --- End copied style options ---
                  )
            }
        }
    }
  })

  # ... (rest of server logic) ...


  # --- Dynamic UI Elements ---
  # ... (output$region_filter_ui, output$display_mode_ui, observeEvent display_mode_select, output$plot_header_ui remain the same) ...
   output$region_filter_ui <- renderUI({
    tagList(
      # hr(), # Removed extra hr
      selectInput(
        "region_filter", "Filter Map Layers by Region:", # Updated label
        choices = c("All", regions),
        selected = "All"
      )
    )
  })

  output$display_mode_ui <- renderUI({
    countries <- selected_countries() # Use debounced value
    req(countries)
    if (length(countries) > 1) {
      radioButtons(
        "display_mode_select", "Display Mode: ✨",
        choices = c(
          "Countrywise expansion of the CS 🗾" = "compare_individuals",
          "Find Joint Collaborations Trends 🤝🏽" = "find_collaborations"
        ),
        selected = display_mode(),
        inline = TRUE
      )
    } else {
      NULL
    }
  })

  observeEvent(input$display_mode_select, {
    display_mode(input$display_mode_select)
  })

  output$plot_header_ui <- renderUI({
    countries <- selected_countries() # Use debounced value
    req(countries)
    mode <- display_mode()
    chem <- input$chemical_category
    get_plot_header(countries, mode, chem, country_list) # Use helper function
  })


  # --- Data Processing ---
  # Use the DEBOUNCED selected_countries reactive here
  processed_data <- reactive({
    countries <- selected_countries() # Use debounced value
    # ... (rest of processing logic remains the same) ...
     req(length(countries) > 0, input$years, input$chemical_category) # Require at least one selection

    mode <- if (length(countries) == 1) "individual" else display_mode()
    current_region_filter_for_data <- if (mode %in% c("compare_individuals", "individual")) {
      req(input$region_filter)
    } else {
      "All"
    }

    withProgress(message = "Processing data...", {
      get_display_data(
        ds = ds,
        selected_isos = countries, # Pass debounced value
        year_range = input$years,
        chemical_category = input$chemical_category,
        display_mode = mode,
        region_filter = current_region_filter_for_data,
        country_list = country_list
      )
    })
  }) %>% bindCache(
    # Cache key remains the same, depends on debounced value
    paste(sort(selected_countries()), collapse = "-"),
    input$years,
    input$chemical_category,
    display_mode(),
    if (display_mode() %in% c("individual", "compare_individuals")) input$region_filter else "All"
  )

  # --- Outputs ---
  # ... (output$main_plot, output$contributionMapPlot, output$summary_table, output$summary_value_boxes_ui remain the same) ...
  # ... (output$articleGdpPlot etc. remain the same) ...
  output$main_plot <- renderPlotly({
    validate(
      need(length(selected_countries()) > 0, "Click on the map to select a country.")
    )
    data_to_plot <- processed_data()
    validate(
      need(nrow(data_to_plot) > 0, "No data available for the current selection and filters.")
    )
    mode <- if (length(selected_countries()) == 1) "individual" else display_mode()
    create_main_plot(
      data = data_to_plot,
      display_mode = mode,
      selected_isos = selected_countries(),
      country_list = country_list
    )
  })

  output$contributionMapPlot <- renderPlotly({
    countries <- selected_countries()
    req(countries)
    mode <- if (length(countries) == 1) "individual" else display_mode()
    validate(
      need(length(countries) > 0, "Select countries on the map."),
      need(mode %in% c("individual", "compare_individuals"), "Map is available only when viewing individual country data.")
    )
    data_for_map <- processed_data()
    validate(
      need(nrow(data_for_map) > 0, "No data available to generate map.")
    )
    create_contribution_map_plot(
      processed_data_df = data_for_map,
      main_title = paste("Chemical space selected:", input$chemical_category),
      fill_label = "Avg % Contribution"
    )
  })

  output$summary_table <- renderDT({
    countries <- selected_countries()
    validate(need(length(countries) > 0, "Select countries to see summary data."))
    data_to_summarize <- processed_data()
    validate(
      need(nrow(data_to_summarize) > 0, "No data available for the current selection.")
    )
    mode <- if (length(selected_countries()) == 1) "individual" else display_mode()
    create_summary_table(
      data = data_to_summarize,
      display_mode = mode
    )
  })

  output$summary_value_boxes_ui <- renderUI({
    countries <- selected_countries()
    validate(
        need(length(countries) > 0, ""),
        need(!is.null(processed_data()), "")
    )
    data <- processed_data()
    validate(need(nrow(data) > 0, ""))

    mode <- if (length(countries) == 1) "individual" else display_mode()

    # --- Calculate Stats ---
    avg_contrib_val <- NA
    if ("total_percentage" %in% names(data)) {
        avg_contrib_val <- mean(data$total_percentage, na.rm = TRUE)
    } else if ("collaboration_percentage" %in% names(data)) {
        avg_contrib_val <- mean(data$collaboration_percentage, na.rm = TRUE)
    }
    avg_contrib_str <- if (!is.na(avg_contrib_val)) scales::percent(avg_contrib_val / 100, accuracy = 0.1) else "N/A"
    avg_contrib_title <- if(mode == "find_collaborations") "Avg. Collaboration %" else "Avg. Contribution %"

    num_points <- nrow(data)
    num_years <- length(unique(data$year))
    points_str <- paste(num_points, "data points across", num_years, "years")

    peak_year_val <- NA
    peak_year_str <- "N/A"
    if ("total_percentage" %in% names(data)) {
        if(nrow(data) > 0) { # Ensure data is not empty before which.max
            peak_row <- data[which.max(data$total_percentage), ]
            if(nrow(peak_row) > 0) {
                peak_year_val <- peak_row$year[1]
                peak_perc_val <- peak_row$total_percentage[1]
                peak_year_str <- paste(peak_year_val, " (", scales::percent(peak_perc_val / 100, accuracy = 0.1), ")", sep="")
            }
        }
    }
    peak_year_title <- if(mode == "find_collaborations") "Peak Collaboration Year" else "Peak Contribution Year"

    # --- Create Value Boxes ---
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Countries Selected",
        value = length(countries),
        showcase = bsicons::bs_icon("globe-americas"),
        theme = "primary"
      ),
      value_box(
        title = avg_contrib_title,
        value = avg_contrib_str,
        showcase = bsicons::bs_icon("graph-up-arrow"),
         theme = "info"
      ),
      value_box(
        title = peak_year_title,
        value = peak_year_str,
        showcase = bsicons::bs_icon("calendar-event"),
         theme = "success"
      )
    )
  })

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


} # End server

# Run the app
shinyApp(ui, server)
