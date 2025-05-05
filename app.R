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
# Define links with icons for the nav_menu
link_app_repo <- tags$a(
  bsicons::bs_icon("github"), "App Repository",
  href = "https://github.com/santi-rios/Chemical-Space/",
  target = "_blank"
)
link_issues <- tags$a(
  bsicons::bs_icon("bug"), "Report Issue",
  href = "https://github.com/santi-rios/Chemical-Space/issues",
  target = "_blank"
)
link_raw_data <- tags$a(
  bsicons::bs_icon("database"), "Raw Data Source",
  href = "https://github.com/mbermudezmoTec/China-s-great-leap-forward-in-the-chemical-space/tree/main/data",
  target = "_blank"
)
link_pi <- tags$a(
  bsicons::bs_icon("person"), "PI Website",
  href = "https://sites.google.com/site/guillermorestrepo/Home",
  target = "_blank"
)
link_preprint <- tags$a(
  bsicons::bs_icon("file-earmark-text"), "Preprint (ChemRxiv)",
  href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6",
  target = "_blank"
)
link_mis_mpg <- tags$a(
  bsicons::bs_icon("building"), "MPI MiS",
  href = "https://www.mis.mpg.de/",
  target = "_blank"
)
link_tec_monterrey <- tags$a(
  bsicons::bs_icon("mortarboard"), "Tecnológico de Monterrey",
  href = "https://tec.mx/es/ingenieria-y-ciencias",
  target = "_blank"
)
# Keep the old GitHub link if it's different (e.g., personal publications)
link_personal_pubs <- tags$a(
  shiny::icon("github"), "Author Publications", # Renamed for clarity
  href = "https://santi-rios.github.io/publications/",
  target = "_blank"
)


# --- UI Definition ---
ui <- page_navbar(
  title = "Chemical Space Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   # Optional: Add some custom CSS for spacing if needed
                   # "navbar-nav { margin-right: 1rem; }", # Example
                   # ".navbar-brand { margin-right: 2rem; }" # Example
                   ),
  fillable = FALSE,

  # Add a spacer between title and main tabs for better visual separation
  nav_spacer(),

  # --- Explorer Tab ---
  nav_panel(
    title = "Chemical Space Explorer",
    # --- Row 1: Map and Filters ---
    layout_columns(
      # Responsive column widths: Full width stack on XS/SM, 8/4 on MD+, 9/3 on LG+
      col_widths = c(12, 12, 8, 4, 9, 3),
      # --- Map Card ---
      card(
        full_screen = TRUE,
        card_header(
          "Country Selection Map 🌎",
          tooltip(
            bsicons::bs_icon("info-circle-fill"), # Changed icon slightly
            "Click on a country to select/deselect. Data updates after a short delay. Use filters on the right to narrow down data or map layers."
            )
          ),
        # Add a placeholder while map loads
        shinycssloaders::withSpinner(leafletOutput("selection_map", height = "450px"))
      ),
      # --- Filters Card ---
      card(
        card_header("Filters & Options ⚙️"), # Renamed header
        card_body(
          # Region Filter (Dynamic UI) - Moved to top for better dropdown visibility
          uiOutput("region_filter_ui"),
          # Add a visual separator and spacing
          shiny::tags$hr(style = "margin-top: 1rem; margin-bottom: 1rem;"),
          # Year Slider
          sliderInput(
            "years", "Year Range:", # Simplified label
            min = min_year_data,
            max = max_year_data,
            value = c(
              max(min_year_data, 1996, na.rm = TRUE),
              min(max_year_data, 2022, na.rm = TRUE)
            ),
            step = 1, sep = ""
          ),
          # Chemical Category Radio Buttons
          radioButtons(
            "chemical_category", "Chemical Space Category:", # Simplified label
            choices = chemical_categories,
            selected = "All"
          ),
          tooltip( # Tooltip associated with radio buttons
            bsicons::bs_icon("question-circle"),
            paste(
              "Filter data by chemical subspace.",
              "'All' includes Organic, Organometallic, and Rare-Earths."
            ),
            placement = "right"
          ),
          # Add some space before the button
          shiny::div(style = "margin-top: 2rem;"),
          # Clear Selection Button
          actionButton(
            "clear_selection", "Clear Map Selection", # Clarified label
            icon = icon("trash-alt"),
            class = "btn-outline-danger btn-sm", # Removed mt-3, handled by div above
            width = "100%"
          )
        )
      )
    ), # End layout_columns for Map & Filters

    # --- Row 2: Plots and Table ---
    # Conditional UI for display mode (appears when >1 country selected)
    uiOutput("display_mode_ui"),
    # Tabset for plots and table
    navset_card_pill(
      id = "plot_tabs",
      full_screen = TRUE,
      # --- Main Plot Panel ---
      nav_panel(
        title = "Trends",
        icon = bsicons::bs_icon("graph-up"), # Added icon
        value = "trends",
        # Dynamic header shows what's being plotted
        uiOutput("plot_header_ui"),
        # Add placeholder/spinner
        shinycssloaders::withSpinner(plotlyOutput("main_plot", height = "400px"))
      ),
      # --- Contribution Map Panel ---
      nav_panel(
        title = "Contribution Map",
        icon = bsicons::bs_icon("geo-alt-fill"), # Added icon
        value = "contribution_map",
        helpText("Shows average contribution percentage over the selected period for individual countries."),
        shinycssloaders::withSpinner(plotlyOutput("contributionMapPlot", height = "400px"))
      ),
      # --- Data Table Panel ---
      nav_panel(
        title = "Data Table",
        icon = bsicons::bs_icon("table"), # Added icon
        value = "data_table",
        helpText("Detailed data for the current selection. Use search box to filter."),
        shinycssloaders::withSpinner(DTOutput("summary_table"))
      )
    ), # End navset_card_pill

    # --- Row 3: Value Boxes ---
    # Dynamic UI for value boxes (now shows overall top contributors)
    uiOutput("summary_value_boxes_ui")

  ), # End Explorer nav_panel

  # --- Article Figures Tab ---
  nav_panel(
    title = "Article Figures",
    icon = bsicons::bs_icon("bar-chart-line-fill"), # Added icon
    p(
      strong("Key Findings from the Article:"), # Added emphasis
      "From 1996 to 2022, China surged to dominate chemical discoveries, driven mainly by domestic research, while US solo contributions declined amidst rising international collaboration."
    ),
    helpText("Static plots replicating key figures from the source article. These plots do not react to the filters in the 'Explorer' tab."),
    layout_columns(
      # Responsive: 2 cols on MD+, 1 col on SM/XS
      col_widths = c(12, 12, 12, 12, 12),
      # Plots arranged in cards
      card(
        card_header("Country Participation in the Chemical Space"),
        shinycssloaders::withSpinner(plotlyOutput("countrycsPlot", height = "350px"))
      ),
      card(
        card_header("Top 10 Collaboration Trends (All Chemicals)"),
        shinycssloaders::withSpinner(plotlyOutput("articleTopCollabsPlot", height = "350px"))
      ),
      card(
        card_header("GDP Growth Rate"),
        shinycssloaders::withSpinner(plotlyOutput("articleGdpPlot", height = "350px"))
      ),
      card(
        card_header("Number of Researchers"),
        shinycssloaders::withSpinner(plotlyOutput("articleResearchersPlot", height = "350px"))
      ),
      card(
        card_header("Chemical Space Expansion"),
        shinycssloaders::withSpinner(plotlyOutput("articleCsExpansionPlot", height = "350px"))
      )
    )
  ), # End Article Figures nav_panel

  # --- Article Tab ---
  nav_panel(
    title = "Original Article",
    icon = bsicons::bs_icon("file-pdf"), # Added icon
    helpText("Displaying the original article PDF. Ensure your browser allows embedding or use the link in 'Useful Links' to open directly."),
    tags$iframe(
      style = "height:80vh; width:100%; scrolling:yes; border: none;",
      src = "original_article.pdf"
    )
  ), # End nav_panel

  # --- Legal & Privacy Tabs (Combined into a Menu for less clutter) ---
  nav_menu(
      title = "Legal Info",
      align = "left", # Keep these less prominent than main tabs
      icon = bsicons::bs_icon("shield-check"),
      nav_panel(
          title = "Legal Notice",
          card(
              card_header("Legal Notice / Provider Identification"),
              card_body(
                  # ... (Legal notice markdown content remains the same) ...
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
              ) # end card_body
          ) # end card
      ), # end nav_panel Legal Notice
      nav_panel(
          title = "Privacy Policy",
          card(
              card_header("Privacy Policy"),
              card_body(
                  # ... (Privacy policy markdown content remains the same) ...
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
              ) # end card_body
          ) # end card
      ) # end nav_panel Privacy Policy
  ), # End Legal Info nav_menu

  # Add another spacer before the right-aligned menu
  nav_spacer(),

  # --- Top Right Menu (Useful Links) ---
  nav_menu(
    title = "Useful Links",
    align = "right",
    icon = bsicons::bs_icon("link-45deg"), # Added icon
    # Add the new links here
    nav_item(link_app_repo),
    nav_item(link_issues),
    nav_item(link_raw_data),
    nav_item(link_preprint),
    nav_item(link_pi),
    nav_item(link_mis_mpg),
    nav_item(link_tec_monterrey),
    nav_item(link_personal_pubs) # Kept the original link too
  ),

  # --- Footer ---
  footer = div(
     style = "text-align: center; padding: 10px; background: #f8f9fa; border-top: 1px solid #dee2e6;", # Added border
    "Source: Bermúdez-Montaña, M., et al. (2025). China's rise in the chemical space and the decline of US influence.",
    a("Working Paper (ChemRxiv)", href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6", target = "_blank") # Added target blank
  )
) # End page_navbar


# --- Server Logic ---
server <- function(input, output, session) {
  # --- Reactive Values ---
  selected_countries_immediate <- reactiveVal(c()) # Stores ISO codes immediately on click
  # Debounce still needed for downstream calculations (plots, table, value boxes)
  # Let's set it back to 3 seconds for testing, adjust as needed (e.g., 1000-1500ms might be good)
  selected_countries <- debounce(selected_countries_immediate, 1000)
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
# ... (previous server logic) ...

 # --- Updated Value Box UI for Overall Top Contributors ---
  output$summary_value_boxes_ui <- renderUI({
    # Use reactive inputs for filters, except year range
    chemical_category <- input$chemical_category
    region_filter <- input$region_filter

    # Ensure inputs are available
    req(chemical_category, region_filter) # Year range not needed here

    # Calculate top contributors using the helper function, ignoring year filter
    top_data_raw <- calculate_top_contributors(
      ds = ds,
      year_range = c(min_year_data, max_year_data), # Pass dummy range, it will be ignored
      chemical_category = chemical_category,
      region_filter = region_filter,
      country_list = country_list,
      top_n = 5,
      ignore_year_filter = TRUE # <<< Key change: Ignore year slider
    )

    # Validate that we got results
    validate(need(nrow(top_data_raw) > 0, "No overall contributor data for current chemical/region filters."))

    # --- Explicitly arrange by rank just in case ---
    top_data <- top_data_raw %>% arrange(rank)

    # Format the top contributors for display, using the rank from the function
    # This loop now iterates through the explicitly sorted top_data
    top_list_items <- map_chr(1:nrow(top_data), ~{
      paste0(
        top_data$rank[.x], ". ", # Use rank column
        top_data$country[.x], " (",
        scales::percent(top_data$avg_percentage[.x] / 100, accuracy = 0.1), ")"
      )
    })

    # Combine into a single string with line breaks
    top_list_string <- paste(top_list_items, collapse = "\n")

    # Create a single value box for OVERALL top countries
    value_box(
      title = paste("Historical Top", nrow(top_data), "Countries (Avg % contribution) for current filters"), # Updated title
      value = top_list_string,
      theme = "info", # Changed theme slightly
      showcase = bsicons::bs_icon("graph-up-arrow"), # Changed icon
      tooltip(
        bsicons::bs_icon("info-circle"),
        paste("Showing top", nrow(top_data), "countries based on overall average percentage across all years",
              "for", chemical_category, "chemicals",
              if(region_filter != "All") paste("within the", region_filter, "region(s).") else ".",
              "Year range slider is ignored for this box.") # Updated tooltip
      ),
      style = "white-space: pre-wrap;" # CSS to respect newlines
    )
  })
  # --- End Updated Value Box UI ---

  # ... (rest of server logic) ...

  output$countrycsPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Country participation in the CS")
    create_article_plot_simple(df, "Country participation in the CS", "% of New Substances", animate = FALSE)
  })

  output$articleGdpPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Annual growth rate of the GDP")
    create_article_plot_simple(df, "Annual growth rate of the GDP", "GDP Growth Rate (%)", animate = FALSE)
  })

  output$articleResearchersPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Number of Researchers")
    create_article_plot_simple(df, "Number of Researchers", "Researchers", animate = FALSE)
  })

  output$articleCsExpansionPlot <- renderPlotly({
    req(nrow(article_data) > 0)
    df <- article_data %>% filter(source == "Expansion of the CS")
    create_article_plot_simple(df, "Expansion of the CS", "Number of New Substances", animate = FALSE)
  })

  # --- NEW: Render Top Collaborations Plot ---
  output$articleTopCollabsPlot <- renderPlotly({
    # 1. Filter collaboration data for "All" chemicals
    collab_data_all_chem <- ds %>%
      filter(is_collab == TRUE, chemical == "All") %>%
      # Select necessary columns (use 'country' as it holds the collab string)
      select(year, percentage, country) %>%
      collect() # Collect this subset

    validate(need(nrow(collab_data_all_chem) > 0, "No collaboration data found for 'All' chemicals."))

    # 2. Calculate average percentage per collaboration
    avg_collabs <- collab_data_all_chem %>%
      group_by(country) %>%
      summarise(avg_percentage = mean(percentage, na.rm = TRUE)) %>%
      filter(!is.na(avg_percentage)) %>%
      arrange(desc(avg_percentage))

    validate(need(nrow(avg_collabs) > 0, "Could not calculate average collaboration percentages."))

    # 3. Get top 10 collaboration IDs
    top_10_collab_ids <- head(avg_collabs$country, 10)

    # 4. Filter the original collaboration data for these top 10
    top_10_data <- collab_data_all_chem %>%
      filter(country %in% top_10_collab_ids)

    # 5. Create the plot using the new function
    create_top_collabs_plot(top_10_data)
  })
  # --- End NEW Plot Rendering ---


} # End server

# Run the app
shinyApp(ui, server)
