# app.R
# 2025_04_04
# Codigo por Santiago Garcia Rios
# Checar paquetes necesarios

# core Shiny and UI  
library(shiny)  
library(bslib)  
library(shinycssloaders)  

# plotting  
library(plotly)  
library(ggplot2)  
library(RColorBrewer)  
library(mapproj)  

# data back‐end  
library(nanoarrow)        # your parquet reader  
library(arrow)            # for open_dataset + predicate pushdown  
library(dplyr)            # verbs and arrow integration  
library(tidyr)            # unnest  
library(purrr)            # map over list columns  
library(data.table)       # fast aggregations  
library(tidytable)        # optional alternative dplyr syntax  
library(countrycode)      # iso lookups  
library(glue)             # string glue  
library(scales)           # percent_format  

# reporting  
library(gt)  

theme_set(theme_light())

source("R/plot_function.R")

# Efficient data preparation using Arrow and dplyr
ds <- arrow::open_dataset("./data/data.parquet", format = "parquet")

# Load world map data once and collect immediately (more efficient)
# this is used for the map plot
world_data <- arrow::read_parquet("data/world_data.parquet") %>%
  as.data.frame()

# Precompute country list from individual data (fast metadata operation)
country_list <- ds %>%
  dplyr::filter(is_collab == FALSE) %>%
  dplyr::select(country, iso2c) %>%
  dplyr::distinct() %>%
  dplyr::collect() %>%
  dplyr::arrange(country) %>%
  data.table::as.data.table()


# Create a Shiny app object
ui <- page_navbar(
  id = "selected",
  selected = "National Trends 📈",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = NULL,
  navbar_options = navbar_options(collapsible = TRUE, underline = TRUE),
  sidebar = sidebar(
    title = "Country and Region Filters 🌍",
    width = "14rem",
    id = "sidebar",
    open = FALSE,
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
            "Trends in CS📈",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "China's Chemical Revolution: From 1996 to 2022, China surged to claim the chemical discoveries—far outpacing the US’s share—driven almost entirely by domestic research. In contrast, US solo contributions has steadily dropped, with rising international collaboration. Toggle between country-specific and collaboration plots to explore these dynamics.", # nolint: line_length_linter.
              placement = "left"
            ),
            h5("Countrywise expansion of the chemical space (CS)"),
            p("National Contributions to CS, which spans all chemicals and reactions reported in the literature."),
            withSpinner(plotlyOutput("trendPlot", height = "auto", width = "100%"), color = "#024173"),
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
              "Double click on the legend to filter isolate categories. Single click to isolate a category.",
              placement = "left"
            ),
            withSpinner(plotlyOutput("mapPlot", height = "auto", width = "100%"), color = "#024173"),
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
            "Trends in subspaces🧪",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Chemical Space was divided into three subspaces: Organic, Organometallic, and Rare-Earths. The plots show the distribution of these subspaces over time.",
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
            withSpinner(plotlyOutput("substancePlot", height = "auto", width = "100%"), color = "#024173"),
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
        card_header(
          div(
            class = "d-flex align-items-center",
            h4("Top Contributors 🏆", class = "mb-0 me-2")
          )
        ),
        card_body(
          gt_output("top_contributors_table")
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
            "Trends in CS📈",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Top 25 collaborations are available in this panel. For more details, please check the Explore All Collaborations 🤝 tab.",
              placement = "left"
            ),
            withSpinner(plotlyOutput("collabTrendPlot", height = "auto", width = "100%"), color = "#024173"),
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
          # nav_panel(
          #   "Map📌",
          #   tooltip(
          #     bsicons::bs_icon("question-circle"),
          #     "Double click on the legend to filter isolate categories. Single click to isolate a category.",
          #     placement = "left"
          #   ),
          #   withSpinner(plotlyOutput("mapPlotCollab"), color = "#024173"),
          #   card_footer(
          #     "Source: China's rise in the chemical space and the decline of US influence.",
          #     popover(
          #       a("Learn more", href = "#"),
          #       markdown(
          #         "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A., Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in the chemical space and the decline of US influence. Working Paper, Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
          #       )
          #     )
          #   )
          # ),
          nav_panel(
            "Trends in subspaces🧪",
            tooltip(
              bsicons::bs_icon("question-circle"),
              "Chemical Space was divided into three subspaces: Organic, Organometallic, and Rare-Earths. The plots show the distribution of these subspaces over time.",
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
            withSpinner(plotlyOutput("collabSubstancePlot", height = "auto", width = "100%"), color = "#024173"),
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
    "Explore All Collaborations 🤝",
    fluidPage(
      fluidRow(
        column(
          width = 8,
          selectizeInput(
            "country_select", "Select Country:",
            # selected = NULL,
            choices = setNames(country_list$iso2c, country_list$country),
            options = list(
              placeholder = "Search for a country and see its collaborations",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        ),
        column(
          width = 4,
          selectInput(
            "collab_filter", "Data Amount:",
            choices = c(
              "Top 10% (Fast)" = 0.10,
              "Top 50% (Medium)" = 0.5, 
              "Top 75% (Slower)" = 0.75,
              "All Data (Slowest)" = 1
            ),
            selected = 0.10
          ),
          conditionalPanel(
            condition = "input.collab_filter > 0.25",
            div(
              style = "color: #d9534f; font-size: 12px; margin-top: -15px;",
              icon("exclamation-triangle"), 
              "Showing more data will increase loading time"
            )
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel(
            "Explore All collaborations",
            # tooltip(
            #   bsicons::bs_icon("question-circle"),
            #   "China's Chemical Revolution: From 1996 to 2022, China surged to claim the chemical discoveries—far outpacing the US’s share—driven almost entirely by domestic research. In contrast, US solo contributions has steadily dropped, with rising international collaboration. Toggle between country-specific and collaboration plots to explore these dynamics.",
            #   placement = "left"
            # ),
            uiOutput("conditionalCollabPlot") %>% withSpinner(),
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
      # card(
      #   card_header(
      #     div(
      #       class = "d-flex align-items-center",
      #       h4("Top Historical Contributors 🏆", class = "mb-0 me-2")
      #     )
      #   ),
      #   card_body(
      #     popover(
      #     bsicons::bs_icon("info-circle"),
      #     a("About these percentages"),
      #     markdown("These percentages represent the total proportion of chemical substances discovered through collaboration between the selected country and each partner country. Higher percentages indicate stronger and more productive scientific partnerships in terms of novel chemical discoveries."),
      #     ),
      #     gt_output("top_contributors_tableB") %>% withSpinner()
      #   )
      # )
    )
  ),
  # ------------------------------,
  # 3) ARTICLE FIGURES,
  # ------------------------------,
   # ------------------------------,
  # 3) ARTICLE FIGURES,
  # ------------------------------,
  nav_panel(
    "Article Figures 📰",
    navset_card_tab(
      nav_panel(
        "Country Participation in CS",
        withSpinner(
          plotlyOutput("countryParticipationPlot", height = "600px", width = "100%"),
          color = "#024173"
        ),
        div(style = "height: 30px;"),
        hr(),
        withSpinner(gt_output("countryParticipationTable"), color = "#024173")
      ),
      nav_panel(
        "Researchers",
        withSpinner(
          plotlyOutput("researchersPlot", height = "600px", width = "100%"),
          color = "#024173"
        ),
        div(style = "height: 30px;"),
        withSpinner(gt_output("researchersTable"), color = "#024173")
      ),
      nav_panel(
        "GDP Growth",
        withSpinner(
          plotlyOutput("gdpGrowthPlot", height = "600px", width = "100%"),
          color = "#024173"
        ),
        div(style = "height: 30px;"),
        withSpinner(gt_output("gdpGrowthTable"), color = "#024173")
      ),
      nav_panel(
        "China and US in the CS",
        p(
          "Note that China-US collaboration represent percentage of new substances with participation of either China or USA, while the rest of data represent percentage of new substances that are reported in papers with no international collaboration."
        ),
        withSpinner(
          plotlyOutput("chinaUsInTheCS", height = "600px", width = "100%"),
          color = "#024173"
        )
        # div(style = "height: 30px;"),
        # withSpinner(gt_output("chinaUSTable"), color = "#024173")
      ),
      nav_panel(
        "CS Expansion",
        withSpinner(
          plotlyOutput("csExpansionPlot", height = "600px", width = "100%"),
          color = "#024173"
        ),
        div(style = "height: 30px;"),
        withSpinner(gt_output("csExpansionTable"), color = "#024173")
      ),
      # New About the Research panel
      nav_panel(
        "About the Research 🥼",
        fluidPage(
          fluidRow(
            column(
              width = 12,
              h3("China's rise in the chemical space and the decline of US influence"),
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
      ),
    nav_panel(
  "Legal Notice",
  fluidPage(
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
  )
),

# ------------------------------
# 5) PRIVACY POLICY,
# ------------------------------
nav_panel(
  "Privacy Policy",
  fluidPage(
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
  )
)
    ),
    card_footer(
      "Source: China's rise in the chemical space and the decline of US influence.",
      popover(
        a("Learn more", href = "#"),
        markdown(
          paste(
            "Preprint published in: [Bermúdez-Montaña, M., Garcia-Chung, A.,", 
            "Stadler, P. F., Jost, J., & Restrepo, G. (2025). China's rise in", 
            "the chemical space and the decline of US influence. Working Paper,", 
            "Version 1.](https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6)"
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
    } else if (active_tab() == "Explore All Collaborations 🤝") {
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


  # Add this new reactive to filter countries by region
  filtered_countries <- reactive({
    # Start with active dataset
    result <- active_dataset()
    
    # Apply region filter if set
    if (!is.null(input$region) && input$region != "All") {
      result <- result %>% dplyr::filter(region == input$region)
    }
    
    # Get distinct countries and sort them
    result %>%
      dplyr::distinct(country) %>%
      dplyr::collect() %>%
      dplyr::pull(country) %>%
      sort()
  }) %>% bindCache(active_tab(), input$region)

  # Modify this observer to use filtered_countries instead of all_countries
  observe({
    req(filtered_countries())
    req(top_countries())

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
      # Keep only selected countries that are still in the filtered list
      intersect(input$countries, filtered_countries())
    }

    # Store current region for comparison
    prev_region <<- input$region

    updateCheckboxGroupInput(session, "countries",
      choices = filtered_countries(),
      selected = selected_countries
    )

    # Reset the flag
    user_cleared <- FALSE
  }) %>% bindEvent(filtered_countries(), top_countries(), input$region)

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

  # Add this observer to handle tab switching
  observe({
    # When the active tab changes...
    if (active_tab() == "Collaboration Trends 🤝") {
      # Reset country selection when switching to Collaboration tab
      # This ensures we start fresh with appropriate countries for this view
      user_cleared <- FALSE
      
      # Auto-select top countries for the new tab
      top_countries_current_sel <- top_countries()
      updateCheckboxGroupInput(session, "countries", selected = top_countries_current_sel)
      
      # If a region was selected in the previous tab, maintain it if valid
      # Otherwise reset to "All"
      if (!is.null(input$region)) {
        existing_regions <- all_regions()
        if (!(input$region %in% existing_regions)) {
          updateSelectizeInput(session, "region", selected = "All")
        }
      }
    }
  }) %>% bindEvent(active_tab())

  observe({
    if (active_tab() == "National Trends 📈") {
      showNotification("Hover over the plot to see more details. I you don't see the plot, please select a country again from the sidebar.",
        type = "message",
        duration = 5
      )
    } else if (active_tab() == "Collaboration Trends 🤝") {
      showNotification("Please select Plot Top or Plot All from the sidebar to see the visualizations.",
        type = "error",
        duration = 4
      )
    } else if (active_tab() == "Explore All Collaborations 🤝") {
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
    if (active_tab() %in% c("Explore All Collaborations 🤝", "Article Figures 📰", "Know more about the research 🥼")) {
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
    req(top_contributors_by_year(), active_tab() == "National Trends 📈")
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
        contribution = "Contribution (%)"
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
      end_labels_data = end_labels_data,
      y_label = "% of New Substances contributed by collaborations",
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


  # output$mapPlotCollab <- renderPlotly({
  #   req(active_tab() == "Collaboration Trends 🤝")

  #   # Filter data for collaboration tab
  #   data <- filtered_data() %>%
  #     filter(chemical == "All")

  #   # IMPORTANT: Check if there's data after filtering
  #   req(nrow(data) > 0)

  #   # Process the collaboration data by splitting country pairs
  #   map_data <- data %>%
  #     # Step 1: Get yearly averages by collaboration pair
  #     dplyr::group_by(country, year) %>%
  #     dplyr::summarise(yearly_avg = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
  #     # Step 2: Split the country codes and expand the data
  #     mutate(
  #       country_parts = strsplit(as.character(country), ","),
  #       country1 = sapply(country_parts, function(x) if (length(x) > 0) x[1] else NA_character_),
  #       country2 = sapply(country_parts, function(x) if (length(x) > 1) x[2] else NA_character_)
  #     ) %>%
  #     # Filter out any rows where splitting didn't work properly
  #     filter(!is.na(country1), !is.na(country2)) %>%
  #     # Step 3: Create separate rows for each country in the pair
  #     tidyr::pivot_longer(
  #       cols = c(country1, country2),
  #       names_to = "country_position",
  #       values_to = "individual_country"
  #     ) %>%
  #     # Step 4: Group by individual country and year to get yearly stats
  #     dplyr::group_by(individual_country, year) %>%
  #     dplyr::summarise(
  #       value = sum(yearly_avg, na.rm = TRUE),
  #       # Store the specific collaboration pairs for this country by year
  #       collab_pairs = paste(sort(unique(country)), collapse = "; "),
  #       .groups = "drop"
  #     ) %>%
  #     # Step 5: Find best/worst years for each country with their values
  #     dplyr::group_by(individual_country) %>%
  #     dplyr::mutate(
  #       avg_value = mean(value, na.rm = TRUE),
  #       max_idx = which.max(value),
  #       min_idx = which.min(value),
  #     ) %>%
  #     dplyr::summarise(
  #       value = mean(value, na.rm = TRUE),
  #       best_year = year[max_idx][1], # Take first if multiple ties
  #       worst_year = year[min_idx][1],
  #       best_year_value = value[max_idx][1],
  #       worst_year_value = value[min_idx][1],
  #       # Create a formatted list of countries this country collaborates with
  #       collab_list = paste(unique(unlist(strsplit(
  #         paste(unique(collab_pairs), collapse = "; "),
  #         "; "
  #       ))), collapse = "; "),
  #       .groups = "drop"
  #     ) %>%
  #     # Step 6: Rename to match the expected format for the plot function
  #     rename(country = individual_country) %>%
  #     # Add a placeholder for region as it might be needed
  #     mutate(region = NA_character_)

  #   # Generate the map plot
  #   createCollabMapPlot(
  #     df = map_data,
  #     world_df = world_data,
  #     fill_var = "value",
  #     fill_label = "Average Collaboration Strength (%)",
  #     main_title = "International Research Collaboration Network"
  #   )
  # }) %>%
  #   bindCache(active_tab(), filtered_data())

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
      title = paste("Contribution to", input$chemicalSelector),
      x_label = ""
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
      title = paste("Contribution to", input$chemicalSelectorcollab),
      x_label = ""
    )
  }) %>%
    bindCache(active_tab(), input$chemicalSelectorcollab, filtered_data())


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flag buttons
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  # Improved loading indicator in conditionalCollabPlot
  output$conditionalCollabPlot <- renderUI({
    if (!is.null(input$country_select) && input$country_select != "") {
      tagList(
        div(
          class = "alert alert-info",
          style = "padding: 8px; margin-bottom: 10px;",
          icon("info-circle"), 
          "Tip: Start with '10% (Fast)' data setting and increase if needed. Loading 'All Data' may take much longer."
        ),
        withSpinner(
          plotlyOutput("collab_plot", width = "100%", height = "100%"),
          color = "#024173",
          type = 8,
          size = 1.2
        ),
        conditionalPanel(
          condition = "input.collab_filter >= 0.75",
          tags$div(
            style = "text-align: center; color: #d9534f; padding: 10px;",
            tags$span(icon("exclamation-circle")),
            "Loading large datasets... This may take some time."
          )
        )
      )
    } else {
      div(
        style = "text-align: center; margin-top: 100px; color: #666;",
        h4("Data ready. Please select a country to view visualizations."),
        icon("search")
      )
    }
  })
  # Reactive for filtered collaboration data
  # Reactive for filtered collaboration data

# Pre-aggregate collaborations in Arrow
preagg_collab <- reactive({
  req(active_tab() == "Explore All Collaborations 🤝")
  req(input$country_select != "")
  
  # This is the heavy lifting part - let Arrow do this work
  tryCatch({
    ds %>%
      dplyr::filter(
        is_collab == TRUE,
        grepl(input$country_select, iso2c)
      ) %>%
      dplyr::select(iso2c, year, percentage) %>%
      dplyr::collect()
  }, error = function(e) {
    warning("Error in preagg_collab: ", e$message)
    return(data.frame())
  })
}) 
# %>% bindCache(active_tab(), input$country_select)

  # Process the pre-aggregated data
  filtered_collab <- reactive({
  # Get base data from pre-aggregation
  base_data <- preagg_collab()
  
  # Validate filter value
  filter_value <- as.numeric(input$collab_filter)
  if(is.na(filter_value)) filter_value <- 0.10
  
  # Show a notification for larger data requests
  if (filter_value > 0.10) {
    showNotification(
      paste0("Loading ", filter_value*100, "% of data. Please wait..."),
      type = "warning", 
      duration = 3
    )
  }
  
  # Early exit if no data
  if (nrow(base_data) == 0) return(data.frame())
  
  # Ensure percentage is numeric
  base_data$percentage <- as.numeric(base_data$percentage)
  
  # Extract partner country codes and join with country names
  processed_data <- tryCatch({
    # Process in chunks if dataset is large
    if (nrow(base_data) > 10000) {
      chunk_size <- 5000
      chunks <- split(base_data, ceiling(seq_len(nrow(base_data))/chunk_size))
      result <- data.frame()
      
      for (chunk in chunks) {
        chunk_result <- chunk %>%
          mutate(
            partners = strsplit(as.character(iso2c), "-"),
            partner = purrr::map(partners, ~ setdiff(.x, input$country_select))
          ) %>%
          tidyr::unnest(partner) %>%
          left_join(country_list, by = c("partner" = "iso2c")) %>%
          rename(partner_country = country)
        
        result <- rbind(result, chunk_result)
      }
      result
    } else {
      base_data %>%
        mutate(
          partners = strsplit(as.character(iso2c), "-"),
          partner = purrr::map(partners, ~ setdiff(.x, input$country_select))
        ) %>%
        tidyr::unnest(partner) %>%
        left_join(country_list, by = c("partner" = "iso2c")) %>%
        rename(partner_country = country)
    }
  }, error = function(e) {
    warning("Error processing partner data: ", e$message)
    return(data.frame())
  })
  
  if (nrow(processed_data) == 0) return(data.frame())
  
  # Aggregate the data efficiently
  agg_data <- tryCatch({
    if (requireNamespace("data.table", quietly = TRUE)) {
      DT <- data.table::as.data.table(processed_data)
      data.table::setkey(DT, partner, partner_country, year)
      result <- DT[, .(total_percentage = sum(percentage, na.rm = TRUE)), 
                  by = .(partner, partner_country, year)]
      as.data.frame(result)
    } else {
      processed_data %>%
        group_by(partner, partner_country, year) %>%
        summarise(
          total_percentage = sum(percentage, na.rm = TRUE),
          .groups = "drop"
        )
    }
  }, error = function(e) {
    warning("Error aggregating data: ", e$message)
    return(data.frame())
  })
  
  if (nrow(agg_data) == 0) return(data.frame())
  
  # Apply filtering based on user selection
  if (filter_value < 1) {
    tryCatch({
      partner_totals <- agg_data %>%
        group_by(partner, partner_country) %>%
        summarise(total_contribution = sum(total_percentage, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_contribution))
      
      if(nrow(partner_totals) > 0 && is.numeric(partner_totals$total_contribution)) {
        cutoff_value <- ceiling(filter_value * nrow(partner_totals))
        significant_partners <- partner_totals %>%
          slice_head(n = cutoff_value) %>%
          pull(partner)
        
        agg_data <- agg_data %>%
          filter(partner %in% significant_partners)
      }
    }, error = function(e) {
      warning("Error filtering top contributors: ", e$message)
    })
  }
  
  # Ensure all numeric columns are actually numeric
  if(nrow(agg_data) > 0) {
    agg_data$total_percentage <- as.numeric(agg_data$total_percentage)
    agg_data$year <- as.numeric(agg_data$year)
  }
  
  return(agg_data)
}) %>% 
  bindCache(input$country_select, input$years, input$collab_filter) %>%
  debounce(500)
  # Collaboration plot with optimized rendering

  output$collab_plot <- renderPlotly({
    # Get the data and validate it
    data <- filtered_collab()
    validate(need(nrow(data) > 0, "No collaborations found..."))
    
    # Ensure we have the right types of data
    data$total_percentage <- as.numeric(data$total_percentage)
    data$year <- as.numeric(data$year)
    
    # Safely convert filter percentage
    filter_value <- as.numeric(input$collab_filter)
    if(is.na(filter_value)) filter_value <- 0.10
    filter_pct <- filter_value * 100
    
    # Get proper country name for display
    country_selected_name <- tryCatch({
      country_match <- match(input$country_select, country_list$iso2c)
      if(!is.na(country_match)) {
        country_list$country[country_match]
      } else {
        input$country_select
      }
    }, error = function(e) {
      input$country_select
    })
    
    # Count number of unique partner countries for subtitle
    partner_count <- length(unique(data$partner_country))
    
    # OPTIMIZATION 1: Limit points plotted based on data size
    max_points <- 1000
    if(nrow(data) > max_points) {
      # Sample data if too many points to display
      set.seed(123)  # For reproducible results
      data <- data %>% 
        group_by(partner_country) %>%
        slice_sample(prop = min(1, max_points/nrow(data))) %>%
        ungroup()
      sampled_note <- paste0(" (sampled ", nrow(data), " points)")
    } else {
      sampled_note <- ""
    }
    
    # OPTIMIZATION 2: Use direct plotly for large datasets instead of ggplotly conversion
    if(nrow(data) > 500) {
      # Direct plotly implementation for better performance with large datasets
      plot <- plot_ly(
        data = data,
        x = ~year,
        y = ~total_percentage,
        color = ~partner_country,
        type = "scattergl",
        mode = "markers",
        marker = list(
          size = ~total_percentage/max(total_percentage, na.rm=TRUE)*14 + 4,
          opacity = 0.6,
          line = list(width = 1, color = '#FFFFFF')
        ),
        text = ~paste0(
          "<b>", partner_country, "</b><br>",
          "<b>Year:</b> ", year, "<br>",
          "<b>Collaboration with ", country_selected_name, ":</b> ",
          scales::percent(total_percentage/100, accuracy = 0.01)
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = paste("Collaborations for", country_selected_name, sampled_note),
        xaxis = list(
          title = "Year", 
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "% of substances contributed by each collaboration",
          tickformat = ".1%",
          gridcolor = "#eeeeee"
        ),
        showlegend = TRUE,
        legend = list(
          font = list(size = 10),
          itemsizing = "constant"
        ),
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(family = "Arial", size = 12)
        )
      ) %>%
      config(displayModeBar = TRUE)
      
      return(plot)
    }
    
    # For smaller datasets, use ggplot with gradual enhancements
    tryCatch({
      plot <- ggplot(data, aes(x = year, y = total_percentage)) +
        geom_jitter(
          aes(
            size = total_percentage,
            color = partner_country,
            text = paste0(
              "<b>", partner_country, "</b><br>",
              "<b>Year:</b> ", year, "<br>",
              "<b>Collaboration with ", country_selected_name, ":</b> ",
              scales::percent(total_percentage/100, accuracy = 0.01)
            )
          ),
          alpha = 0.6,
          width = 0.5
        ) +
        scale_color_viridis_d(
          option = "turbo",
          name = "Partner Country",
          guide = guide_legend(
            override.aes = list(size = 3),
            ncol = if(partner_count > 20) 3 else 2
          )
        ) +
        scale_radius(range = c(0.5, 6), name = "") +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 0.01, scale = 1),
          expand = expansion(mult = c(0.05, 0.15))
        ) +
        scale_x_continuous(
          breaks = scales::pretty_breaks(n = 10)
        ) +
        labs(
          title = paste("Collaborations for", country_selected_name),
          subtitle = if(filter_pct < 100) {
            paste0("Showing top ", filter_pct, "% of collaborations (", partner_count, " countries)")
          } else {
            paste0("Showing all collaborations (", partner_count, " countries)")
          },
          x = "Year",
          y = "% of substances contributed by each collaboration"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8, color = "#666666"),
          legend.key.size = unit(0.5, "lines"),
          legend.text = element_text(size = 7)
        )
      
      # Convert to plotly with improved tooltip handling
      ggplotly(plot, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = "black",
            font = list(family = "Arial", size = 12)
          ),
          legend = list(
            font = list(size = 9),
            itemsizing = "constant"
          )
        ) %>%
        config(displayModeBar = TRUE)
    }, error = function(e) {
      # If plot generation fails, return a simple error message plot
      plot_ly() %>% 
        layout(
          title = "Error creating plot",
          annotations = list(
            x = 0.5,
            y = 0.5,
            text = paste("Could not generate plot:", e$message),
            showarrow = FALSE
          )
        )
    })
  })

# Top contributors table (historical) - optimized
# top_contributors <- reactive({
#   req(filtered_collab())
  
#   # Use direct data.table operations if available for better performance
#   if (requireNamespace("data.table", quietly = TRUE)) {
#     dt <- data.table::as.data.table(filtered_collab())
#     result <- dt[, .(total_contribution = sum(total_percentage, na.rm = TRUE)), 
#                 by = .(partner, partner_country)]
#     data.table::setorder(result, -total_contribution)
#     result <- head(result, 3)
#     return(as.data.frame(result[, .(partner, partner_country)]))
#   }
  
#   # Fall back to dplyr
#   filtered_collab() %>%
#     group_by(partner, partner_country) %>%
#     summarise(total_contribution = sum(total_percentage, na.rm = TRUE), .groups = "drop") %>%
#     arrange(desc(total_contribution)) %>%
#     slice_head(n = 3) %>%
#     select(partner, partner_country)
# }) 
# # %>% bindCache(input$country_select, input$years, input$collab_filter)

#   # Top Contributors Table B with improved error handling
#   output$top_contributors_tableB <- render_gt({
#     req(top_contributors())
#     req(nrow(top_contributors()) > 0)

#     top_contributors() %>%
#       gt() %>%
#       gt::tab_header(
#         title = "Top 3 Historical Contributors of Country selected",
#         subtitle = "All selected years considered for the Historical top contributors"
#       ) %>%
#       gt::fmt_flag(columns = partner) %>% # Mostrar banderas desde códigos ISO2
#       gt::cols_label(
#         partner = "", # Ocultar título de columna de banderas
#         partner_country = "Country"
#       ) %>%
#       gt::cols_width(
#         partner ~ px(50), # Ancho fijo para columna de bandera
#         partner_country ~ px(200)
#       ) %>%
#       gt::opt_row_striping() %>%
#       gt::tab_options(
#         table.font.size = "14px",
#         heading.title.font.size = "18px"
#       )
#   })


  #################
  # Article Figures
  ##################
  # app.R (server)
  # FLAG-BASED VISUALIZATIONS

  # GDP Growth Rate Plot
  output$gdpGrowthPlot <- renderPlotly({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Annual growth rate of the GDP")

    createArticleFlagPlot(
      data = article_data,
      source_title = "Annual growth rate of the GDP",
      y_title = "GDP per capita growth (annual %)",
      flag_size_range = c(1, 4)
    )
  })

  # Researchers Plot
  output$researchersPlot <- renderPlotly({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Number of Researchers")

    createArticleFlagPlot(
      data = article_data,
      source_title = "Number of Researchers",
      y_title = "Researchers",
      flag_size_range = c(0.4, 4)
    )
  })

  # Country Participation Plot
  output$countryParticipationPlot <- renderPlotly({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Country participation in the CS")

    createArticleDotPlot(
      data = article_data,
      source_title = "Country participation in the CS",
      y_title = "Number of new substances"
      # flag_size_range = c(1, 4)
    )
  })

  # DOT-BASED VISUALIZATIONS

  # CS Expansion Plot
  # CS Expansion Plot without animation and end annotations
  output$csExpansionPlot <- renderPlotly({
    req(active_tab() == "Article Figures 📰")
  
    # Get the data
    article_data <- figure_article() %>%
      dplyr::filter(source == "Expansion of the CS")
    
    # Prepare data for plotting
    plot_data <- article_data %>%
      dplyr::mutate(
        # Ensure percentage is treated as a count, not a percentage
        value = as.numeric(percentage),
        formatted_value = scales::comma(value),
        iso2c = countrycode::countrycode(country, "country.name", "iso2c", warn = FALSE)
      )
    
    # Define colors for countries
    country_colors <- c(
      "China" = "#c5051b",
      "China alone" = "#c56a75",
      "China w/o US" = "#9b2610",
      "France" = "#0a3161",
      "Germany" = "#000000", 
      "India" = "#ff671f",
      "Japan" = "#000091",
      "Russia" = "#d51e9b", 
      "USA alone" = "#3b5091",
      "USA w/o China" = "#006341",
      "United Kingdom" = "#74acdf",
      "United States" = "#002852",
      "All substances" = "#4879a7",
      "Organic Chemicals" = "#ff6d45",
      "Organometallics" = "#55713e",
      "Rare-Earths" = "#800525"
    )
    
    # Get available country colors and create mapping
    available_countries <- unique(plot_data$country)
    plot_colors <- country_colors[names(country_colors) %in% available_countries]
    
    # For any countries not in our predefined list, assign colors from a palette
    missing_countries <- setdiff(available_countries, names(country_colors))
    if (length(missing_countries) > 0) {
      extra_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(missing_countries))
      names(extra_colors) <- missing_countries
      plot_colors <- c(plot_colors, extra_colors)
    }
    
    # Create plot without animation and end annotations
    p <- plot_data %>%
      plot_ly() %>%
      add_trace(
        x = ~year, 
        y = ~value,
        color = ~country,
        colors = plot_colors,
        type = "scatter",
        mode = "lines+markers",
        marker = list(
          size = 8,
          opacity = 0.8,
          line = list(width = 1, color = '#FFFFFF')
        ),
        line = list(width = 2),
        text = ~paste(
          "<b>Country:</b> ", country,
          "<br><b>Year:</b> ", year,
          "<br><b>Value:</b> ", formatted_value
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = list(
          text = "",
          font = list(size = 18)
        ),
        xaxis = list(
          title = "Year", 
          gridcolor = "#eeeeee",
          range = c(min(plot_data$year) - 0.5, max(plot_data$year) + 0.5)
        ),
        yaxis = list(
          title = "Number of new substances",
          gridcolor = "#eeeeee",
          tickformat = ",.0f"
        ),
        plot_bgcolor = "rgb(250, 250, 250)",
        paper_bgcolor = "rgb(250, 250, 250)",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.15
        )
      ) %>%
      config(displayModeBar = TRUE)
    
    return(p)
  })


  # Add these after your existing plot renderers
# China-US in the CS Plot
output$chinaUsInTheCS <- renderPlotly({
  req(active_tab() == "Article Figures 📰")
  
  # Get the data
  article_data <- figure_article() %>%
    dplyr::filter(source == "China-US in the CS")
  
  # Define main countries and scaling factor for collaboration data
  main_countries <- c("China", "United States")
  SCALE_FACTOR <- 10  # Adjust to control visibility of collaboration data
  
  # Prepare data: adjust scales for plotting
  plot_data <- article_data %>%
    dplyr::mutate(
      # Convert main countries to decimal (e.g., 93.19 -> 0.9319)
      # Scale collaboration data for better visibility
      percentage = dplyr::if_else(
        country %in% main_countries,
        percentage / 100,  # Main data (as decimal)
        percentage * SCALE_FACTOR  # Collaboration data (scaled up)
      ),
      # Format hover text correctly for both groups
      formatted_value = dplyr::if_else(
        country %in% main_countries,
        scales::percent(percentage, accuracy = 0.1),  # Main: "93.2%"
        scales::percent(percentage / SCALE_FACTOR, accuracy = 0.01, scale = 1)  # Collab: "0.18%"
      ),
      # Handle country codes
      iso2c = dplyr::if_else(
        country %in% c("CN-US collab/CN", "CN-US collab/US"),
        NA_character_,
        countrycode::countrycode(country, "country.name", "iso2c", warn = FALSE)
      )
    )
  
  # Split data
  main_data <- plot_data %>% dplyr::filter(country %in% main_countries)
  collab_data <- plot_data %>% dplyr::filter(!country %in% main_countries)
  
  # Define colors
  country_colors <- c(
    "China" = "#c5051b",
    "United States" = "#0285d1",
    "CN-US collab/CN" = "#9b0b17",
    "CN-US collab/US" = "#a04981"
  )
  
  # Create plot
  p <- plot_ly() %>%
    # Main countries (left axis)
    add_trace(
      data = main_data,
      x = ~year,
      y = ~percentage,
      color = ~country,
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 12, opacity = 0.8, line = list(width = 1, color = '#FFFFFF')),
      line = list(width = 3),
      yaxis = "y1",
      text = ~paste(
        "<b>Country:</b> ", country,
        "<br><b>Year:</b> ", year,
        "<br><b>Value:</b> ", formatted_value
      ),
      hoverinfo = "text"
    ) %>%
    # Collaboration metrics (right axis)
    add_trace(
      data = collab_data,
      x = ~year,
      y = ~percentage,
      color = ~country,
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 10, opacity = 0.8, line = list(width = 1, color = '#FFFFFF')),
      line = list(width = 2, dash = "dot"),
      yaxis = "y2",
      text = ~paste(
        "<b>Collaboration:</b> ", country,
        "<br><b>Year:</b> ", year,
        "<br><b>Value:</b> ", formatted_value
      ),
      hoverinfo = "text"
    ) %>%
    layout(
      title = list(text = "", font = list(size = 18)),
      xaxis = list(title = "Year", gridcolor = "#eeeeee"),
      yaxis = list(
        title = "Own National Contribution (%)",
        tickformat = ".0%",
        range = c(0.6, 1.0),  # 60% to 100%
        gridcolor = "#eeeeee"
      ),
      yaxis2 = list(
        title = "China-US Collaboration (%)",
        overlaying = "y",
        side = "right",
        tickvals = seq(0, 2, by = 0.5),  # Scaled axis markers
        ticktext = c("0%", "0.05%", "0.10%", "0.15%", "0.20%"),  # Actual labels
        range = c(0, 2),  # Scaled range (0% to 0.2%)
        gridcolor = "#eeeeee"
      ),
      plot_bgcolor = "rgb(250, 250, 250)",
      legend = list(orientation = "h", y = -0.2),
      # Add annotations to explain the dual axis
      annotations = list(
        list(
          text = "Left axis: National contributions (60-100%)",
          x = 0.02,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 10, color = "#0285d1")
        ),
        list(
          text = "Right axis: Collaboration percentages (0-0.18%)",
          x = 0.98,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 10, color = "#9b0b17")
        )
      )
    ) %>%
    config(displayModeBar = TRUE)
  
  return(p)
})
  
  # GDP Growth Table
  # GDP Growth Table in wide format
  output$gdpGrowthTable <- render_gt({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Annual growth rate of the GDP") %>%
      dplyr::mutate(iso2c = countrycode::countrycode(country, "country.name", "iso2c")) %>%
      # Pivot to wide format: countries as rows, years as columns
      tidyr::pivot_wider(
        id_cols = c(iso2c, country),
        names_from = year,
        values_from = percentage
      ) %>%
      dplyr::arrange(country)

    # Get list of year columns for formatting
    year_cols <- names(article_data)[!names(article_data) %in% c("iso2c", "country")]

    # Create GT table with wide format
    tbl <- article_data %>%
      gt() %>%
      gt::fmt_flag(columns = iso2c) %>%
      # Format all year columns
      gt::fmt_number(columns = tidyselect::all_of(year_cols), decimals = 2) %>%
      gt::tab_header(
        title = "GDP Growth Rates by Country and Year",
        subtitle = "Annual percentage change in GDP per capita"
      ) %>%
      gt::cols_label(
        iso2c = "",
        country = "Country"
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) %>%
      # Add custom labels for specific years
      gt::cols_label(
        "2007" = gt::md("2007 **Global Financial Crisis**"),
        "2020" = gt::md("2020 **COVID**")
      )

    # Highlight negative growth rates in red
    for (year_col in year_cols) {
      tbl <- tbl %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#FFDDDD"),
            gt::cell_text(color = "#AA0000")
          ),
          locations = gt::cells_body(
            columns = tidyselect::all_of(year_col),
            rows = article_data[[year_col]] < 0
          )
        )
    }

    tbl %>% gt::opt_row_striping()
  })

  # Researchers Table in wide format
  output$researchersTable <- render_gt({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Number of Researchers") %>%
      dplyr::mutate(iso2c = countrycode::countrycode(country, "country.name", "iso2c")) %>%
      # Pivot to wide format
      tidyr::pivot_wider(
        id_cols = c(iso2c, country),
        names_from = year,
        values_from = percentage
      ) %>%
      dplyr::arrange(country)

    # Get list of year columns for formatting
    year_cols <- names(article_data)[!names(article_data) %in% c("iso2c", "country")]

    article_data %>%
      gt() %>%
      gt::fmt_flag(columns = iso2c) %>%
      gt::fmt_number(
        columns = tidyselect::all_of(year_cols),
        decimals = 1,
        scale_by = 1 / 1000,
        suffixing = c("k", "M", "B")
      ) %>%
      gt::tab_header(
        title = "Number of researchers in research and development activities",
        subtitle = "Number of Researchers by Country and Year"
      ) %>%
      gt::cols_label(
        iso2c = "",
        country = "Country"
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) %>%
      gt::opt_row_striping()
  })

  # Country Participation Table in wide format
  output$countryParticipationTable <- render_gt({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Country participation in the CS") %>%
      dplyr::mutate(iso2c = countrycode::countrycode(country, "country.name", "iso2c")) %>%
      # Pivot to wide format and fill missing values with 0
      tidyr::pivot_wider(
        id_cols = c(iso2c, country),
        names_from = year,
        values_from = percentage,
        values_fill = list(percentage = 0)
      ) %>%
      # Order alphabetically by country
      dplyr::arrange(country)

    # Identify year columns
    year_cols <- names(article_data)[!names(article_data) %in% c("iso2c", "country")]

    article_data %>%
      gt() %>%
      gt::fmt_flag(columns = iso2c) %>%
      # Format all year columns in percent format
      gt::fmt_percent(
        columns = tidyselect::all_of(year_cols),
        decimals = 0,
        scale_values = FALSE
      ) %>%
      gt::tab_header(
        title = "Country Participation in Chemical Space",
        subtitle = "Percentage of new substances contributed by country"
      ) %>%
      gt::cols_label(
        iso2c = "",
        country = "Country"
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) %>%
      gt::opt_row_striping()
  })

  # CS Expansion Table in wide format
  output$csExpansionTable <- render_gt({
    req(active_tab() == "Article Figures 📰")

    article_data <- figure_article() %>%
      dplyr::filter(source == "Expansion of the CS") %>%
      # Pivot to wide format
      tidyr::pivot_wider(
        id_cols = c(country),
        names_from = year,
        values_from = percentage
      ) %>%
      dplyr::arrange(country)

    # Get list of year columns for formatting
    year_cols <- names(article_data)[!names(article_data) %in% c("country")]

    article_data %>%
      gt() %>%
      gt::fmt_number(columns = tidyselect::all_of(year_cols), decimals = 0) %>%
      gt::tab_header(
        title = "Chemical Space Expansion Over Time",
        subtitle = "Growth in number of substances"
      ) %>%
      gt::cols_label(
        country = "Country"
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = list(gt::cell_fill(color = "#4879a7")),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(gt::cell_fill(color = "#ff6d45")),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(gt::cell_fill(color = "#55713e")),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::tab_style(
        style = list(gt::cell_fill(color = "#993751")),
        locations = gt::cells_body(rows = 4)
      ) %>%
      gt::opt_row_striping()
  })

# China-US Table in wide format
 
# output$chinaUSTable <- render_gt({
 
#   req(active_tab() == "Article Figures 📰")
 

 
#   article_data <- figure_article() %>%
 
#     dplyr::filter(source == "China-US in the CS") %>%
 
#     dplyr::mutate(iso2c = countrycode::countrycode(country, "country.name", "iso2c")) %>%
 
#     # Pivot to wide format
 
#     tidyr::pivot_wider(
 
#       id_cols = c(iso2c, country),
 
#       names_from = year,
 
#       values_from = percentage
 
#     ) %>%
 
#     dplyr::arrange(country)
 

 
#   # Get list of year columns for formatting
 
#   year_cols <- names(article_data)[!names(article_data) %in% c("iso2c", "country")]
 
#   article_data %>%
 
#     gt() %>%
 
#     gt::fmt_flag(columns = iso2c) %>%
 
#     # Use percentage formatting for this table
 
#     gt::fmt_percent(columns = tidyselect::all_of(year_cols), decimals = 1, scale = 0.01) %>%
 
#     gt::tab_header(
 
#       title = "China-US Contributions and Collaborations",
 
#       subtitle = "Percentage of national contribution"
 
#     ) %>%
 
#     gt::cols_label(
 
#       iso2c = "",
 
#       country = "Country/Metric"
 
#     ) %>%
 
#     gt::tab_style(
 
#       style = gt::cell_text(weight = "bold"),
 
#       locations = gt::cells_column_labels()
 
#     ) %>%
 
#     gt::opt_row_striping()
 
# })


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # End of server
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

shinyApp(ui, server)
