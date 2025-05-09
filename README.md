## Chemical Space Explorer Shiny App



### **Technical Summary: Chemical Space Explorer Shiny App**

**1. Purpose & Overview:**
This Shiny application provides an interactive platform for exploring trends in chemical research contributions, focusing on data derived from scientific publications or patents. Users can visualize and compare the percentage contribution of individual countries or analyze joint collaborations between selected countries over time and across different chemical categories. The primary interface elements are an interactive world map for country selection, sidebar controls for filtering (year range, chemical category, region), a time-series plot displaying contribution trends, and a summary data table.

**2. Core Logic & User Interaction:**
*   **Selection:** Users select countries by clicking on them on the Leaflet map or by clicking buttons corresponding to top contributors (dynamically calculated based on filters). Selections can be cleared using a dedicated button.
*   **Filtering:** Users can filter the data globally using sidebar controls for:
    *   **Year Range:** A slider defines the time period of interest.
    *   **Chemical Category:** Radio buttons allow focusing on specific chemical fields (e.g., "Organic", "All").
    *   **Region:** A dropdown filters the countries displayed *on the map* and optionally the data used for calculating *top contributors* and *individual country comparisons*.
*   **Display Modes:** When multiple countries are selected, users can switch between:
    *   **Comparing Individual Contributions:** Shows separate trend lines for each selected country.
    *   **Finding Joint Collaborations:** Identifies and plots data points representing collaborations involving *all* selected countries.
*   **Outputs:** Based on selections and filters, the application reactively updates:
    *   The Leaflet map (highlighting selected countries, filtering visible countries by region).
    *   The Plotly time-series plot (showing contribution percentages over the selected years).
    *   A DT summary table (displaying aggregated statistics like average/max contribution).
    *   The list of top contributing countries in the sidebar.

**3. Data Handling & Processing:**
*   **Data Source:** The application reads data from a Parquet file (`data.parquet`). Parquet is a columnar storage format optimized for analytical queries.
*   **Engine:** The `duckplyr` package is used, which leverages the high-performance DuckDB in-process analytical data management system as a backend for `dplyr`-like operations.
*   **Loading:** Data is initially loaded using `duckplyr::read_parquet_duckdb()`. This creates a reference (a DuckDB relation) to the data without loading the entire dataset into R's memory immediately.
*   **Query Execution:** Filtering (`filter`), selection (`select`), and aggregation (`summarise`, `group_by`) operations are translated by `duckplyr` into SQL queries executed efficiently by DuckDB directly on the Parquet file or DuckDB's internal representation.
*   **Data Transfer (`collect()`):** Data is only transferred from DuckDB into an R data frame (using `collect()`) at the last possible moment – typically after filtering and aggregation have significantly reduced the data size. This minimizes R's memory footprint and leverages DuckDB's speed for the heavy lifting. This occurs primarily in `load_country_data` (for initial setup lists) and `get_display_data` / `calculate_top_contributors` after database-side processing.
*   **Collaboration Logic:** Identifying joint collaborations involves filtering for `is_collab == TRUE` and then iteratively applying `grepl` on the `iso2c` column within the DuckDB query. A final R-based check using `strsplit` and `all()` ensures that *all* selected countries are part of the collaboration identifier after collecting the potential matches.

**4. Performance & Optimization:**
*   **DuckDB Backend:** Using `duckplyr`/DuckDB is the primary optimization. It avoids loading large datasets into R memory and performs filtering/aggregation very efficiently.
*   **Reactive Caching:** Shiny's `bindCache()` function is used for key reactive expressions (`processed_data`, `top_contributors_data`). This memoizes the results based on their input values (selected countries, filters). If the inputs haven't changed, the cached result is returned instantly, avoiding expensive recalculations (like re-querying DuckDB or re-processing data in R).
*   **Targeted Data Collection:** As mentioned, `collect()` is used sparingly, only bringing necessary, pre-processed data into R for visualization or final R-specific manipulations.
*   **Efficient Map Updates:** `leafletProxy` is used to update map elements (like polygon colors or layer visibility) without redrawing the entire map base, leading to smoother interactions. Layer groups are used to efficiently show/hide countries based on the region filter.

**5. Summary for IT/PIs:**
The application is designed for efficient exploration of potentially large chemical contribution datasets. It leverages the DuckDB database via `duckplyr` to perform most data filtering and aggregation tasks outside of R's main memory, significantly improving performance and reducing resource requirements compared to loading the entire dataset into R. Reactive programming and caching further enhance responsiveness by avoiding redundant calculations. The user interface allows for intuitive selection and filtering, with results dynamically displayed on an interactive map, plot, and table.

---

### Optimizations

**How the New App is Smarter (and Faster):**

1.  **Smart Storage (Parquet):** The data is stored in a special format (Parquet) that's already organized for quick look-ups, like an encyclopedia with a really good index.

2.  **Super-Fast Assistant (DuckDB / `duckplyr`):**
    *   Instead of copying the whole encyclopedia, the new app uses a very fast "assistant" (DuckDB, accessed via the `duckplyr` package).
    *   When you change a filter (like the year slider or chemical category), the app doesn't grab all the data. It just tells the assistant: "Hey, go into the encyclopedia (Parquet file) and find *only* the pages about 'Organic Chemicals' between '2010 and 2020' for 'China'."
    *   The assistant is incredibly fast at finding *just* that specific information directly within the encyclopedia, without needing to copy everything first.

3.  **Only Getting What's Needed (`collect()`):**
    *   Once the assistant (DuckDB) has found the specific, small amount of data needed for the current plot or table, *only then* is that small piece brought over to the app's main workspace (`collect()`).
    *   This means the app only ever handles the small, relevant results, not the entire massive dataset.

4.  **Remembering Results (Caching / `bindCache`):**
    *   The app now has a "memory". If you select China with certain filters, it calculates the result and shows the plot.
    *   If you then select Germany, and later go *back* to selecting China with the *exact same filters* as before, the app remembers the result it calculated earlier.
    *   Instead of asking the assistant to look it up again, it just pulls the result from its memory instantly. This makes switching between selections or slightly tweaking filters much faster after the first time.

5.  **Waiting for You to Finish (Debouncing):**
    *   When you click on the map, the app used to immediately recalculate everything. If you clicked several countries quickly, it would try to do all the work multiple times unnecessarily.
    *   Now, when you click the map, it highlights the country instantly (so you see something happened), but it *waits* for one second. If you click another country within that second, it resets the timer. Only when you *pause* clicking for a second does it tell the assistant to go get the data for all the countries you selected.
    *   This prevents lots of wasted calculations while you're still deciding on your selection.

6.  **Organized Layout:** Instead of potentially many separate pages each trying to load data, the main interactive parts are consolidated. The map, filters, plots, and table work together more smoothly within tabs and cards.

**In Simple Terms:**

The old app was like photocopying the entire encyclopedia every time you had a question. The new app has a super-fast librarian (DuckDB) who looks up just the specific page you need in the original, well-indexed encyclopedia (Parquet) and only brings you that single page (`collect()`). It also remembers the answers to recent questions (caching) and waits for you to finish asking before running off to find the answer (debouncing).

This makes the new app feel much faster, handle large amounts of data without slowing down, and provide a much smoother experience.



### app overview

**UI (`app.R`)**

*   **Overall Structure:**
    *   `page_navbar` with `bs_theme(version = 5, bootswatch = "flatly")` provides a modern and clean look. `flatly` is a good choice for a data-centric application.
    *   `fillable = FALSE` is generally a safe default for complex layouts, preventing unexpected full-height behaviors unless specifically managed.
    *   The use of `nav_panel` for main sections and `nav_menu` for "Legal Info" and "Useful Links" effectively organizes the content and keeps the main navigation bar uncluttered.
    *   Icons (`bsicons::bs_icon`) enhance visual appeal and user guidance.
    *   `nav_spacer` is used well for visual separation.
*   **Explorer Tab:**
    *   **Article Highlights Section:**
        *   The `card` with `navset_card_tab` is a good way to present multiple static plots compactly.
        *   `height = "550px"` for the `navset_card_tab` and `height = "450px"` for the plots within provides consistent sizing.
        *   Tooltips on `nav_panel` titles are a nice touch for providing more context.
        *   Filters (`selectInput`, `radioButtons`) directly within the "Explore Top collaborators" `nav_panel` are well-scoped to that specific plot.
    *   **Interactive Exploration Section:**
        *   The `card` with `layout_sidebar` (sidebar on the right) for the map and its filters is a standard and effective layout.
        *   Filter controls (`uiOutput("region_filter_ui")`, `sliderInput`, `radioButtons`, `actionButton`) are well-placed in the sidebar.
        *   `leafletOutput` for the map is correctly placed as the main content of this `layout_sidebar`.
        *   `navset_card_pill` for "Trends," "Contribution Map," and "Data Table" is a good choice for switching between related views of the selected data.
*   **Responsiveness:**
    *   `bslib` (Bootstrap 5) is inherently responsive. Components like `card`, `layout_sidebar`, `navset_card_tab`, `navset_card_pill` generally adapt well to different screen sizes. The sidebar in `layout_sidebar` should stack vertically on smaller screens.
    *   Fixed heights for plots (e.g., `450px`) should be tested on smaller mobile screens. If they cause content to be cut off or require excessive scrolling *within the plot area itself*, consider relative units (like `vh`) or `bslib::heights` for more dynamic sizing, though fixed heights are often acceptable.
    *   The `iframe` for the PDF uses `height:80vh`, which is responsive.
*   **Tooltips:** Used effectively throughout the UI to provide additional information without cluttering the interface.
*   **Footer:** Simple and informative.

**Server Logic (`app.R`)**

*   **Reactivity & Performance:**
    *   `selected_countries_immediate` (reactiveVal) and `selected_countries <- debounce(selected_countries_immediate, 1000)` is an excellent pattern. It allows immediate UI feedback on the map while deferring more computationally intensive updates.
    *   `bindCache` on the `processed_data` reactive is a significant performance optimization, especially since this reactive depends on multiple inputs. The cache key appears comprehensive.
    *   `withProgress` is used during data processing, providing good user feedback.
    *   `req()` and `validate()` are used appropriately to handle missing inputs and prevent errors, offering informative messages to the user.
*   **Map Interaction:**
    *   The initial map is created by `create_selection_map`.
    *   Map clicks (`input$selection_map_shape_click`) efficiently update individual polygons using `leafletProxy` to `removeShape` and then `addPolygons`. This is more performant than redrawing many shapes.
    *   The "Clear Map Selection" button also uses `leafletProxy` effectively.
    *   Region filtering uses `showGroup` and `hideGroup` on the proxy, which is efficient.
*   **Data Handling:**
    *   `load_country_data()` (from functions.R) is called once at startup.
    *   The main `processed_data` reactive calls `get_display_data()`, which centralizes the core data filtering and transformation logic based on user inputs.
*   **Dynamic UI:**
    *   `renderUI` is used for `region_filter_ui`, `display_mode_ui`, and `plot_header_ui`, allowing parts of the UI to adapt to selections.

**Functions (`functions.R`)**

*   **`load_country_data`:**
    *   Uses `duckplyr::read_parquet_duckdb`, which is good for performance with Parquet files.
    *   `collect()` is generally used after initial filtering/distinct operations on the `duckdb` relation, which is efficient. The collected datasets (country_list, chemical_categories, article_data) are relatively small.
*   **`create_selection_map`:**
    *   Uses `rnaturalearth` for base map geometries.
    *   The logic for coloring selected vs. available countries is clear.
*   **`get_display_data`:**
    *   This is a critical function. It correctly filters data based on `is_collab`, `chemical`, `year_range`, and `selected_isos`.
    *   The collaboration filtering (looping `grepl` then a secondary `sapply` check post-collect) is a pragmatic approach. For a small number of `selected_isos` (typical for map selections), this should perform adequately.
*   **Plotting Functions (`create_main_plot`, `create_contribution_map_plot`, `create_article_plot_simple`, `create_top_collabs_plot`):**
    *   Follow a good pattern: prepare data, create a `ggplot` object, then convert to `plotly` or `girafe`.
    *   Conditional aesthetics and layouts based on `display_mode` are handled.
    *   `ggiraph` is used for interactive ggplot-based charts, and pure `plotly` for others, offering flexibility.
    *   Tooltips are customized for better user experience.
*   **Other Helpers:** Functions like `get_plot_header`, `create_summary_table`, `calculate_top_contributors` are well-defined and contribute to modularity.

**Potential Areas for Final Polish & Minor Suggestions:**

1.  **CSS & Mobile Refinements:**
    *   **Legends:** On very small mobile screens, plot legends (especially horizontal ones or those with many items) can sometimes become cramped or take up too much vertical space. Test this thoroughly. For `plotly` plots, you might adjust `legend = list(orientation = 'h', y = -0.2, yanchor = "top")` or similar. For `ggplot2` based plots, `legend.position = "bottom"` is generally robust.
    *   **Sidebar Filters:** Ensure all filter inputs in the map's sidebar are easily tappable and readable on mobile. `bslib` should handle stacking well.
    *   **Table Horizontal Scroll:** `DTOutput` with `scrollX = TRUE` is good. Ensure it's intuitive on mobile.
2.  **`theme_set(theme_light())` vs. `bs_theme()`:**
    *   You have `theme_set(theme_light())` globally and `bs_theme(bootswatch = "flatly")` in `page_navbar`. `bslib`'s theming will generally apply to `ggplot2` plots, potentially overriding `theme_light()`. This is usually desired for consistency. If you specifically want `theme_light()` for `ggplot2` plots, you might need to apply it explicitly within each plot function or explore `thematic::thematic_shiny()`. Given `flatly` is a light theme, the difference might be minimal.
3.  **`update_map_polygons` Function:**
    *   The `update_map_polygons` function in functions.R doesn't seem to be used in the app.R server logic (map updates are handled directly via `leafletProxy` in `observeEvent`s). If it's indeed unused, consider removing it to avoid confusion. The current direct proxy update method is efficient for click-based changes.
4.  **Tooltip for "Collaborations" Tab (Article Highlights):**
    *   The `nav_panel` title is "Explore Top collaborators", and the tooltip is "Explore top contributors and their collaboration trends...". This seems fine, especially since the content within can be toggled. If you wanted to be extremely precise, the tab title could be more generic like "Top Trends" if it frequently shows individual country data too. However, the current setup is understandable.
5.  **Error Handling in `create_selection_map`:**
    *   The `map_data` creation within `create_selection_map` has `cc = ifelse(is.na(cc) | cc == "", "#808080", cc)`. This fallback is good.
6.  **Clarity of `article_data` Loading:**
    *   In `load_country_data`, `article_data_raw` is created by selecting `year = year_x, country = country_x, value = percentage_x`. Ensure these `_x` suffixed columns are indeed what you intend for the static article plots.
    *   If the original data has many columns, consider using `select(-c(...))` to drop unnecessary columns after loading, rather than just selecting the few you need. This can help clarify intent and reduce memory usage.

#### dependencies

Here is a list of R package dependencies required to run your Shiny application.

**Core R Packages:**

*   `shiny`
*   `bslib`
*   `shinycssloaders`
*   `plotly`
*   `ggplot2`
*   `conflicted`
*   `duckplyr`
*   `dplyr`
*   `tidyr`
*   `purrr`
*   `scales`
*   `DT`
*   `stringr`
*   `leaflet`
*   `sf`
*   `ggiraph`
*   `rnaturalearth`
*   `rnaturalearthdata`
*   `leaflet.extras`
*   `RColorBrewer`

**Installation Command for R:**

```r
install.packages(c(
  "shiny", "bslib", "shinycssloaders", "plotly", "ggplot2",
  "conflicted", "duckplyr", "dplyr", "tidyr", "purrr", "scales",
  "DT", "stringr", "leaflet", "sf", "ggiraph", "rnaturalearth",
  "rnaturalearthdata", "leaflet.extras", "RColorBrewer"
))
```

**System Dependencies (Important for `sf`):**

The `sf` package often has system-level dependencies. On a Linux server, these typically include:

*   **GDAL (>= 2.2.3)**
*   **GEOS (>= 3.4.0)**
*   **PROJ (>= 4.9.3)**
*   **SQLite3**
*   **UDUNITS2** (sometimes needed indirectly)

The exact commands to install these system libraries vary by Linux distribution. For example, on Debian/Ubuntu:

```bash
sudo apt-get update
sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev
```

For RHEL/CentOS/Fedora:

```bash
sudo yum install -y gdal-devel geos-devel proj-devel sqlite-devel udunits2-devel
# or for newer Fedora/RHEL:
sudo dnf install -y gdal-devel geos-devel proj-devel sqlite-devel udunits2-devel
```

Ensure these system libraries are installed *before* trying to install the `sf` R package. If `sf` installation fails, it's almost always due to missing system dependencies.

**Other Considerations:**

*   **R Version:** Ensure a reasonably modern version of R is installed on the server (e.g., R >= 4.0.0 is a good baseline, though your packages might work on slightly older versions).
*   **DuckDB:** The `duckplyr` package interfaces with DuckDB. While `duckplyr` should handle its DuckDB component, it's worth noting. No separate DuckDB installation is usually required if using `duckplyr`.
*   **File Paths:** The application reads data from data.parquet and an article PDF from `"original_article.pdf"`. Ensure these files are present in the correct locations relative to the app.R file on the server.
*   **Permissions:** The R process running the Shiny app will need read access to the app files, data files, and write access to a temporary directory (usually handled by Shiny Server or Connect).

