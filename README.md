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



### **Required R Packages (Dependencies):**

Based on the `library()` calls in app.R, the following packages are needed:

*   `shiny` (Core web framework)
*   `bslib` (Bootstrap themes and UI components)
*   `shinycssloaders` (Loading spinners)
*   `plotly` (Interactive plots)
*   `ggplot2` (Plotting grammar - used by `plotly`)
*   `conflicted` (For managing function name conflicts)
*   `duckplyr` (DuckDB backend for dplyr)
*   `dplyr` (Data manipulation grammar)
*   `tidyr` (Data tidying utilities)
*   `purrr` (Functional programming tools)
*   `scales` (Graphical scales formatting - e.g., percentages)
*   `DT` (Interactive data tables)
*   `stringr` (String manipulation)
*   `leaflet` (Interactive maps)
*   `sf` (Simple Features for spatial data - used by leaflet/rnaturalearth)
*   `rnaturalearth` (World map vector data)
*   `leaflet.extras` (Extensions for Leaflet, including search)

The IT team should ensure these packages (and their own dependencies) are installed in the R environment where the app will run.