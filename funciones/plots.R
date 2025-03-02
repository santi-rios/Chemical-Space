library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(dplyr)
library(countrycode)
library(leaflet)
library(highcharter)
library(viridisLite)
library(glue)
library(ggplot2)

# interactive data exploration
library(explore)

df <- arrow::read_parquet("./data/df_clean_merged_chem.parquet") |>
    dplyr::filter(is.na(value_supplements)) |>
    dplyr::filter(!source_supps %in% c("FigureS-4_organicKW", "FigureS-4_organoMetallicKW", "FigureS-4_inorganicKW", "FigureS-7a_b", "FigureS-7c", "FigureS-7d_e", "FigureS-7f_g"))

df <- as.data.frame(df)

df |> explore()

write.csv(df, "./data2/df_1_f1.csv", row.names = FALSE)

df <- read.csv("./data2/df_1_f1.csv")

str(df)

top_countries <- df |>
    dplyr::filter(country %in% c("China", "United States", "India", "Japan", "Germany", "Russia", "United Kingdom", "France"))

View(top_countries)

write.csv(top_countries, "./data2/df_1_f1_topcountries.csv", row.names = FALSE)

ggplot(top_countries, aes(x = year, y  = value_fig1)) +
    geom_line() +
    geom_point(aes(shape = substance)) +
    facet_wrap(~substance, scales = "free") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "Histogram of value by source_supps", x = "value", y = "count")
