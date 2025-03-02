library("gapminder")
library("tidyverse")

gap_with_colors <-
  data.frame(gapminder,
    cc = I(country_colors[match(
      gapminder$country,
      names(country_colors)
    )]),
    cont_cc = I(continent_colors[match(
      gapminder$continent,
      names(continent_colors)
    )])
    )

glimpse(gap_with_colors)

write.csv(gap_with_colors, "gap_with_colors.csv", row.names = FALSE)

gap_with_new_colors <-
  gap_with_colors %>%
  mutate(
    cc = case_when(
    country == "United States" ~ "#0a3161",
    country == "China" ~ "#c5051b",
    country == "India" ~ "#ff671f",
    country == "Germany" ~ "#000000",
    country == "Japan" ~ "#995162",
    country == "France" ~ "#000091",
    country == "Mexico" ~ "#006341",
    country == "Russia" ~ "#d51e9b",
    country == "United Kingdom" ~ "#3b5091",
    country == "Colombia" ~ "#fcd116",
    country == "Brazil" ~ "#009b3a",
    country == "Ecuador" ~ "#ffdd00",
    country == "Peru" ~ "#d52b1e",
    country == "Chile" ~ "#0033a0",
    country == "Argentina" ~ "#74acdf",
    country == "Australia" ~ "#ffcc00",
    TRUE ~ cc
    )
  )

glimpse(gap_with_new_colors)

write.csv(gap_with_new_colors, "gap_with_new_colors.csv", row.names = FALSE)

gap_with_flags_unicode <- gap_with_new_colors %>%
    mutate(
        flag = countrycode::countrycode(
            country,
            origin = "country.name",
            destination = "unicode.symbol"
        )
    )

glimpse(gap_with_flags_unicode)

write.csv(gap_with_flags_unicode, "gap_with_flags_unicode.csv", row.names = FALSE)

sample_n(gap_with_flags_unicode, 10)

gap_with_flags_unicode_clean <- gap_with_flags_unicode %>%
    select(country, year, lifeExp, pop, gdpPercap, cc, cont_cc, flag) %>%
    filter(year > 1995)

glimpse(gap_with_flags_unicode_clean)
write.csv(gap_with_flags_unicode_clean, "gap_with_flags_unicode_clean.csv", row.names = FALSE)


library("arrow")

df <- arrow::read_parquet("./data/df_clean_coords.parquet")

str(df)
str(gap_with_flags_unicode)

df_clean <- df %>%
    select(-polity_iv) %>%
    filter(year > 1995) %>%
    left_join(gap_with_flags_unicode_clean, by = c("country", "year"))

dim(df_clean)
str(df_clean)
glimpse(df_clean)
write_parquet(df_clean, "df_clean_with_flags.parquet")

source_supps_vec <- df_clean$source_supps
print(unique(source_supps_vec))
unique(df_clean$source_fig1)
unique(df_clean$iso3c)


# https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/refs/heads/main/data/collabs_expanded.csv
collabs <- read.csv("https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/refs/heads/main/data/collabs_sample.csv")

str(collabs)

collab_clean <- collabs %>%
    janitor::clean_names() %>%
    filter(year > 1996) %>%
    rename(
        value_collab = "percentage",
        chemical = "source",
        iso3c_collab = "iso3c",
        iso2c_collab = "iso2c",
        total_colab = "total_value"
    ) %>%
    mutate(
        compaund = case_when(
            chemical == "absolute_counting_colabs_totalSpace" ~ "All",
            chemical == "absolute_counting_colabs_organic" ~ "Organic",
            chemical == "absolute_counting_colabs_metalOrganic" ~ "Metal-Organic",
            chemical == "absolute_counting_colabs_raras" ~ "Rare-Earths"
        )
    )

arrow::write_parquet(collab_clean, "collaborations.parquet")

str(collab_clean)

# Correct the coordinates for iso2c == "US"
df_clean_US <- df_clean %>%
  mutate(
    lat = ifelse(iso2c == "US", lng, lat),  # Swap lat and lng for 'US'
    lng = ifelse(iso2c == "US", lat, lng)   # Swap lat and lng for 'US'
  )

write_parquet(df_clean_US, "df_clean_with_flags_US.parquet")

# Step 2: Extract a new dataframe with unique iso2c, lat, and lng
df_coordinates <- df_clean %>%
  select(iso2c, lat, lng) %>%
  distinct()

write.csv(df_coordinates, "df_coordinates.csv", row.names = FALSE)

coords <- read.csv("df_coordinates_good.csv")

head(coords)

coords_flag <- coords %>%
    mutate(
        flag = countrycode::countrycode(
            iso2c,
            origin = "iso2c",
            destination = "unicode.symbol"
        )
    )

str(coords_flag)
View(coords_flag)

str(df_clean)

unique(df_clean$iso2c)
unique(coords_flag$iso2c)

df_clean_merged <- df_clean %>%
    select(-lat, -lng, -flag) %>%
    left_join(coords_flag, by = "iso2c")

str(df_clean_merged)
write_parquet(df_clean_merged, "df_clean_merged.parquet")


str(collab_clean)
str(coords_flag)

collab_clean_merged <- collab_clean %>%
      select(-value, -chemical, -total_colab) %>%
      rename(
        country = collab_group,
        iso2c = iso2c_collab,
        value_fig1 = value_collab,
        year = year
      ) %>%
      # Join with coordinates (preprocess collaboration centroids)
      mutate(iso_list = strsplit(iso2c, "-")) %>%
      tidyr::unnest(iso_list) %>%
      left_join(coords_flag, by = c("iso_list" = "iso2c"))

str(collab_clean_merged)
write_parquet(collab_clean_merged, "collab_clean_merged.parquet")
head(collab_clean_merged)

head(df_clean_merged)


chemicals_df <- read.csv("https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/refs/heads/main/data/countries_with_chemicals_joined.csv")

str(chemicals_df)

chemicals_df_clean <- chemicals_df %>%
    janitor::clean_names() %>%
    filter(year > 1995) %>%
    rename(
        value_fig1 = "value_x",
        source_fig1 = "source_x",
        substance_value = "value_y"
    ) %>%
    select(country, year, value_fig1, substance_value, iso3c, substance)

View(chemicals_df_clean)
str(chemicals_df_clean)
str(df_clean_merged)

df_clean_merged_chem <- df_clean_merged %>%
    left_join(chemicals_df_clean, by = c("country", "year", "value_fig1", "iso3c"))

str(df_clean_merged_chem)
write_parquet(df_clean_merged_chem, "df_clean_merged_chem.parquet")


head(df_clean_merged_chem)
tail(df_clean_merged_chem)


library(dplyr)
library(tidyr)

# Fill constant columns within each country group
df_filled <- df_clean_merged_chem %>%
  group_by(country) %>%
  fill(flag, cc, cont_cc, lat, lng, .direction = "downup") %>%
  ungroup() %>%
  # Arrange by year to ensure proper time order
  arrange(country, year) %>%
  # Fill time-dependent columns within each country group
  group_by(country) %>%
  fill(gdpPercap, pop, lifeExp, .direction = "downup") %>%
  ungroup()


str(df_filled)
head(df_filled)
tail(df_filled)
sample(df_filled, 10)
unique(df_filled$source_fig1)

write_parquet(df_filled, "df_filled.parquet")

# A lookup for more readable source labels:
source_mapping <- c(
  "Figure-12-a_b"                = "Country Participation in the Growth of the Chemical Space",
  "Figure1-d"                    = "% of the Annual Growth Rate of the GDP Per Capita",
  # "absolute_counting_colabs_biochem" = "Biochemistry",
  "Figure1-e"                    = "Number of Researchers in R&D" # just an example if you have duplicates
)

glimpse(df_filled)
