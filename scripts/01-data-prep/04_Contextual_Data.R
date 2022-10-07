#-----------------------Contextual Data Pre-Processing--------------------------
#-Author: A. Jordan Nafa----------------------------Created: September 9, 2021-#
#-R Version: 4.1.2--------------------------------------Revised: March 6, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "sjlabelled",
  "tidyverse",
  "data.table",
  "dtplyr",
  "collapse",
  "countrycode",
  install = FALSE
)

# Read the IVS subset from script 01
ivs_df_sub <- read_rds("data/IVS/IVS_Subset.rds")

# Read the V-Dem subset from script 02
vdem_df <- read_rds("data/V-Dem/vdem_subset.rds")

# Read the WDI subset from script 03
wdi_df <- read_rds("data/WDI/wdi_subset.rds")

# Read the CNTS subset from script 03
cnts_df <- read_rds("data/CNTS/cnts_subset.rds")

#------------------------------------------------------------------------------#
#-------------------------Combining the Contextual Data-------------------------
#------------------------------------------------------------------------------#

# Generate a data frame from the IVS countries
context_ivs <- ivs_df_sub %>%
  # Keep distinct countries
  distinct(country) %>%
  # Generate a country iso identifier
  mutate(
    iso_code = countryname(
      sourcevar = country,
      destination = "iso3c"
    )
  ) %>%
  # Group the data by country and iso_code
  group_by(country, iso_code) %>%
  # Create an observation for each country from 1900 to 2020
  tidyr::expand(year = seq(1900, 2020, 1)) %>%
  # Ungroup the data
  ungroup() %>%
  # Recode non-continuous countries based on V-Dem historical trajectories
  mutate(
    vdem_iso = case_when(
      iso_code == "MKD" & year %in% 1912:1991 ~ "SRB",
      iso_code == "MNE" & year %in% 1916:1997 ~ "SRB",
      iso_code == "BIH" & year <= 1992 ~ "SRB",
      iso_code == "SVN" & year %in% 1918:1989 ~ "SRB",
      iso_code == "HRV" & year %in% c(1918:1991) ~ "SRB",
      iso_code %in% c("SVK", "CZE", "HRV") & year %in% 1900:1917 ~ "HUN",
      iso_code %in% c("POL", "EST", "LVA") & year %in% 1900:1918 ~ "RUS",
      iso_code == "SVK" & year %in% 1918:1991 ~ "CZE",
      iso_code == "LVA" & year %in% 1940:1990 ~ "RUS",
      iso_code %in% c("LTU", "EST") & year %in% 1944:1990 ~ "RUS",
      iso_code == "BGD" & year %in% 1947:1971 ~ "PAK",
      str_detect(iso_code, "ARM|AZE|BLR|GEO|KGZ|MDA|UKR") & 
        year %in% 1919:1990 ~ "RUS",
      country == "North Ireland" ~ "GBR",
      TRUE ~ iso_code
    )
  )

# Merge in the V-Dem and Economic Development Data
context_df <- context_ivs %>% 
  # Merge in the V-Dem Data
  left_join(
    vdem_df %>%
      select(
        iso_code,
        region = region_polgeo,
        year_lag,
        legislature:regime_type
      ),
    by = c("vdem_iso" = "iso_code", "year" = "year_lag")
  ) %>%
  # Merge in the CNTS data
  left_join(
    cnts_df[, 2:4],
    by = c("vdem_iso" = "iso_code", "year" = "year_lag")
  ) %>%
  # Merge in the world bank data
  left_join(
    wdi_df[, c(4, 7:8)],
    by = c("vdem_iso" = "iso3", "year" = "year_lag")
  )

# Write the merged contextual data to a file
write_rds(
  x = context_df, 
  file = "data/Context_Data.rds",
  compress = "gz",
  compression = 9L
)
