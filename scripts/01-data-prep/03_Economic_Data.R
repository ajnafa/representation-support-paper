#-------------------------Economic Development Data-----------------------------
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
  "WDI",
  install = FALSE
)

#------------------------------------------------------------------------------#
#-----------------------------CNTS Data Preparation-----------------------------
#------------------------------------------------------------------------------#

# Read in the CNTS Data File
cnts <- read_rds("data/CNTS/CNTS_2021.rds") %>%
  # Select a subset of the data
  transmute(
    # Country name
    country = case_when(
      country == "German FR" ~ "Germany",
      country == "Serbia/Montenegro" ~ "Serbia",
      country == "Yugoslavia" ~ "Serbia",
      TRUE ~ country
    ),
    # ISO3 Country Code
    iso_code = countryname(
      sourcevar = country,
      destination = "iso3c"
    ),
    # Year
    year_lag = year + 1,
    # Gross Domestic Product Per Capita (Factor Cost)
    gdp_pcap_fct = economics2
  )

# Write the CNTS subset to a file to a file
write_rds(
  x = cnts, 
  file = "data/CNTS/cnts_subset.rds",
  compress = "gz",
  compression = 9L
)

#------------------------------------------------------------------------------#
#------------------------------WDI Data Preparation-----------------------------
#------------------------------------------------------------------------------#

# Pull data from the world bank api
wdi <- WDI(
  country = "all",
  indicator = c(
    gdp_pcap_2010_usd = toupper("ny.gdp.pcap.kd"),
    gdp_pcap_curr_usd = toupper("ny.gdp.pcap.cd")
  ),
  start = 1960,
  end = 2020
) %>%
  # Generate country identifiers for merging
  mutate(
    # ISO3 Code
    iso3 = countrycode(
      origin = "iso2c",
      sourcevar = iso2c,
      destination = "iso3c"
    ),
    # V-Dem Identifier
    vdem_id = countrycode(
      origin = "iso3c",
      sourcevar = iso3,
      destination = "vdem"
    ),
    .before = 3,
  ) %>%
  # Drop countries without V-Dem identifiers
  drop_na(vdem_id)

# Construct a balanced panel and interpolate missing values
wdi_df <- wdi %>% 
  # Expand each country for the period 1960-2020
  expand(country, year = seq(1960, 2020, 1)) %>% 
  # Merge in WDI panel
  left_join(wdi, by = c("country", "year")) %>%
  # Group the data by country
  group_by(country) %>%
  # Use linear interpolation to fill the missing values between years
  mutate(
    across(
      starts_with("gdp_pcap"),
      ~ zoo::na.approx(.x, na.rm = FALSE, yleft = -666, yright = -666)
    ),
    .before = 5
  ) %>%
  # Ungroup the data
  ungroup() %>%
  # Replace -666 values with NA
  na_if(-666) %>%
  # Create a t + 1 identifier column
  mutate(year_lag = year + 1)

# Write the CNTS subset to a file to a file
write_rds(
  x = wdi_df, 
  file = "data/WDI/wdi_subset.rds",
  compress = "gz",
  compression = 9L
)
