#-------------------------V-Dem Data Pre-Processing-----------------------------
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

#------------------------------------------------------------------------------#
#--------------------------V-Dem Data Preparation-------------------------------
#------------------------------------------------------------------------------#

# Check if the V-Dem Data package is installed and load the data
if(!require(vdemdata)){
  remotes::install_github("vdeminstitute/vdemdata")
  data(vdem, package = "vdemdata")
} else {
  data(vdem, package = "vdemdata")
}

# Preparing a subset of the V-Dem Data
vdem_cp <- vdem %>%
  # Transmute and modify variables of interest
  transmute(
    # Panel identifiers
    across(c(country_name:year, starts_with("hist")), ~.x),
    # Generate an ISO code
    iso_code = countryname(
      sourcevar = country_name,
      destination = "iso3c"
    ),
    # Political-Geographic Region (10 Category)
    region_polgeo = factor(
      e_regionpol,
      levels = 1:10,
      labels = c(
        "Eastern Europe and Post-Soviet",
        "Latin America",
        "Middle East and North Africa",
        "Sub-Saharan Africa",
        "Western Europe and North America",
        "Eastern Asia",
        "South-Eastern Asia",
        "Southern Asia",
        "The Pacific",
        "The Caribbean"
      )
    ),
    # Generate year lag and lead variables
    across(
      year,
      list(
        lag = ~ .x + 1,
        lead = ~ .x - 1
      ),
      .names = "{.col}_{.fn}"
    ),
    # Whether a Country has a Legislature
    legislature = if_else(v2lgbicam %in% 1:2, 1, NA_real_),
    # % Female Legislators
    pct_female_leg = v2lgfemleg,
    # Election Year
    election_year = case_when(
      v2eltype_0 == 1 | v2eltype_1 == 1 | v2eltype_2 == 1 ~ 1,
      TRUE ~ 0
    ),
    # Year of the last election
    last_election = case_when(
      election_year == 1 ~ year,
      TRUE ~ NA_real_
    ),
    # Polyarchy Index
    polyarchy = v2x_polyarchy,
    # Liberal Democracy Index
    libdem = v2x_libdem,
    # Regimes of the World index
    regime_type = factor(
      v2x_regime,
      levels = 0:3,
      labels = c(
        "Closed Autocracy",
        "Electoral Autocracy",
        "Electoral Democracy",
        "Liberal Democracy"
      )
    )
  )

# Write the V-Dem data to a file
write_rds(
  x = vdem_cp, 
  file = "data/V-Dem/vdem_subset.rds",
  compress = "gz",
  compression = 9L
)
