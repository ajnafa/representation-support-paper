#------------------------Model Data Preparation Script--------------------------
#-Author: A. Jordan Nafa------------------------------Created: August 25, 2021-#
#-R Version: 4.1.0---------------------------------------Revised: May 31, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "sjlabelled",
  "tidyverse",
  "data.table",
  "dtplyr",
  "easystats",
  "collapse",
  install = FALSE
)

#------------------------------------------------------------------------------#
#----------------------------Model Data Preparation-----------------------------
#------------------------------------------------------------------------------#

# Note: I recode a single respondent from the 1890-1899 cohort to 1900-1909
# to avoid model convergence issues resulting from the cluster only having a
# single observation

# Read in the project data list from the socialization script
model_data <- read_rds("data/IVS/IVS_Socialization_Democracies.rds") %>% 
  # Transmute the subset of the data for the main analysis
  transmute(
    # Country
    country_jj = country,
    # Survey Identifiers
    across(c(
      resp_id, year, project, 
      matches("window|region|vdem_iso")), 
      ~ .x),
    # Original Sample Size
    orig_samp = samp,
    # Country-Year
    country_year = str_c(country, year, sep = "-"),
    # Country-Survey-Year
    survey_tt = str_c(country, project, year, sep = "-"),
    # Five Year Birth Cohorts
    cohort_5y_kk = if_else(
      cohort_5y_kk == "1895-1899", 
      "1900-1904", 
      cohort_5y_kk
      ),
    # Ten Year Birth Cohorts
    cohort_10y_kk = if_else(
      cohort_10y_kk == "1890-1899", 
      "1900-1909", 
      cohort_10y_kk
      ),
    # Dependent Variable Items
    across(ends_with("pref"), ~ .x),
    # Support for Democracy
    support_democ = case_when(
      as.integer(democ_pref) %in% 4:5 ~ 1,
      as.integer(democ_pref) %in% 1:3 ~ 0
    ),
    # Rejection of Dictatorship
    reject_dict = case_when(
      as.integer(dictator_pref) %in% 1:2 ~ 1,
      as.integer(dictator_pref) %in% 3:5 ~ 0
    ),
    # Rejection of Military Rule
    reject_army = case_when(
      as.integer(army_pref) %in% 1:2 ~ 1,
      as.integer(army_pref) %in% 3:5 ~ 0
    ),
    # Rejection Non-Democratic Alternatives
    reject_nondem = case_when(
      reject_dict == 1 & reject_army == 1 ~ 1,
      reject_dict == 0 | reject_army == 0 ~ 0
    ),
    # Main Dependent Variable
    churchill = case_when(
      support_democ == 1 & reject_nondem == 1 ~ 1,
      support_democ == 0 | reject_nondem == 0 ~ 0
    ),
    # Respondent Sex
    sex,
    # Respondent Age in Years
    age,
    # Respondent Birth Year
    birth_year,
    # Recoding age as a categorical variable
    age_cat = factor(
      case_when(
        between(age, 15, 34) ~ 1,
        between(age, 35, 54) ~ 2,
        age >= 55 ~ 3
      ),
      levels = 1:3,
      labels = c("16-34", "35-54", "55 and Older")
    ),
    # Respondent educational attainment
    educ,
    # Female Legislative Representation (16-21)
    soc_pctfemleg = case_when(
      (birth_year + 16) >= min_legislature_5y ~ replace_na(soc_pctfemleg5, 0),
      TRUE ~ soc_pctfemleg5
    ),
    # Female Legislative Representation (11-21)
    soc_pctfemleg_10y = case_when(
      (birth_year + 11) >= min_legislature_10y ~ replace_na(soc_pctfemleg10, 0),
      TRUE ~ soc_pctfemleg10
    ),
    # Female Legislative Representation (Present)
    pctfemleg = pct_female_leg,
    # Liberal Democracy (16-21)
    soc_libdem = soc_libdem5*10,
    # Liberal Democracy (11-21)
    soc_libdem_10y = soc_libdem10*10,
    # Last Election Year
    last_election = last_election,
    # Liberal Democracy Index
    libdem = libdem*10,
    # GDP Per Capita (WB + CNTS)
    gdp_pcap = case_when(
      !is.na(gdp_pcap_curr_usd) ~ gdp_pcap_curr_usd/1000,
      !is.na(gdp_pcap_fct) ~ gdp_pcap_fct/1000
    ),
    # Current Regime Type
    regime_type = regime_type
  ) %>% 
  # Adding Variable Labels
  var_labels(
    country_jj = "Country Name",
    year = "Survey Year",
    project = "Survey Project",
    resp_id = "IVS Respondent Identifier",
    window_5y = "Five-Year Socialization Window",
    window_10y = "Ten-Year Socialization Window",
    vdem_iso = "V-Dem ISO3 Code",
    region = "Geo-Political Region",
    country_year = "Country-Year",
    orig_samp = "Sample Size",
    survey_tt = "Country-Project-Year",
    democ_pref = "Democracy Preference",
    dictator_pref = "Dictatorship Preference",
    army_pref = "Military Rule Preference",
    support_democ = "Respondent Supports Democracy",
    reject_dict = "Respondent Rejects Dictatorship",
    reject_army = "Respondent Rejects Military Rule",
    reject_nondem = "Respondent Rejects Non-Democratic Regimes",
    churchill = "Churchill Hypothesis",
    sex = "Respondent Sex (Ref = Male)",
    age = "Respondent Age in Years",
    educ = "Respondent Education (Ref = High School or Less)",
    soc_pctfemleg = "% Female Legislators (16-21)",
    soc_pctfemleg_10y = "% Female Legislators (11-21)",
    pctfemleg = "% Female Legislators (Present)",
    last_election = "Year of Last Election",
    libdem = "Liberal Democracy Index (Present)",
    soc_libdem = "Liberal Democracy Index (16-21)",
    soc_libdem_10y = "Liberal Democracy Index (11-21)",
    gdp_pcap = "GDP Per Capita (WB + CNTS)"
  )

#------------------------------------------------------------------------------#
#------------------------------Check Missingness--------------------------------
#------------------------------------------------------------------------------#

# Check missingness and question coverage
sample_countries <- model_data %>%
  # Group the data by country-year
  group_by(country_jj, country_year, project, orig_samp) %>%
  # Collapse the data by country
  summarise(
    across(
      dictator_pref:gdp_pcap,
      ~ (1 - (sum(case_when(!is.na(.x) ~ 1, TRUE ~ 0), na.rm = T) / n())) * 100
    )
  ) %>%
  # Ungroup the data
  ungroup() %>%
  # Set 100% missing to NA
  na_if(100) %>%
  # Drop missing data in the gdp variable
  drop_na(c("gdp_pcap", "pctfemleg")) %>%
  # Group the data by country
  group_by(country_jj) %>%
  # Calculate the number of waves per country
  mutate(
    # Country-Wave
    waves = 1:n(),
    # Total Waves per Country
    n_waves = n()
  ) %>%
  # Ungroup the data
  ungroup()

# Subset the cases in the main analysis
project_df <- model_data %>%
  # Filter countries in sample
  filter(country_year %in% sample_countries$country_year) %>%
  # Merge in the sample_countries data frame
  left_join(
    sample_countries %>% select(country_jj:project, waves:n_waves),
    by = c("country_jj", "country_year", "project")
  )

# Write the full including NAs data to a file, mostly for appendix tables
write_rds(
  x = project_df, 
  file = "output/project-data/IVS_Model_Data_Full.rds",
  compress = "gz",
  compression = 9L
)

#------------------------------------------------------------------------------#
#------------------------Main Data file for the Models--------------------------
#------------------------------------------------------------------------------#

# Create the data file to be used in the main analysis
model_df <- project_df %>%
  # Exclude missing values in data for key parameters, see appendix for discussion
  drop_na(matches("churchill|sex|age_cat|educ|soc_pctfemleg|soc_libdem")) %>% 
  # Group by country-project-year
  group_by(survey_tt) %>% 
  # Calculate a new sample size per survey variable
  mutate(final_samp = n()) %>% 
  # Ungroup the data
  ungroup()

# Write the data to a file
write_rds(
  x = model_df, 
  file = "output/project-data/IVS_Model_Data.rds",
  compress = "gz",
  compression = 9L
)
