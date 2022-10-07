#--------------------------IVS Data Pre-Processing------------------------------
#-Author: A. Jordan Nafa----------------------------Created: September 9, 2021-#
#-R Version: 4.1.2--------------------------------------Revised: March 6, 2022-#

## Load the necessary libraries----
pacman::p_load(
  "sjlabelled",
  "tidyverse",
  "data.table",
  "dtplyr",
  "collapse",
  install = FALSE
)

#------------------------------------------------------------------------------#
#----------------------------IVS Data Preparation-------------------------------
#------------------------------------------------------------------------------#

## Load the WVS (1981-2020) trend file----
wvs <- read_rds("data/IVS/WVS_Trend_I-VII_(1981-2020).rds") %>%
  # Set all charactrers in names to uppercase
  rename_with(.fn = ~ str_to_upper(.x))

## Load the EVS (1981-2017) trend file----
evs <- read_rds("data/IVS/EVS_1981-2017.rds") %>%
  # Set all charactrers in names to uppercase
  rename_with(.fn = ~ str_to_upper(.x))

## Subset the variables from the WVS----
wvs <- wvs %>%
  # Intersect returns the names that appear in both x and y
  select(intersect(names(evs), names(.)))

## Subset the variables from the EVS----
evs <- evs %>%
  # Subset based on the wvs tibble
  select(intersect(names(wvs), names(.)))

## Construct the Integrated Values Study----
ivs <- wvs %>%
  # Append the EVS to the WVS
  bind_rows(evs) %>%
  # "Set Not asked", "Missing; Unknown", and "No answer" as NA
  ftransformv(
    is.double,
    recode_num,
    `-4` = NA, # Not asked
    `-5` = NA, # Missing; Unknown
    `-2` = NA # No answer
  ) %>%
  # Recode non-response in the education variables
  mutate(
    # Respondent's Highest Educational Attainment
    X025 = recode_num(
      X025,
      `-3` = 0, # Not applicable; No formal education
      `-1` = NA # Don't know
    ),
    # Respondent's Highest Educational Attainment
    X025A_01 = recode_num(X025A_01, `-1` = NA)
  ) %>%
  # Remove any columns with no non-missing values
  select(where(~ any(!is.na(.x)))) %>%
  # Drop the value labels that are not used
  drop_labels()

## Data Pre-Processing
ivs_df <- ivs %>%
  # Transmute a subset of the data
  transmute(
    # Survey Identifier
    project = factor(
      S001,
      levels = 1:2,
      labels = c("EVS", "WVS")
    ),
    # Country Name
    country = factor(
      S003,
      levels = get_values(S003),
      labels = get_labels(S003)
    ),
    # Survey Year
    year = as.numeric(S020),
    # Respondent Identifier
    resp_id = S007,
    # Survey Country-Year
    country_year = str_c(country, year, sep = "-"),
    # Respondent Gender
    sex = factor(
      X001,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    # Respondent Age
    age = recode_num(
      as.integer(X003),
      `-3` = NA, # Not applicable
      `-1` = NA # Don't know
    ),
    # Respondent's Birth Year
    birth_year = year - age,
    # Respondent's Highest Educational Attainment
    educ = factor(
      case_when(
        X025 %in% c(0:3, 5) | X025A_01 %in% 0:3 ~ 1,
        X025 %in% c(4, 6:7) | X025A_01 %in% 4:5 ~ 2,
        X025 %in% 8 | X025A_01 %in% 6:8 ~ 3
      ),
      levels = 1:3,
      labels = c(
        "High School or Less",
        "Associates or Technical Degree",
        "University Graduate"
      )
    ),
    # Regime Preference Variables
    across(
      E114:E117,
      ~ factor(
        case_when(
          .x == 1 ~ 4, # Very Good
          .x == 2 ~ 3, # Fairly Good
          .x == -1 ~ 2, # Don't know
          .x == 3 ~ 1, # Fairly Bad
          .x == 4 ~ 0 # Very Bad
        ),
        levels = 0:4,
        labels = c(
          "Very Bad",
          "Fairly Bad",
          "Don\'t Know",
          "Fairly Good",
          "Very Good"
        ))
    )
  ) %>%
  # Renaming select variables
  rename(
    dictator_pref = E114,
    techno_pref = E115,
    army_pref = E116,
    democ_pref = E117
  )

## Check missingness and question coverage----
sample_countries <- ivs_df %>%
  # Group the data by country-year
  group_by(country, country_year, project) %>%
  # Collapse the data by country
  summarise(
    samp = n(),
    across(
      sex:democ_pref,
      ~ (1 - (sum(if_else(!is.na(.x), 1, 0), na.rm = T) / n())) * 100
    )
  ) %>%
  # Ungroup the data
  ungroup() %>%
  # Set NA if Q was not asked
  na_if(100) %>%
  # Drop if key covariates were not available
  drop_na(matches("sex|age|educ|(r|c|y)_pref")) %>%
  # Select the identifier columns
  select(country:samp) %>%
  # Group the data by country
  group_by(country) %>%
  # Calculate the number of waves per country
  mutate(
    # Country-Wave
    waves = 1:n(),
    # Total Waves per Country
    n_waves = n()
  ) %>%
  # Ungroup the data
  ungroup() %>%
  # Keep countries with two or more waves
  filter(n_waves >= 2)

## Subset the IVS data for the analysis----
ivs_df_sub <- ivs_df %>%
  # Inner-Join the Sample Countries tibble
  inner_join(
    sample_countries,
    by = c("country", "country_year", "project")
  ) %>%
  # Generate a new country-project identifier
  mutate(project_country = str_c(country, project, sep = "-")) %>%
  # Drop unnused factor levels
  droplevels() %>%
  # Excluding countries with evidence of translation errors time-invariance
  filter(
    !country_year %in% c(
      "Albania-1998", "Cyprus-2019", "Nigeria-1995", "Colombia-1997"
      ) &
      !country %in% c(
        "India", "Indonesia", "Thailand", "Lebanon", "Malta",
        "Philippines", "Moldova", "Luxembourg", "Denmark"
      ) &
      !(country == "Romania" & (year < 2018 & project == "WVS"))
  )

# Write the IVS Model Data to a File
write_rds(
  x = ivs_df_sub, 
  file = "data/IVS/IVS_Subset.rds",
  compress = "gz",
  compression = 9L
  )
