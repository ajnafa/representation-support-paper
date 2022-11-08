#-----------------------Tables: Descriptive Statistics--------------------------
#-Author: A. Jordan Nafa--------------------------------Created: March 1, 2022-#
#-R Version: 4.2.1-----------------------------------Revised: November 1, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "modelsummary",
  "kableExtra",
  install = FALSE
)

#------------------------------------------------------------------------------#
#--------------------Table 1A: Descriptive Statistics---------------------------
#------------------------------------------------------------------------------#

# Load the data
model_df <- read_rds("output/project-data/IVS_Model_Data_Full.rds")

# Pre-Processing the data
model_df_sub <- model_df %>%
  # Select a subset of the data for descriptive statistics
  select(
    project, churchill, sex, age_cat, educ, soc_pctfemleg, 
    soc_libdem, pctfemleg, gdp_pcap, libdem
  ) %>%
  # Convert factors to dichotmous to calculate proportions
  fastDummies::dummy_cols(
    select_columns = c("sex", "age_cat", "educ"),
    ignore_na = T
  ) %>% 
  # Transform data to match the model inputs
  mutate(across(matches("pctfemleg|gdp_pcap"), ~ .x/10))

# Construct a table for the descriptive statistics
descriptive_stats <- datasummary(
  churchill + sex_Male + sex_Female + `age_cat_16-34` + 
    `age_cat_35-54` + `age_cat_55 and Older` + `educ_High School or Less` +
    `educ_Associates or Technical Degree` + `educ_University Graduate` +
    soc_pctfemleg + soc_libdem + pctfemleg + gdp_pcap + libdem ~
    Mean + Median + SD + Min + Max + N,
  data = model_df_sub,
  output = "data.frame",
  add_columns = tibble(
    rownames = c(
      "Outcome: Support for Democracy",
      "Male",
      "Female",
      "16-34",
      "35-54",
      "55 and Older",
      "High School or Less",
      "Associates or Technical Degree",
      "University Graduate",
      "% Female Legislators (16-21)",
      "Liberal Democracy (16-21)",
      "% Female Legislators",
      "GDP Per Capita",
      "Liberal Democracy"
    )
  )) %>% 
  # Add number of groups for the random effect levels and total
  add_row(
    N = c(
      as.character(length(unique(model_df$country_jj))),
      as.character(length(unique(model_df$survey_tt))),
      as.character(length(unique(model_df$cohort_5y_kk))),
      as.character(nrow(model_df_sub)),
      as.character(nrow(model_df_sub)),
      as.character(nrow(model_df_sub))
    ),
    rownames = c(
      "Countries J",
      "Surveys T",
      "Birth Cohorts L",
      "Sample by Country",
      "Sample by Survey",
      "Sample by Cohorts"
    ),
    Min = c(NA_character_, "2", "11", "1409", "197", "27"),
    Max = c(NA_character_, "7", "21", "13719", "3997", "34058"),
    Median = c(NA_character_, "4", "19", "5337", "1236", "18041"),
    Mean = c(NA_character_, NA_character_, NA_character_, "5626", "1396", "16879")
    )

## Create the html table----
descriptive_stats_html <- descriptive_stats %>%
  # Reorder the variables
  select(rownames, Mean:N) %>%
  # Set the vartiable column as rownames
  column_to_rownames(var = "rownames") %>%
  # Generate a kable
  kbl(
    row.names = T,
    col.names = c("Mean", "Median", "SD", "Min", "Max", "N"),
    align = "lccccc",
    caption = "Table A1. Descriptive Statistics for the Main Analysis",
    booktabs = T,
    linesep = ""
  ) %>%
  # Apply HTML formatting
  kable_classic(html_font = "serif") %>%
  # Group Respondent level variable rows
  group_rows(index = c(
    "Respondent-Level Variables" = 9,
    "Socialization Variables" = 2,
    "Country-Level Variables" = 3,
    "Random Effects Levels" = 3,
    "Observations by Level"  = 3
  )) %>%
  # Group Respondent level variable rows
  group_rows(
    index = c(
      " " = 1, 
      "Sex" = 2, 
      "Age in Categories" = 3, 
      "Educational Attainment" = 4
    ),
    label_row_css = "padding-left: 1em;",
    indent = FALSE
  ) %>% 
  #
  row_spec(20, extra_css = "border-bottom: 1px solid;") %>% 
  #
  add_footnote(
    notation = "none",
    label = "Notes: Descriptive statistics are shown for the combined sample of countries that met the minimum criteria for inclusion discussed in the main text. For categorical predictors, the mean represents an unweighted proportion of responses for each category."
  )
  
## Create the latex table----
descriptive_stats_tex <- descriptive_stats %>%
  # Reorder the variables
  select(rownames, Mean:N) %>%
  # Set the vartiable column as rownames
  column_to_rownames(var = "rownames") %>%
  # Generate a kable
  kbl(
    row.names = T,
    format = "latex",
    col.names = c("Mean", "Median", "SD", "Min", "Max", "N"),
    align = "lccccc",
    caption = "Descriptive Statistics for the Main Analysis",
    booktabs = T,
    linesep = ""
  ) %>%
  # Group Respondent level variable rows
  group_rows(index = c(
    "Respondent-Level Variables" = 9,
    "Socialization Variables" = 2,
    "Country-Level Variables" = 3,
    "Random Effects Levels" = 3,
    "Observations Per Country"  = 1
  )) %>%
  # Group Respondent level variable rows
  group_rows(
    index = c(
      " " = 1, 
      "Sex" = 2, 
      "Age in Categories" = 3, 
      "Educational Attainment" = 4
    ),
    label_row_css = "padding-left: 1em;",
    indent = FALSE
  ) %>% 
  #
  row_spec(20, hline_after = TRUE)

