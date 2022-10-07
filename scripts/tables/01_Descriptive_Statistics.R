#-----------------------Tables: Descriptive Statistics--------------------------
#-Author: A. Jordan Nafa--------------------------------Created: March 1, 2022-#
#-R Version: 4.1.2--------------------------------------Revised: June 22, 2022-#

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
  ))

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
    caption = "Table 1A. Descriptive Statistics for the Main Analysis",
    booktabs = T,
    linesep = ""
  ) %>%
  # Apply HTML formatting
  kable_classic(html_font = "serif") %>% 
  # Group Respondent level variable rows
  group_rows(
    group_label = "Respondent-Level Variables",
    start_row = 1,
    end_row = 9
  ) %>%
  # Group Respondent sex variable rows
  group_rows(
    group_label = "Sex",
    label_row_css = "padding-left: 1em;",
    start_row = 2,
    end_row = 4,
    indent = FALSE
  ) %>%
  # Group Respondent level variable rows
  group_rows(
    group_label = "Age",
    label_row_css = "padding-left: 1em;",
    start_row = 4,
    end_row = 7,
    indent = FALSE
  ) %>%
  # Group Respondent level variable rows
  group_rows(
    group_label = "Educational Attainment",
    label_row_css = "padding-left: 1em;",
    start_row = 7,
    end_row = 9,
    indent = FALSE
  ) %>%
  # Group Respondent level variable rows
  group_rows(
    group_label = "Socialization Variables",
    label_row_css = "padding-left: 1em;",
    start_row = 10,
    end_row = 11
  ) %>%
  # Group country level variable rows
  group_rows(
    group_label = "Country-Level Variables",
    start_row = 12,
    end_row = 14
  )

