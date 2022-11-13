#------------------------Model Averaged Results Tables--------------------------
#-Author: A. Jordan Nafa----------------------------Created: November 10, 2022-#
#-R Version: 4.2.1----------------------------Last Modified: November 13, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "brms",
  "arrow",
  "modelsummary",
  "kableExtra",
  install = FALSE
)

# Use the custom glance functions
options(modelsummary_get = "broom")

#------------------------------------------------------------------------------#
#----------------------Load the Model Averaged Posteriors-----------------------
#------------------------------------------------------------------------------#

# Load the socialization models for the main analysis
soc_mod_avg <- map(
  .x = list.files(
    "output/fits/posteriors/", 
    pattern = "model_averaged_soc.*.gz.parquet",
    full.names = TRUE
  ),
  ~ read_parquet(.x)
)

# Assign names to the list
names(soc_mod_avg) <- c("Inter", "Inter_Alt", "Main", "Main_Alt")

# Load the contemporary models for the main analysis
contemp_mod_avg <- map(
  .x = list.files(
    "output/fits/posteriors/", 
    pattern = "model_averaged_cont.*.gz.parquet",
    full.names = TRUE
  ),
  ~ read_parquet(.x)
)

# Assign names to the list
names(contemp_mod_avg) <- c("Inter", "Inter_Alt", "Main", "Main_Alt")

# Build a tibble with the information for each group
glance_info <- tibble(
  cohort_5y_kk = 21,
  country_jj = 63,
  survey_tt = 254,
  n = 354458
)

#------------------------------------------------------------------------------#
#---------------Model Averaged Posterior Tables, Main Analysis------------------
#------------------------------------------------------------------------------#

# Build a list of modelsummary lists for the model averaged posteriors
bma_summaries_main <- map(
  .x = list(
    soc_mod_avg$Main,
    soc_mod_avg$Inter,
    contemp_mod_avg$Main,
    contemp_mod_avg$Inter
  ),
  ~ model_averaged_summary(.x, glance_info)
)

# Force round estimates and confidence intervals to two digits, match terms
for (i in seq_along(bma_summaries_main)) {
  bma_summaries_main[[i]]$tidy <- bma_summaries_main[[i]]$tidy %>% 
    mutate(
      across(c(estimate, conf.low, conf.high), ~ round(.x, 2)),
      term = str_remove_all(term, "soc_")
      )
}

## Write the output to a file
write_rds(
  bma_summaries_main, 
  "output/tables/model_averaged_main_table_list.rds",
  compress = "gz",
  compression = 9
)

# Build the Model summary data frame
bma_summaries_main_df <- modelsummary(
  bma_summaries_main,
  output = "data.frame",
  statistic = "conf.int",
  fmt = 2,
  coef_map = c(
    "b_Intercept" = "Intercept, $\\alpha_{0}$",
    "b_female_wi" = "Sex: Female, $\\beta_{1_{tj}}$",
    "b_age_cat1" = "Age: 35-54, $\\beta_{2}$",
    "b_age_cat2" = "Age: 55 and Older, $\\beta_{3}$",
    "b_educ1" = "Education: Associates or Technical Degree, $\\beta_{4}$",
    "b_educ2" = "Education: University Graduate, $\\beta_{5}$",
    "b_pctfemleg_wi" = "\\% Female Legislators, $\\gamma_{2}$",
    "b_female_wi:pctfemleg_wi" = "Sex $\\times$ \\% Female Legislators, $\\delta_{2}$",
    "b_pctfemleg_be" = "\\% Female Legislators, $\\omega_{2}$",
    "b_female_wi:pctfemleg_be" = "Sex $\\times$ \\% Female Legislators, $\\upsilon_{2}$",
    "sd_survey_tt__Intercept" = "Survey Intercepts SD, $\\sigma_{\\alpha_{t}}$",
    "sd_survey_tt__female_wi" = "Survey-Sex Slopes SD, $\\sigma_{\\beta_{1t}}$",
    'sd_country_jj__Intercept' = "Country Intercepts SD, $\\sigma_{\\alpha_{j}}$",
    'sd_country_jj__female_wi' = "Country-Sex Slopes SD, $\\sigma_{\\beta_{1j}}$",
    "sd_cohort_5y_kk__Intercept" = "Cohort Intercepts SD, $\\sigma_{\\alpha_{l}}$",
    "cor_survey_tt__Intercept__female_wi" = "Survey Intercept-Slope Correlations, $\\rho_{\\alpha_{t}\\beta_{1t}}$",
    "cor_country_jj__Intercept__female_wi" = "Country Intercept-Slope Correlations, $\\rho_{\\alpha_{j}\\beta_{1j}}$"
  ),
  escape = F,
  gof_map = tribble(
    ~ raw, ~ clean, ~ fmt,
    "n", "N", 0,
    "survey_tt", "Surveys $T$", 0,
    "country_jj", "Countries $J$", 0,
    "cohort_5y_kk", "Cohorts $L$", 0
  )) %>% 
  mutate(term = case_when(
    statistic == "conf.int" ~ NA_character_,
    TRUE ~ term
  )) %>% 
  # Select a subset of the data
  select(-c(statistic, part))

# Generate a kable for the MS Word manuscript table
kbl(
  bma_summaries_main_df,
  row.names = F,
  format = "latex",
  col.names = c("", "Main", "Interaction", "Main", "Interaction"),
  align = "lcc",
  caption = "Bayesian Model Averaged Multilevel Logit Analysis of Support for Democracy",
  booktabs = TRUE,
  escape = FALSE,
  linesep = ""
) %>%
  # Add a header indicating the model set
  add_header_above(header = c(
    " " = 1, 
    "Socialization" = 2,
    "Contemporary" = 2
  )) %>% 
  # Group rows for Population-Level Effects
  group_rows(
    index = c(
      "Population-Level Effects" = 12, 
      "Survey-Level Effects (Longitudinal)" = 4,
      "Country-Level Effects (Cross-Sectional)" = 4,
      "Variance Components" = 10,
      "Random Effects Correlations" = 4
    ),
    indent = FALSE
  ) %>% 
  # Add the bottom Border
  row_spec(34, hline_after = TRUE)

#------------------------------------------------------------------------------#
#---------------Model Averaged Posterior Tables, Main Analysis------------------
#------------------------------------------------------------------------------#

# Build a list of modelsummary lists for the model averaged posteriors
bma_summaries_alt <- map(
  .x = list(
    soc_mod_avg$Main_Alt,
    soc_mod_avg$Inter_Alt,
    contemp_mod_avg$Main_Alt,
    contemp_mod_avg$Inter_Alt
  ),
  ~ model_averaged_summary(.x, glance_info)
)

# Force round estimates and confidence intervals to two digits, match terms
for (i in seq_along(bma_summaries_alt)) {
  bma_summaries_alt[[i]]$tidy <- bma_summaries_alt[[i]]$tidy %>% 
    mutate(
      across(c(estimate, conf.low, conf.high), ~ round(.x, 2)),
      term = str_remove_all(term, "soc_")
    )
}

## Write the output to a file
write_rds(
  bma_summaries_alt, 
  "output/tables/model_averaged_alt_table_list.rds",
  compress = "gz",
  compression = 9
)

# Build the Model summary data frame
bma_summaries_alt_df <- modelsummary(
  bma_summaries_alt,
  output = "data.frame",
  statistic = "conf.int",
  fmt = 2,
  coef_map = c(
    "b_Intercept" = "Intercept, $\\alpha_{0}$",
    "b_female_wi" = "Sex: Female, $\\beta_{1_{tj}}$",
    "b_age_cat1" = "Age: 35-54, $\\beta_{2}$",
    "b_age_cat2" = "Age: 55 and Older, $\\beta_{3}$",
    "b_educ1" = "Education: Associates or Technical Degree, $\\beta_{4}$",
    "b_educ2" = "Education: University Graduate, $\\beta_{5}$",
    "b_pctfemleg_wi" = "\\% Female Legislators, $\\gamma_{2}$",
    "b_female_wi:pctfemleg_wi" = "Sex $\\times$ \\% Female Legislators, $\\delta_{2}$",
    "b_pctfemleg_be" = "\\% Female Legislators, $\\omega_{2}$",
    "b_female_wi:pctfemleg_be" = "Sex $\\times$ \\% Female Legislators, $\\upsilon_{2}$",
    "sd_survey_tt__Intercept" = "Survey Intercepts SD, $\\sigma_{\\alpha_{t}}$",
    "sd_survey_tt__female_wi" = "Survey-Sex Slopes SD, $\\sigma_{\\beta_{1t}}$",
    'sd_country_jj__Intercept' = "Country Intercepts SD, $\\sigma_{\\alpha_{j}}$",
    'sd_country_jj__female_wi' = "Country-Sex Slopes SD, $\\sigma_{\\beta_{1j}}$",
    "sd_cohort_5y_kk__Intercept" = "Cohort Intercepts SD, $\\sigma_{\\alpha_{l}}$",
    "cor_survey_tt__Intercept__female_wi" = "Survey Intercept-Slope Correlations, $\\rho_{\\alpha_{t}\\beta_{1t}}$",
    "cor_country_jj__Intercept__female_wi" = "Country Intercept-Slope Correlations, $\\rho_{\\alpha_{j}\\beta_{1j}}$"
  ),
  escape = F,
  gof_map = tribble(
    ~ raw, ~ clean, ~ fmt,
    "n", "N", 0,
    "survey_tt", "Surveys $T$", 0,
    "country_jj", "Countries $J$", 0,
    "cohort_5y_kk", "Cohorts $L$", 0
  )) %>% 
  mutate(term = case_when(
    statistic == "conf.int" ~ NA_character_,
    TRUE ~ term
  )) %>% 
  # Select a subset of the data
  select(-c(statistic, part))

# Generate a kable for the MS Word manuscript table
kbl(
  bma_summaries_main_df,
  row.names = F,
  format = "latex",
  col.names = c("", "Main", "Interaction", "Main", "Interaction"),
  align = "lcc",
  caption = "Bayesian Model Averaged Multilevel Logit Analysis of Support for Democracy",
  booktabs = TRUE,
  escape = FALSE,
  linesep = ""
) %>%
  # Add a header indicating the model set
  add_header_above(header = c(
    " " = 1, 
    "Socialization" = 2,
    "Contemporary" = 2
  )) %>% 
  # Group rows for Population-Level Effects
  group_rows(
    index = c(
      "Population-Level Effects" = 12, 
      "Survey-Level Effects (Longitudinal)" = 4,
      "Country-Level Effects (Cross-Sectional)" = 4,
      "Variance Components" = 10,
      "Random Effects Correlations" = 4
    ),
    indent = FALSE
  ) %>% 
  # Add the bottom Border
  row_spec(34, hline_after = TRUE)
