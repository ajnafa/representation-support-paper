#-------Model Diagnostics: Societal Growth Curve Models (Socialization)---------
#-Author: A. Jordan Nafa--------------------------------Created: March 1, 2022-#
#-R Version: 4.2.1------------------------------------Revised: October 6, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "brms",
  "bayestestR",
  "tidybayes",
  "bayesplot",
  "patchwork",
  install = FALSE
)

# Load the socialization models for the main analysis
soc_models_main <- map(
  .x = list.files(
    soc_models_dir, 
    pattern = ".*_Final.rds",
    full.names = TRUE
  ),
  ~ read_rds(.x)
)

#------------------------------------------------------------------------------#
#-----------------------------Model Diagnostics---------------------------------
#------------------------------------------------------------------------------#

# Set the bayesplot color scheme
color_scheme_set("viridis")

### Create the paths to save the R-hat plots to----
soc_rhat_files <- str_c(
  "Rhats_SGC_HLogit_Full_M",
  0:7,
  "_Socialization_Final.jpeg"
)

### Generate plots for the R-hat diagnostics for each parameter----
rhats_socialization <- map2(
  .x = soc_models_main, 
  .y = str_c("Socialization Model M", 0:7), 
  #  Extract the rhat values for each parameter
  ~ rhat(.x) %>% 
    # Plot the distribution of the r-hat values
    mcmc_rhat_hist() +
    # Add a title to the plot
    labs(title = parse(text = paste("bold('Gelman-Rubin '*hat(R)*' Diagnostic for", .y, "')"))) +
    # Apply custom plot theme settings
    plot_theme(
      title_size = 24,
      plot.margin = margin(5, 5, 5, 5, "mm")
    ) +
    # Adjust the breaks on the x axis
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
)

# Print the R-hat plots
# map(rhats_socialization, ~ print(.x))

# Save the generated plot object as a .jpeg file
map2(
  .x = soc_rhat_files,
  .y = rhats_socialization,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "socializaton/rhats/"),
    width = 12,
    height = 8,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the plots to Effective Sample Size----
soc_neff_files <- str_c(
  "NEFF_SGC_HLogit_Full_M",
  0:7,
  "_Socialization_Final.jpeg"
)

### Generate plots for the N/EFF diagnostics for each parameter----
neff_socialization <- map2(
  .x = soc_models_main, 
  .y = str_c("Socialization Model M", 0:7),
  # Extract the effective sample size ratios for each parameter
  ~ neff_ratio(.x) %>% 
    # Plot the neff values for each parameter
    mcmc_neff_hist(binwidth = 0.05) +
    # Add a title to the plot
    labs(title = "Effective Sample Size Ratios for ", .y) +
    # Apply custom plot theme settings
    plot_theme(
      title_size = 24,
      plot.margin = margin(5, 5, 5, 5, "mm")
    ) +
    # Adjust the breaks on the x axis
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
)

# Print the N/EFF plots
# map(neff_socialization, ~ print(.x))

# Save the generated plot object as a .jpeg file
map2(
  .x = soc_neff_files,
  .y = neff_socialization,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "socializaton/neff/"),
    width = 12,
    height = 8,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the NUTS Diagnostics plots to----
soc_nuts_files <- str_c(
  "NUTS_Energy_SGC_HLogit_Full_M",
  0:7,
  "_Socialization_Final.jpeg"
)

### Generate plots for the NUTS diagnostics for each parameter----
nuts_socialization <- map2(
  .x = soc_models_main,
  .y = str_c("Socialization Model M", 0:7),
  # Extract the NUTS Diagnostics
  ~ nuts_params(.x) %>% 
    # Create a chain ID to facet by
    mutate(Chain = str_c("Chain", Chain, sep = " ")) %>% 
    # NUTS Energy Diagnostic Plot
    mcmc_nuts_energy(., binwidth = 1, alpha = 0.1) +
    # Add a title to the plot
    labs(title = str_c("No U-Turn Sampler Energy Diagnostic for ", .y)) +
    # Apply custom plot theme settings
    plot_theme(
      title_size = 24,
      plot.margin = margin(5, 5, 5, 5, "mm"),
      strip_size = 14
    )
)

# Print the NUTS Energy plots
# map(nuts_socialization, ~ print(.x))

# Save the generated plot object as a .jpeg file
map2(
  .x = soc_nuts_files,
  .y = nuts_socialization,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "socializaton/nuts/"),
    width = 14,
    height = 8,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the trace plots to----
soc_trace_files <- str_c(
  "Trace_SGC_HLogit_Full_M",
  0:7,
  "_Socialization_Final.jpeg"
)

### Set model parameter names for the facets----
math_labels_socialization <- as_labeller(
  x = c(
    "b_Intercept" = "bold('Population-Level Intercept, '*alpha[0])",
    "b_female_wi" = "bold('Sex: Female, '*beta[1])",
    "b_age_cat1" = "bold('Age: 35-54, '*beta[2])",
    "b_age_cat2" = "bold('Age: 55 and Older, '*beta[3])",
    "b_educ1" = "bold('Education: Associates or Technical Degree, '*beta[4])",
    "b_educ2" = "bold('Education: University Graduate, '*beta[5])",
    "b_soc_libdem_wi" = "bold('Liberal Democracy (16-21), '*gamma[2])",
    "b_soc_pctfemleg_wi" = "bold('% Female Legislators (16-21), '*gamma[1])",
    "b_female_wi:soc_libdem_wi" = "bold('Sex ' %*% ' Liberal Democracy (16-21), '*delta[2])",
    "b_female_wi:soc_pctfemleg_wi" = "bold('Sex ' %*% ' % Female Legislators (16-21), '*delta[1])",
    "b_soc_pctfemleg_be" = "bold('% Female Legislators (16-21), '*omega[1])",
    "b_soc_libdem_be" = "bold('Liberal Democracy (16-21), '*omega[2])",
    "b_female_wi:soc_pctfemleg_be" = "bold('Sex ' %*% ' % Female Legislators (16-21), '*upsilon[1])",
    "b_female_wi:soc_libdem_be" = "bold('Sex ' %*% ' Liberal Democracy (16-21), '*upsilon[2])",
    'sd_survey_tt__Intercept' = "bold('Survey Intercept SD, '*sigma[alpha[t]])",
    'sd_survey_tt__female_wi' = "bold('Survey Respondent Sex Slope SD, '*sigma[beta[t1]])",
    "cor_survey_tt__Intercept__female_wi" = "bold('Survey Intercept-Respondent Sex Slope Correlations, '*rho[alpha[t]*beta[1*t]])",
    'sd_country_jj__Intercept' = "bold('Country Intercept SD, '*sigma[alpha[j]])",
    'sd_country_jj__female_wi' = "bold('Country Respondent Sex Slope SD, '*sigma[beta[j]])",
    "cor_country_jj__Intercept__female_wi" = "bold('Country Intercept-Respondent Sex Slope Correlations, '*rho[alpha[j]*beta[1*j]])",
    "sd_cohort_5y_kk__Intercept" = "bold('Cohort Intercept SD, '*sigma[alpha[l]])"
  ), 
  default = label_parsed
)

### Generate trace plots for the key parameters in each model----
trace_socialization <- map2(
  .x = soc_models_main, 
  .y = str_c("Socialization Model M", 0:7),
  # Create trace plots faceted by parameter for each model
  ~ mcmc_trace(
    .x, 
    regex_pars =  "^b_|^sd_|^cor_",
    facet_args = list(scales = "free_y", labeller = math_labels_socialization)
  ) +
    # Add labels to the plot
    labs(
      y = "", 
      x = "Iteration",
      title = str_c("MCMC Trace Plots for ", .y),
      caption = "Each chain was run for 8,000 iterations with the first 3,000 discarded after the intitial warmup adaptation stage"
    ) +
    # Apply custom plot theme settings
    plot_theme(
      title_size = 24,
      plot.margin = margin(5, 5, 5, 5, "mm"),
      strip_size = 12
    ) +
    # Setting the parameters for the plot legend
    guides(color = guide_legend(
      title = "Chain",
      override.aes = list(
        size = 4,
        alpha = 1
      )
    ))
)

# Print the trace plots
# map(trace_socialization, ~ print(.x))

# Save the generated plot objects as a .jpeg file
map2(
  .x = soc_trace_files,
  .y = trace_socialization,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "socializaton/traceplots/"),
    width = 27,
    height = 12,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the rank overlay plots to----
soc_rank_files <- str_c(
  "Rank_SGC_HLogit_Full_M",
  0:7,
  "_Socialization_Final.jpeg"
)

### Rank plots for the key model parameters----
rank_socialization <- map2(
  .x = soc_models_main, 
  .y = str_c("Socialization Model M", 0:7), 
  # Create rank plots faceted by parameter for each model
  ~ mcmc_rank_overlay(
    .x, 
    regex_pars =  "^b_|^sd_|^cor_",
    facet_args = list(scales = "free_y", labeller = math_labels_socialization)
  ) +
    # Add labels to the plot
    labs(
      y = "", 
      x = "Rank", 
      title = str_c("MCMC Rank Plots for ", .y),
      caption = "Each chain was run for 8,000 iterations with the first 3,000 discarded after the intitial warmup adaptation stage"
    ) +
    # Apply custom plot theme settings
    plot_theme(
      title_size = 24,
      plot.margin = margin(5, 5, 5, 5, "mm"),
      strip_size = 12
    ) +
    # Setting the parameters for the plot legend
    guides(color = guide_legend(
      title = "Chain",
      override.aes = list(
        size = 4,
        alpha = 1
      )
    ))
)

# Print the rank plots
# map(rank_socialization, ~ print(.x))

# Save the generated plot objects as a .jpeg file
map2(
  .x = soc_rank_files,
  .y = rank_socialization,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "socializaton/rankplots/"),
    width = 27,
    height = 12,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the combined diagnostic plots to----
soc_diagplot_files <- str_c(
  "Diagnostics_SGC_HLogit_Full_M",
  0:7,
  "_Socialization_Final.jpeg"
)

# Combining the diagnostic plots into a single one via patchwork----
diag_plots_socialization <- map(
  .x = seq_along(soc_models_main),
  ~ (
    rhats_socialization[[.x]] + 
      ggtitle(parse(text = "bold('Gelman-Rubin '*hat(R)*' Diagnostic')")) + 
      neff_socialization[[.x]] + 
      ggtitle("Effective Sample Size Ratios") +
      nuts_socialization[[.x]] + 
      ggtitle("No U-Turn Sampler Energy Diagnostic")) / trace_socialization[[.x]] + 
    ggtitle("MCMC Trace Plots") + plot_layout(widths = c(1, 1), heights = c(1, 2)) & 
    plot_theme(
      title_size = 24,
      strip_size = 14,
      plot.margin =  margin(5, 5, 5, 5, "mm")
    )
)

# Save the generated plot objects as a .jpeg file
map2(
  .x = soc_diagplot_files,
  .y = diag_plots_socialization,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "socializaton/"),
    width = 32,
    height = 20,
    units = "in",
    dpi = "retina",
    type = "cairo",
    limitsize = F
  )
)