#--------Model Diagnostics: Societal Growth Curve Models (Contemporary)---------
#-Author: A. Jordan Nafa--------------------------------Created: March 1, 2022-#
#-R Version: 4.2.1------------------------------------Revised: October 6, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse",
  "arrow",
  "brms",
  "bayestestR",
  "tidybayes",
  "bayesplot",
  "patchwork",
  "palettetown",
  install = FALSE
)

# Load the contemporary models for the main analysis
contemp_models_alt <- map(
  .x = list.files(
    contemp_models_dir, 
    pattern = ".*_AltPrior.rds",
    full.names = TRUE
  ),
  ~ read_rds(.x)
)

#------------------------------------------------------------------------------#
#-----------------------------Model Diagnostics---------------------------------
#------------------------------------------------------------------------------#

# Set the bayesplot color scheme
color_scheme_set(pokepal(245, spread = 6))

### Create the paths to save the R-hat plots to----
alt_contemp_rhat_files <- str_c(
  "Rhats_SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior.jpeg"
)

### Generate plots for the R-hat diagnostics for each parameter----
alt_rhats_contemp <- map2(
  .x = contemp_models_alt, 
  .y = str_c("Contemporary Model M", 0:10), 
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
# map(alt_rhats_contemp, ~ print(.x))

# Save the generated plot object as a .jpeg file
map2(
  .x = alt_contemp_rhat_files,
  .y = alt_rhats_contemp,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "contemporary/rhats/"),
    width = 12,
    height = 8,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the plots to Effective Sample Size----
alt_contemp_neff_files <- str_c(
  "NEFF_SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior.jpeg"
)

### Generate plots for the N/EFF diagnostics for each parameter----
alt_neff_contemp <- map2(
  .x = contemp_models_alt, 
  .y = str_c("Contemporary Model M", 0:10),
  # Extract the effective sample size ratios for each parameter
  ~ neff_ratio(.x) %>% 
    # Plot the neff values for each parameter
    mcmc_neff_hist(binwidth = 0.05) +
    # Add a title to the plot
    labs(title = str_c("Effective Sample Size Ratios for ", .y)) +
    # Apply custom plot theme settings
    plot_theme(
      title_size = 24,
      plot.margin = margin(5, 5, 5, 5, "mm")
    ) +
    # Adjust the breaks on the x axis
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
)

# Print the N/EFF plots
# map(alt_neff_contemp, ~ print(.x))

# Save the generated plot object as a .jpeg file
map2(
  .x = alt_contemp_neff_files,
  .y = alt_neff_contemp,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "contemporary/neff/"),
    width = 12,
    height = 8,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the NUTS Diagnostics plots to----
alt_contemp_nuts_files <- str_c(
  "NUTS_Energy_SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior.jpeg"
)

### Generate plots for the NUTS diagnostics for each parameter----
alt_nuts_contemp <- map2(
  .x = contemp_models_alt,
  .y = str_c("Contemporary Model M", 0:10),
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
# map(alt_nuts_contemp, ~ print(.x))

# Save the generated plot object as a .jpeg file
map2(
  .x = alt_contemp_nuts_files,
  .y = alt_nuts_contemp,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "contemporary/nuts/"),
    width = 14,
    height = 8,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the trace plots to----
alt_contemp_trace_files <- str_c(
  "Trace_SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior.jpeg"
)

### Set model parameter names for the facets----
math_labels_contemp <- as_labeller(
  x = c(
    "b_Intercept" = "bold('Population-Level Intercept, '*alpha[0])",
    "b_female_wi" = "bold('Sex: Female, '*beta[1])",
    "b_age_cat1" = "bold('Age: 35-54, '*beta[2])",
    "b_age_cat2" = "bold('Age: 55 and Older, '*beta[3])",
    "b_educ1" = "bold('Education: Associates or Technical Degree, '*beta[4])",
    "b_educ2" = "bold('Education: University Graduate, '*beta[5])",
    "b_libdem_wi" = "bold('Liberal Democracy, '*gamma[2])",
    "b_pctfemleg_wi" = "bold('% Female Legislators, '*gamma[1])",
    "b_gdp_pcap_wi" = "bold('GDP Per Capita, '*gamma[3])",
    "b_female_wi:libdem_wi" = "bold('Sex ' %*% ' Liberal Democracy, '*delta[2])",
    "b_female_wi:pctfemleg_wi" = "bold('Sex ' %*% ' % Female Legislators, '*delta[1])",
    "b_female_wi:gdp_pcap_wi" = "bold('Sex ' %*% ' GDP Per Capita, '*delta[3])",
    "b_pctfemleg_be" = "bold('% Female Legislators, '*omega[1])",
    "b_libdem_be" = "bold('Liberal Democracy, '*omega[2])",
    "b_gdp_pcap_be" = "bold('GDP Per Capita, '*omega[2])",
    "b_female_wi:pctfemleg_be" = "bold('Sex ' %*% ' % Female Legislators, '*upsilon[1])",
    "b_female_wi:libdem_be" = "bold('Sex ' %*% ' Liberal Democracy, '*upsilon[2])",
    "b_female_wi:gdp_pcap_be" = "bold('Sex ' %*% ' GDP Per Capita, '*upsilon[3])",
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
alt_trace_contemp <- map2(
  .x = contemp_models_alt, 
  .y = str_c("Contemporary Model M", 0:10),
  # Create trace plots faceted by parameter for each model
  ~ mcmc_trace(
    .x, 
    regex_pars =  "^b_|^sd_|^cor_",
    facet_args = list(scales = "free_y", labeller = math_labels_contemp)
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
# map(alt_trace_contemp, ~ print(.x))

# Save the generated plot objects as a .jpeg file
map2(
  .x = alt_contemp_trace_files,
  .y = alt_trace_contemp,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "contemporary/traceplots/"),
    width = 27,
    height = 12,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the rank overlay plots to----
alt_contemp_rank_files <- str_c(
  "Rank_SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior.jpeg"
)

### Rank plots for the key model parameters----
alt_rank_contemp <- map2(
  .x = contemp_models_alt, 
  .y = str_c("Contemporary Model M", 0:10), 
  # Create rank plots faceted by parameter for each model
  ~ mcmc_rank_overlay(
    .x, 
    regex_pars =  "^b_|^sd_|^cor_",
    facet_args = list(scales = "free_y", labeller = math_labels_contemp)
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
# map(alt_rank_contemp, ~ print(.x))

# Save the generated plot objects as a .jpeg file
map2(
  .x = alt_contemp_rank_files,
  .y = alt_rank_contemp,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "contemporary/rankplots/"),
    width = 27,
    height = 12,
    units = "in",
    dpi = "retina",
    limitsize = F
  )
)

### Create the paths to save the combined diagnostic plots to----
alt_contemp_diagplot_files <- str_c(
  "Diagnostics_SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior.jpeg"
)

# Combining the diagnostic plots into a single one via patchwork----
alt_diag_plots_contemp <- map(
  .x = seq_along(contemp_models_alt),
  ~ (
    alt_rhats_contemp[[.x]] + 
      ggtitle(parse(text = "bold('Gelman-Rubin '*hat(R)*' Diagnostic')")) + 
      alt_neff_contemp[[.x]] + 
      ggtitle("Effective Sample Size Ratios") +
      alt_nuts_contemp[[.x]] + 
      ggtitle("No U-Turn Sampler Energy Diagnostic")) / alt_trace_contemp[[.x]] + 
    ggtitle("MCMC Trace Plots") + plot_layout(widths = c(1, 1), heights = c(1, 2)) & 
    plot_theme(
      title_size = 24,
      strip_size = 14,
      plot.margin =  margin(5, 5, 5, 5, "mm")
    )
)

# Save the generated plot objects as a .jpeg file
map2(
  .x = alt_contemp_diagplot_files,
  .y = alt_diag_plots_contemp,
  ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = str_c(diags_dir, "contemporary/"),
    width = 32,
    height = 20,
    units = "in",
    dpi = "retina",
    type = "cairo",
    limitsize = F
  )
)