#------------------------Sample Country-Surveys Figures-------------------------
#-Author: A. Jordan Nafa-------------------------------Created: March 10, 2022-#
#-R Version: 4.2.1------------------------------------Revised: October 4, 2022-#

# Load the necessary libraries----
pacman::p_load(
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "rgeos",
  "tidyverse",
  "arrow",
  install = FALSE
)

# Load the data
model_df <- read_rds("output/project-data/IVS_Model_Data_Full.rds")

#------------------------------------------------------------------------------#
#----------------Data Prep of Case Selection for Main Analysis------------------
#------------------------------------------------------------------------------#

# Generate the data for figure 1
cases_df <- model_df %>%
  # Select a subset of the variables
  select(country_jj, year:project, vdem_iso, country_year, regime_type) %>%
  # Keep distinct combinations
  distinct_all() %>%
  # Drop any unused factor levels
  droplevels() %>% 
  # Group the data by identifiers
  group_by(country_jj, year, vdem_iso, country_year, regime_type) %>%
  # Collapse the data
  summarise(
    project = factor(
      sum(as.integer(project)),
      levels = 1:3,
      labels = c(
        "EVS",
        "WVS",
        "WVS + EVS"
      ))) %>%
  # Recode North Ireland EVS waves a part of Great Britain for map purposes 
  mutate(vdem_iso = case_when(
    country_jj == "North Ireland" ~ "GBR",
    TRUE ~ vdem_iso
  )) %>% 
  # Regroup the data by iso code
  group_by(vdem_iso, project) %>% 
  # Calculate the number of waves per country
  mutate(waves = n()) %>% 
  # Ungroup the data
  ungroup()

# Pivot the data to wide form
cases_map_df <- cases_df %>% 
  # Keep distinct survey-waves
  distinct(vdem_iso, waves, project) %>% 
  # Pivot the data to wide form
  pivot_wider(
    id_cols = vdem_iso,
    names_from = project,
    values_from = waves,
    values_fill = 0
  ) %>% 
  # Create columns for plotting
  mutate(
    # Indicator is both WVS and EVS covered the country
    `WVS + EVS` = case_when(
      `WVS + EVS` > 0 ~ as.integer(`WVS + EVS` + 1),
      TRUE ~ `WVS + EVS`
    ),
    # Totals number of surveys per country
    Total = WVS + EVS + `WVS + EVS`,
    # Project-Country indicator
    project = case_when(
      `WVS + EVS` > 0 | (WVS > 0 & EVS > 0) ~ "WVS and EVS",
      WVS > 0 & EVS == 0 ~ "WVS",
      EVS > 0 & WVS == 0 ~ "EVS"
    )) %>% 
  # Set 0 values to NA
  na_if(0)

# Load the earth spatial polygons
world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Merge survey coverage and spatial data
cases_full_world <- world_sf %>%
  # Left-Join the survey coverage
  left_join(cases_map_df, by = c("iso_a3" = "vdem_iso")) %>%
  # Keep a subset of the variables
  select(name_long, WVS:project, continent:region_wb) %>% 
  # Drop antartica
  filter(name_long != "Antarctica")

# Write the IVS Model Data to a File
write_rds(
  x = cases_full_world, 
  file = "output/project-data/Sample-Map-Data.rds",
  compress = "gz",
  compression = 9L
)

#------------------------------------------------------------------------------#
#------------------Figure 1: Case Selection for Main Analysis-------------------
#------------------------------------------------------------------------------#

# Map of WVS and EVS Countries
full_map_plot <- ggplot() +
  # Add the spatial geometries geom
  geom_sf(
    aes(geometry = geometry, fill = project),
    data = cases_full_world,
    color = "black",
    size = .3,
    na.rm = T
  ) +
  # Parameters for the color scale
  scale_fill_manual(
    values = viridis::mako(3, begin = 0.5, end = 1),
    na.translate = FALSE
  ) +
  # Add labels to the plot
  labs(
    fill = "Survey Project",
    caption = str_wrap(
      "Notes: Sample includes countries classified as electoral democracies at 
      some point in thier history and that are covered by at least two surveys 
      between 1995 and 2020. Select countries were excluded despite otherwise 
      meeting the minimum criteria for inclusion due to evidence of severe 
      translation error in at least one of the items used to operationalize 
      the dependent variable or time invariance in one of the main survey-level 
      predictors of interest. See the online appendix for a more detailed 
      discussion.", 
      width = 140
      )
    )  +
  # Apply Custom Map Theme Settings
  map_theme(
    base_family = "serif",
    caption.hjust = 0, 
    caption.vjust = -1, 
    legend.position = "bottom", 
    legend_text_size = 25,
    caption_size = 20,
    show.axis = F,
    plot.margin = margin(0, 0, 2, 0, "mm"),
    plot.caption.position = "plot"
  )

# Preview the quota plot
print(full_map_plot)

# Save the generated plot object as .png and .jpeg files
map(
  .x = c("png", "jpeg"),
  ~ ggsave(
    filename = str_c("Figure_1.1_Full_Sample_Countries_Map.", .x),
    plot = full_map_plot,
    device = .x,
    path = "output/figures",
    width = 16,
    height = 9,
    units = "in",
    dpi = "retina"
  ))

#------------------------------------------------------------------------------#
#-----------------Figure 1A: Detailed Coverage for the Appendix-----------------
#------------------------------------------------------------------------------#

# Sample of countries in the main analysis
figs_coverage_full <- cases_df %>%
  # Initiate a ggplot2 object
  ggplot(aes(
    x = year,
    y = fct_rev(country_jj),
    fill = project,
    shape = project
  ),
  col = "black"
  ) +
  # Add a point geom
  geom_point(size = 3.2) +
  # Set the shape parameter for each group
  scale_shape_manual(values = c(22, 24, 25)) +
  # Parameters for the color scale
  scale_fill_manual(
    values = viridis::mako(3, begin = 0.5, end = 1),
    na.translate = FALSE
  ) +
  # Setting the parameters for the plot legend
  guides(fill = guide_legend(
    title = "Survey Project",
    override.aes = list(
      shape = c(22, 24, 25),
      fill = viridis::mako(3, begin = 0.5, end = 1),
      size = 4
    )
  ),
  shape = "none"
  ) +
  # Plot theme labels
  labs(
    y = "",
    x = "WVS/EVS Year"
    ) +
  # Apply plot theme settings
  plot_theme(
    caption.hjust = 0.5, 
    caption.vjust = -3,
    xaxis_size = 22,
    x_axis_face = "bold",
    y_axis_face = "bold",
    plot.margin = margin(3, 0.2, 5, 0.2, "mm"),
    transparent = TRUE
  )

# Preview the plot
print(figs_coverage_full)

# Save the generated plot object as .png and .jpeg files
map(
  .x = c("png", "jpeg"),
  ~ ggsave(
    filename = str_c("Figure_1.2_Full_Sample_Countries.", .x),
    plot = figs_coverage_full,
    device = .x,
    path = "output/figures",
    width = 12,
    height = 16,
    units = "in",
    dpi = "retina"
  ))

