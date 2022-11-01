#--Bayesian Model Averaged Marginal Effects: Socialization, Alternative Priors--
#-Author: A. Jordan Nafa------------------------------Created: October 5, 2022-#
#-R Version: 4.2.1-----------------------------Last Modified: October 16, 2022-#

# Load the necessary libraries
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "brms",
  "brmsmargins",
  install = FALSE # Set this to TRUE to install missing packages
)

#------------------------------------------------------------------------------#
#-----------Loading the Socialization Models with Alternative Priors------------
#------------------------------------------------------------------------------#

# Load the socialization models w/alternative priors
soc_models_altprior <- map(
  .x = list.files(
    soc_models_dir, 
    pattern = ".*_AltPriors.rds",
    full.names = TRUE
  ),
  ~ read_rds(.x)
)

# Extract the parameters of interest in each model
soc_model_params <- map(
  soc_models_altprior, 
  ~ variables(.x) %>% 
    str_subset(., "^b_|^sd_|^cor_")
)

# Since some of the models have interactions, we only want to average over 
# models containing the predictor when calculating the model averaged contrasts
alt_eff_pos <- map_dbl(
  soc_model_params, 
  ~ str_which(.x, "b_female_wi:soc_pctfemleg.*|soc_pctfemleg") %>% 
    length()
)

#------------------------------------------------------------------------------#
#------------------AME for Socialization % Female Legislators-------------------
#------------------------------------------------------------------------------#

# Subset of the models with the main effects
soc_femleg_mods_alt <- soc_models_altprior[which(0 < alt_eff_pos)]

# Initialize a list to store the results in
ame_main_effs_soc_alt <- list()

# Set h to use for approximating the first derivative
h <- .001

# Calculate the AME for each model, this takes
for (i in seq_along(soc_femleg_mods_alt)) {
  ame_main_effs_soc_alt[[i]] <- brmsmargins(
    object = soc_femleg_mods_alt[[i]],
    add = data.table::data.table(soc_pctfemleg_wi = c(0, h)),
    contrasts = matrix(c(-1/h, 1/h), nrow = 2),
    summarize = FALSE,
    CIType = "ETI", 
    effects = "integrateoutRE", 
    seed = 12345,
    index = seq(1, 30e3, 10), # Thin by 10 due to memory constraints
    cores = 1L
  )
}

# Apply names to the list of marginal effects for each model
names(ame_main_effs_soc_alt) <- str_c("MS", (which(0 < alt_eff_pos) - 1))

# Write the combined data to a parquet file
write_rds(
  ame_main_effs_soc_alt, 
  file = str_c(alt_preds_dir, "socialization/PopAvg_AME_SocMain.rds"),
  compress = "gz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#-------------AME for Socialization % Female Legislators by Gender--------------
#------------------------------------------------------------------------------#

# Subset of the models with the interaction effects
soc_femleg_gender_mods_alt <- soc_models_altprior[which(3 < alt_eff_pos)]

# Take the minimum and maximum within countries 
x_data <- soc_femleg_gender_mods_alt[[1]]$data %>% 
  # Group the data by country
  group_by(country_jj) %>% 
  # Take the minimum and maximum of X within countries
  summarise(
    Xmin = min(soc_pctfemleg_wi),
    Xmax = max(soc_pctfemleg_wi)
  ) %>% 
  # Pivot the data to long form
  pivot_longer(
    cols = Xmin:Xmax,
    names_to = "variable",
    values_to = "value"
  )

# Data for the wat argument in brmsmargins
use_wat <- list(
  ID = "country_jj", 
  soc_pctfemleg_wi = x_data
)

# Data for the at argument in brmsmargins
use_at <- expand.grid(
  female_wi = c(-1, 1), 
  soc_pctfemleg_wi = c("Xmin", "Xmax")
)

# Build the contrast matrix
contr_mat <- cbind(soc_pctfemleg_wi = c(-1, 1)) %x% diag(2)
contr_mat <- cbind(
  contr_mat,
  contr_mat[, 2] - contr_mat[, 1]
)

# Apply names to the contrast matrix
colnames(contr_mat) <- c(
  paste0("AME Socialization: ", c("Male", "Female")),
  "delta AME Socialization"
)

# Initialize a list to store the results in
ame_inter_effs_soc_alt <- list()

# Calculate the AME for each model, this takes a few hours
for (i in seq_along(soc_femleg_gender_mods_alt)) {
  ame_inter_effs_soc_alt[[i]] <- brmsmargins(
    object = soc_femleg_gender_mods_alt[[i]],
    at = use_at,
    wat = use_wat,
    contrasts = contr_mat,
    summarize = FALSE,
    CIType = "ETI", 
    effects = "integrateoutRE", 
    seed = 12345,
    index = seq(1, 30e3, 10), # Thin by 10 due to memory constraints
    cores = 1L
  )
}

# Apply names to the list of marginal effects for each model
names(ame_inter_effs_soc_alt) <- str_c("MS", (which(3 < alt_eff_pos) - 1))

# Write the combined data to a parquet file
write_rds(
  ame_inter_effs_soc_alt, 
  file = str_c(alt_preds_dir, "socialization/PopAvg_AME_SocInter.rds"),
  compress = "gz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#------------BMA/Stacked AME for Socialization % Female Legislators-------------
#------------------------------------------------------------------------------#

# Read in the AMEs from the disk
ame_main_effs_soc_alt <- read_rds(str_c(
  str_c(alt_preds_dir, 
        "socialization/PopAvg_AME_SocMain.rds"
  )))

# Get the code for the models to pass to bridgesampling::post_prob
get_models_marglik("soc_models_altprior", which(0 < alt_eff_pos))

# Generate posterior probability weights based on 100 bridge sampling reps
post_probs_soc_femleg_main <- bridgesampling::post_prob(
  soc_models_altprior[[2]]$criteria$marglik, 
  soc_models_altprior[[4]]$criteria$marglik, 
  soc_models_altprior[[5]]$criteria$marglik, 
  soc_models_altprior[[6]]$criteria$marglik, 
  soc_models_altprior[[7]]$criteria$marglik, 
  soc_models_altprior[[8]]$criteria$marglik,
  prior_prob = rep(1/6, length.out = 6),
  model_names = str_c("MS", (which(0 < alt_eff_pos) - 1))
)

# Generate the stacking weights with 10k Bayesian-Bootstrap replications
loo_weights_soc_main_alt <- stacking_weights(
  ame_main_effs_soc_alt,
  model_names = str_c("MS", (which(0 < alt_eff_pos) - 1)),
  bb_draws = TRUE,
  n = 10e3,
  seed = 12345
)

# Calculate the model averaged marginal effect for the main effect
soc_main_bmame_alt <- model_averaged_ame(
  ame_main_effs_soc_alt, 
  weights = post_probs_soc_femleg_main,
  seed = 12345
)

# Write the combined data to a parquet file
write_rds(
  soc_main_bmame_alt, 
  file = str_c(
    alt_preds_dir, 
    "socialization/BMAME_PopAvg_AME_Soc_Main.rds"
  ),
  compress = "gz", 
  compression = 9L
)

# Calculate the stacked marginal effect for the main effect
soc_main_stacked_alt <- stacked_ame(
  ame_main_effs_soc_alt, 
  weights = loo_weights_soc_main_alt,
  seed = 12345
)

# Write the combined data to a parquet file
write_rds(
  soc_main_stacked_alt, 
  file = str_c(
    alt_preds_dir, 
    "socialization/Stacked_PopAvg_AME_Soc_Main.rds"
  ),
  compress = "gz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#------------BMAME for Socialization % Female Legislators by Gender-------------
#------------------------------------------------------------------------------#

# Read in the AMEs from the disk
ame_inter_effs_soc_alt <- read_rds(str_c(
  alt_preds_dir, 
  "socialization/PopAvg_AME_SocInter.rds"
))

# Get the code for the models to pass to bridgesampling::post_prob
get_models_marglik("soc_models_altprior", which(3 < alt_eff_pos))

# Generate posterior probability weights based on 100 bridge sampling reps
post_probs_soc_femleg_inter <- bridgesampling::post_prob(
  soc_models_altprior[[4]]$criteria$marglik, 
  soc_models_altprior[[7]]$criteria$marglik, 
  soc_models_altprior[[8]]$criteria$marglik,
  prior_prob = rep(1/5, length.out = 5),
  model_names = str_c("MS", (which(3 < alt_eff_pos) - 1))
)

# Generate the stacking weights with 10k Bayesian-Bootstrap replications
loo_weights_soc_inter_alt <- stacking_weights(
  soc_femleg_gender_mods_alt,
  model_names = str_c("MS", (which(3 < alt_eff_pos) - 1)),
  bb_draws = TRUE,
  n = 10e3,
  seed = 12345
)

# Calculate the model averaged marginal effect for the main effect
soc_inter_bmame_alt <- model_averaged_ame(
  ame_inter_effs_soc_alt, 
  weights = post_probs_soc_femleg_inter,
  seed = 12345
)

# Write the combined data to an rds file
write_rds(
  soc_inter_bmame_alt, 
  file = str_c(
    alt_preds_dir, 
    "socialization/BMAME_PopAvg_AME_Soc_Inter.rds"
  ),
  compress = "gz", 
  compression = 9L
)

# Calculate the stacked marginal effect for the main effect
soc_inter_stacked_alt <- stacked_ame(
  ame_inter_effs_soc_alt, 
  weights = loo_weights_soc_inter_alt,
  seed = 12345
)

# Write the combined data to an rds file
write_rds(
  soc_inter_stacked_alt, 
  file = str_c(
    alt_preds_dir, 
    "socialization/Stacked_PopAvg_AME_Soc_Inter.rds"
  ),
  compress = "gz", 
  compression = 9L
)
