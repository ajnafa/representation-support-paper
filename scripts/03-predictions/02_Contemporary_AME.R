#------------Bayesian Model Averaged Marginal Effects: Contemporary-------------
#-Author: A. Jordan Nafa------------------------------Created: October 5, 2022-#
#-R Version: 4.2.1-----------------------------Last Modified: October 16, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "brms",
  "patchwork",
  "brmsmargins",
  install = FALSE ## Set this to TRUE to install missing packages
)

#------------------------------------------------------------------------------#
#----------------------Loading the Contemporary Models--------------------------
#------------------------------------------------------------------------------#

# Load the contemporary models for the main analysis
contemp_models_main <- map(
  .x = list.files(
    contemp_models_dir, 
    pattern = ".*_Final.rds",
    full.names = TRUE
  ),
  ~ read_rds(.x)
)

# Extract the parameters of interest in each model
contemp_models_params <- map(
  contemp_models_main, 
  ~ variables(.x) %>% 
    str_subset(., "^b_|^sd_|^cor_")
)

# Since some of the models have interactions, we only want to average over 
# models containing the predictor when calculating the model averaged contrasts
main_eff_pos <- map_dbl(
  contemp_models_params, 
  ~ str_which(.x, "b_female_wi:pctfemleg.*|pctfemleg") %>% 
    length()
)

#------------------------------------------------------------------------------#
#-------------------AME for Contemporary % Female Legislators-------------------
#------------------------------------------------------------------------------#

## Subset of the models with only the main effects calculations
contemp_femleg_mods <- contemp_models_main[which(0 < main_eff_pos)]

## Initialize a list to store the results in
ame_main_effs_contemp <- list()

## Set h to use for approximating the first derivative
h <- .001

## Calculate the AME for each model, this takes about 15 hours
for (i in seq_along(contemp_femleg_mods)) {
  ame_main_effs_contemp[[i]] <- brmsmargins(
    object = contemp_femleg_mods[[i]],
    add = data.table::data.table(pctfemleg_wi = c(0, h)),
    contrasts = matrix(c(-1/h, 1/h), nrow = 2),
    summarize = FALSE,
    CIType = "ETI", 
    effects = "integrateoutRE", 
    seed = 12345,
    index = seq(1, 30e3, 10), # Use every tenth draw, 3k in total
    cores = 1L
  )
}

## Apply names to the list of marginal effects for each model
names(ame_main_effs_contemp) <- str_c("MC", (which(0 < main_eff_pos) - 1))

# Write the combined data to a parquet file
write_rds(
  ame_main_effs_contemp, 
  file = str_c(
    main_preds_dir, 
    "contemporary/PopAvg_AME_ContempMain.rds"
    ),
  compress = "gz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#--------------AME for Contemporary % Female Legislators by Gender--------------
#------------------------------------------------------------------------------#

## Subset of the models with the interaction effects
contemp_gender_femleg_mods <- contemp_models_main[which(3 < main_eff_pos)]

## Take the minimum and maximum within countries 
x_data <- contemp_gender_femleg_mods[[1]]$data %>% 
  # Group the data by country
  group_by(country_jj) %>% 
  # Take the minimum and maximum of X within countries
  summarise(
    Xmin = min(pctfemleg_wi),
    Xmax = max(pctfemleg_wi)
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
  pctfemleg_wi = x_data
)

# Data for the at argument in brmsmargins
use_at <- expand.grid(
  female_wi = c(-1, 1), 
  pctfemleg_wi = c("Xmin", "Xmax")
)

# Build the contrast matrix
contr_mat <- cbind(pctfemleg_wi = c(-1, 1)) %x% diag(2)
contr_mat <- cbind(
  contr_mat,
  contr_mat[, 2] - contr_mat[, 1]
)

# Apply names to the contrast matrix
colnames(contr_mat) <- c(
  paste0("AME Contemporary: ", c("Male", "Female")),
  "delta AME Contemporary"
)

## Initialize a list to store the results in
ame_inter_effs_contemp <- list()

## Calculate the AME for each model, this takes about seven hours
for (i in seq_along(contemp_gender_femleg_mods)) {
  ame_inter_effs_contemp[[i]] <- brmsmargins(
    object = contemp_gender_femleg_mods[[i]],
    at = use_at,
    wat = use_wat,
    contrasts = contr_mat,
    summarize = FALSE,
    CIType = "ETI", 
    effects = "integrateoutRE", 
    seed = 12345,
    index = seq(1, 30e3, 10), # Thin by 10 due to memory constraints
    cores = 2L
  )
}

## Apply names to the list of marginal effects for each model
names(ame_inter_effs_contemp) <- str_c("MC", (which(3 < main_eff_pos) - 1))

# Write the combined data to a parquet file
write_rds(
  ame_inter_effs_contemp, 
  file = str_c( 
    main_preds_dir, 
    "contemporary/PopAvg_AME_ContempInter.rds"
    ),
  compress = "gz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#-------------BMA/Stacked AME for Contemporary % Female Legislators-------------
#------------------------------------------------------------------------------#

# Read in the AMEs from the disk
ame_main_effs_contemp <- read_rds(str_c(
  main_preds_dir, 
  "contemporary/PopAvg_AME_ContempMain.rds"
  ))

## Get the code for the models to pass to bridgesampling::post_prob
marglik_obs <- str_c(
  "contemp_models_main[[", 
  which(0 < main_eff_pos), 
  "]]$criteria$marglik"
) %>% 
  ## Use glue here to construct the object calls
  glue::glue_collapse(., sep = ", \n")

# Generate posterior probability weights based on 100 bridge sampling reps
post_probs_femleg_main <- bridgesampling::post_prob(
  contemp_models_main[[3]]$criteria$marglik, 
  contemp_models_main[[4]]$criteria$marglik, 
  contemp_models_main[[5]]$criteria$marglik, 
  contemp_models_main[[6]]$criteria$marglik, 
  contemp_models_main[[7]]$criteria$marglik, 
  contemp_models_main[[9]]$criteria$marglik, 
  contemp_models_main[[10]]$criteria$marglik, 
  contemp_models_main[[11]]$criteria$marglik,
  prior_prob = rep(1/8, length.out = 8),
  model_names = str_c("MC", (which(0 < main_eff_pos) - 1))
)

## Generate the stacking weights with 10k Bayesian-Bootstrap replications
loo_weights_main <- stacking_weights(
  contemp_femleg_mods,
  model_names = str_c("MC", (which(0 < main_eff_pos) - 1)),
  bb_draws = TRUE,
  n = 10e3,
  seed = 12345
)

## Calculate the model averaged marginal effect for the main effect
contemp_main_bmame <- model_averaged_ame(
  ame_main_effs_contemp, 
  weights = post_probs_femleg_main,
  summary = TRUE
)

# Write the combined data to a parquet file
write_rds(
  contemp_main_bmame, 
  file = str_c(
    main_preds_dir, 
    "contemporary/BMAME_PopAvg_AME_Contemp_Main.rds"
    ),
  compress = "gz", 
  compression = 9L
)

## Calculate the stacked marginal effect for the main effect
contemp_main_stacked <- stacked_ame(
  ame_main_effs_contemp, 
  weights = loo_weights_main,
  seed = 12345
  )

# Write the combined data to a parquet file
write_rds(
  contemp_main_stacked, 
  file = str_c(
    main_preds_dir, 
    "contemporary/Stacked_PopAvg_AME_Contemp_Main.rds"
    ),
  compress = "gz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#-------------BMAME for Contemporary % Female Legislators by Gender-------------
#------------------------------------------------------------------------------#

# Read in the AMEs from the disk
ame_inter_effs_contemp <- read_rds(str_c(
  main_preds_dir, 
  "contemporary/PopAvg_AME_ContempInter.rds"
))

## Get the code for the models to pass to bridgesampling::post_prob
marglik_obs <- str_c(
  "contemp_models_main[[", 
  which(3 < main_eff_pos), 
  "]]$criteria$marglik"
) %>% 
  ## Use glue here to construct the object calls
  glue::glue_collapse(., sep = ", \n")

# Generate posterior probability weights based on 100 bridge sampling reps
post_probs_femleg_inter <- bridgesampling::post_prob(
  contemp_models_main[[3]]$criteria$marglik, 
  contemp_models_main[[6]]$criteria$marglik, 
  contemp_models_main[[7]]$criteria$marglik, 
  contemp_models_main[[10]]$criteria$marglik, 
  contemp_models_main[[11]]$criteria$marglik,
  prior_prob = rep(1/5, length.out = 5),
  model_names = str_c("MC", (which(3 < main_eff_pos) - 1))
)

## Generate the stacking weights with 10k Bayesian-Bootstrap replications
loo_weights_inter <- stacking_weights(
  contemp_gender_femleg_mods,
  model_names = str_c("MC", (which(3 < main_eff_pos) - 1)),
  bb_draws = TRUE,
  n = 10e3,
  seed = 12345
)

## Calculate the model averaged marginal effect for the main effect
contemp_inter_bmame <- model_averaged_ame(
  ame_inter_effs_contemp, 
  weights = post_probs_femleg_inter,
  summary = TRUE
)

# Write the combined data to a parquet file
write_rds(
  contemp_inter_bmame, 
  file = str_c(
    main_preds_dir, 
    "contemporary/BMAME_PopAvg_AME_Contemp_Inter.rds"
    ),
  compress = "gz", 
  compression = 9L
)

## Calculate the stacked marginal effect for the main effect
contemp_inter_stacked <- stacked_ame(
  ame_inter_effs_contemp, 
  weights = loo_weights_inter,
  seed = 12345
)

# Write the combined data to a parquet file
write_rds(
  contemp_inter_stacked, 
  file = str_c(
    main_preds_dir, 
    "contemporary/Stacked_PopAvg_AME_Contemp_Inter.rds"
    ),
  compress = "gz", 
  compression = 9L
)
