#--------Alternative Priors: Societal Growth Curve Models (Socialization)-------
#-Author: A. Jordan Nafa--------------------------------Created: March 1, 2022-#
#-R Version: 4.2.1--------------------------------Last Modified: June 19, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse",
  "arrow",
  "brms",
  "bayestestR",
  "tidybayes",
  install = FALSE
)

#------------------------------------------------------------------------------#
#----------------------------Model Data Preparation-----------------------------
#------------------------------------------------------------------------------#

## Load and process the data----
soc_model_df <- read_rds("output/project-data/IVS_Model_Data.rds") %>%
  # Generate group-level identifiers
  mutate(
    # Country Identifier
    country_jj = as_factor(country_jj),
    # Country-Survey Identifier
    survey_tt = as_factor(survey_tt),
    # Rename the respondent gender variable
    female = as.integer(sex) - 1,
    # Divide female representation by 10
    soc_pctfemleg = soc_pctfemleg/10
  ) %>% 
  # Drop any unused factor levels
  droplevels() %>%
  # Group the data by country
  group_by(country_jj) %>% 
  # Decompose Predictors into within and between components
  mutate(
    across(
      c(female, soc_pctfemleg, soc_libdem),
      list(
        wi = ~ .x - mean(.x, na.rm = T),
        be = ~ mean(.x, na.rm = T)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Grand Mean Center the Contextual Predictors
  mutate(across(ends_with("_be"), ~ .x - mean(.x, na.rm = T)))

### Apply contrast coding for factors----
soc_model_df <- within(soc_model_df, {
  contrasts(age_cat) <- contr.orthonorm(levels(age_cat))
  contrasts(educ) <- contr.orthonorm(levels(educ))
})

#------------------------------------------------------------------------------#
#------------------------Socialization Model Formulas---------------------------
#------------------------------------------------------------------------------#

# Base model formula
base_form <- "churchill ~ female_wi + age_cat + educ + (1 | cohort_5y_kk) + 
  (1 + female_wi | survey_tt) + (1 + female_wi | country_jj) + "

# Additional terms for each model
terms_forms <- c(
  # Model with Liberal Democracy, without Female Representation
  "soc_libdem_wi + soc_libdem_be",
  # Model with Female Representation, without Liberal Democracy
  "soc_pctfemleg_wi + soc_pctfemleg_be",
  # Model without Female Representation but with Gender x Liberal Democracy
  "soc_libdem_wi + soc_libdem_be + female_wi:soc_libdem_wi + 
    female_wi:soc_libdem_be",
  # Model without Liberal Democracy but with Gender x Female Representation
  "soc_pctfemleg_wi + soc_pctfemleg_be + female_wi:soc_pctfemleg_wi + 
    female_wi:soc_pctfemleg_be",
  # Model with Female Representation and Liberal Democracy, No Interactions
  "soc_pctfemleg_wi + soc_libdem_wi + soc_pctfemleg_be + soc_libdem_be",
  # Model with Gender x Liberal Democracy Interactions and Female Representation
  "soc_pctfemleg_wi + soc_libdem_wi + soc_pctfemleg_be + soc_libdem_be + 
    female_wi:soc_libdem_wi + female_wi:soc_libdem_be",
  # Model with Gender x Female Representation Interactions and Liberal Democracy
  "soc_pctfemleg_wi + soc_libdem_wi + soc_pctfemleg_be + soc_libdem_be + 
    female_wi:soc_pctfemleg_wi + female_wi:soc_pctfemleg_be",
  # Model with Gender x Liberal Democracy and Gender x Female Representation
  "soc_pctfemleg_wi + soc_libdem_wi + soc_pctfemleg_be + soc_libdem_be + 
    female_wi:soc_pctfemleg_wi + female_wi:soc_pctfemleg_be + 
    female_wi:soc_libdem_wi + female_wi:soc_libdem_be"
)

## Return each combination of lhs and rhs as a list
alt_socialization_forms <- formula_builder(lhs = base_form,  rhs = terms_forms)

# Prepare the brms formula for each model
alt_socialization_forms_bf <- map(
  .x = alt_socialization_forms,
  ~ bf(.x, decomp = "QR", center = FALSE)
)

#------------------------------------------------------------------------------#
#-------------------------Socialization Model Priors----------------------------
#------------------------------------------------------------------------------#

# Here I consider alternative prior distributions to ensure the substantive
# conclusions from the main analysis are not sensitive to prior specifications.

# Specify Priors for the model M0
alt_priors_soc_sgc_cond_linvar_M0 <-
  prior(student_t(5, 0, 1.5), class = "b", coef = "Intercept") +
  prior(student_t(10, 0, 1), class = "b") + 
  prior(student_t(15, 0, 1.5), class = "sd", group = "survey_tt") +
  prior(student_t(15, 0, 1), class = "sd", group = "survey_tt", coef = "female_wi") +
  prior(student_t(10, 0, 1.25), class = "sd", group = "cohort_5y_kk") +
  prior(student_t(10, 0, 1.5), class = "sd", group = "country_jj") +
  prior(student_t(15, 0, 1.5), class = "sd", group = "country_jj", coef = "female_wi") +
  prior(lkj(3), class = "cor", group = "country_jj") +
  prior(lkj(3), class = "cor", group = "survey_tt")

# Specify Priors for the model M2 and M5
alt_priors_soc_sgc_cond_linvar_M2_M5 <- alt_priors_soc_sgc_cond_linvar_M0 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:soc_libdem_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:soc_libdem_be")

# Specify Priors for the model M4
alt_priors_soc_sgc_cond_linvar_M3_M6 <- alt_priors_soc_sgc_cond_linvar_M0 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:soc_pctfemleg_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:soc_pctfemleg_be")

# Specify Priors for the model M5
alt_priors_soc_sgc_cond_linvar_M7 <- alt_priors_soc_sgc_cond_linvar_M2_M5 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:soc_pctfemleg_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:soc_pctfemleg_be")


# Add all the priors to a list object
alt_socialization_priors <- list(
  alt_priors_soc_sgc_cond_linvar_M0,
  alt_priors_soc_sgc_cond_linvar_M0,
  alt_priors_soc_sgc_cond_linvar_M2_M5,
  alt_priors_soc_sgc_cond_linvar_M3_M6,
  alt_priors_soc_sgc_cond_linvar_M0,
  alt_priors_soc_sgc_cond_linvar_M2_M5,
  alt_priors_soc_sgc_cond_linvar_M3_M6,
  alt_priors_soc_sgc_cond_linvar_M7
)

#------------------------------------------------------------------------------#
#----------------------------Estimating the Models------------------------------
#------------------------------------------------------------------------------#

# Create the paths to save the model objects to
altprior_soc_model_paths <- str_c(
  soc_models_dir,
  "SGC_HLogit_Full_M",
  0:7,
  "_Socialization_AltPriors"
)

# Fit each of the models (6 chains, 8k iterations), takes about a week to run
alt_sgc_cond_linvar_fits <- map(
  .x = seq_along(alt_socialization_forms_bf),
  .f = ~ brm(
    formula = alt_socialization_forms_bf[[.x]],
    family = brmsfamily("bernoulli", link = "logit"),
    prior = alt_socialization_priors[[.x]],
    data = soc_model_df,
    cores = 24,
    chains = 6,
    iter = 8000,
    warmup = 3000,
    refresh = 10,
    threads = threading(
      threads = 3,
      grainsize = 100
    ),
    control = list(max_treedepth = 12),
    save_pars = save_pars(all = TRUE),
    seed = 12345,
    backend = "cmdstanr",
    sample_prior = "yes",
    file = altprior_soc_model_paths[.x]
  )
)

# Add LOO-CV to each Model
alt_sgc_cond_linvar_fits <- map(
  .x = alt_sgc_cond_linvar_fits,
  .f = ~ add_criterion(
    .x,
    criterion = "loo",
    cores = 1,
    seed = 12345,
    ndraws = 3000
  )
)

# Add Marginal and Conditional Bayes R2 to each Model
alt_sgc_cond_linvar_fits <- map(
  .x = alt_sgc_cond_linvar_fits,
  .f = ~ Bayesian_R2(
    .x,
    conditional = TRUE,
    marginal = TRUE,
    save = TRUE,
    seed = 12345,
    ndraws = 4000
  )
)

# Add Log Marginal Likelihood based on 100 Bridge Sampling Repetitions
alt_sgc_cond_linvar_fits <- map(
  .x = alt_sgc_cond_linvar_fits,
  .f = ~ add_criterion(
    .x,
    criterion = "marglik",
    cores = 16,
    seed = 12345,
    maxiter = 5e4,
    repetitions = 100,
    silent = TRUE
  )
) # Run time is approximately 6-8 hours per model; bridge sampling 
# is performed under Linux Mint since multi-core computation is not
# supported on Windows.

