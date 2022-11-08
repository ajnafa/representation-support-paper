#----------Main Analysis: Societal Growth Curve Models (Contemporary)-----------
#-Author: A. Jordan Nafa----------------------------------Created: May 1, 2022-#
#-R Version: 4.1.2------------------------------Last Modified: October 6, 2022-#

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
contemp_model_df <- read_rds("output/project-data/IVS_Model_Data.rds") %>%
  # Generate group-level identifiers
  mutate(
    # Country Identifier
    country_jj = as_factor(country_jj),
    # Country-Survey Identifier
    survey_tt = as_factor(survey_tt),
    # Rename the respondent gender variable
    female = as.integer(sex) - 1,
    # Divide Contextual Predictors by 10
    pctfemleg = pctfemleg/10,
    # Divide Contextual Predictors by 10
    gdp_pcap = gdp_pcap/10
  ) %>% 
  # Drop any unused factor levels
  droplevels() %>%
  # Group the data by country
  group_by(country_jj) %>% 
  # Decompose Predictors into within and between components
  mutate(
    across(
      c(female, pctfemleg, libdem, gdp_pcap),
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

# Apply contrast coding for factors
contemp_model_df <- within(contemp_model_df, {
  contrasts(age_cat) <- contr.orthonorm(levels(age_cat))
  contrasts(educ) <- contr.orthonorm(levels(educ))
})

#------------------------------------------------------------------------------#
#------------------------Contemporary Model Formulas----------------------------
#------------------------------------------------------------------------------#

# Base model formula
base_form <- "churchill ~ female_wi + age_cat + educ + (1 | cohort_5y_kk) + 
  (1 + female_wi | survey_tt) + (1 + female_wi | country_jj) + "

# Additional terms for each model
terms_forms <- c(
  # Model with Liberal Democracy, without Female Representation
  "libdem_wi + libdem_be",
  # Model without Female Representation but with Gender x Liberal Democracy
  "libdem_wi + libdem_be + female_wi:libdem_wi + female_wi:libdem_be",
  # Model with Female Representation and Liberal Democracy, No Interactions
  "pctfemleg_wi + libdem_wi + pctfemleg_be + libdem_be",
  # Model with Gender x Liberal Democracy Interactions
  "pctfemleg_wi + libdem_wi + pctfemleg_be + libdem_be + female_wi:libdem_wi + 
    female_wi:libdem_be",
  # Model with Gender x Female Representation Interactions
  "pctfemleg_wi + libdem_wi + pctfemleg_be + libdem_be + 
    female_wi:pctfemleg_wi + female_wi:pctfemleg_be",
  # Model with Gender x Liberal Democracy and Gender x Female Representation
  "pctfemleg_wi + libdem_wi + pctfemleg_be + libdem_be + female_wi:pctfemleg_wi + 
    female_wi:pctfemleg_be + female_wi:libdem_wi + female_wi:libdem_be",
  # Model with GDP Per Capita and Liberal Democracy, no Female Representation 
  "gdp_pcap_wi + libdem_wi + gdp_pcap_be + libdem_be",
  # Model with GDP Per Capita, Liberal Democracy, and Female Representation
  "pctfemleg_wi + gdp_pcap_wi + libdem_wi + pctfemleg_be + gdp_pcap_be + 
    libdem_be",
  # Model with GDP, Liberal Democracy, and Gender x Female Representation
  "pctfemleg_wi + gdp_pcap_wi + libdem_wi + pctfemleg_be + gdp_pcap_be + 
    libdem_be + female_wi:pctfemleg_wi + female_wi:pctfemleg_be",
  # Model with GDP, Gender x Liberal Democracy, and Gender x Female Representation
  "pctfemleg_wi + gdp_pcap_wi + libdem_wi + pctfemleg_be + gdp_pcap_be + 
    libdem_be + female_wi:pctfemleg_wi + female_wi:pctfemleg_be + 
    female_wi:libdem_wi + female_wi:libdem_be",
  # Model with Gender x GDP, Liberal Democracy, and Gender x Female Representation
  "female_wi + age_cat + educ + pctfemleg_wi + gdp_pcap_wi + 
    libdem_wi + pctfemleg_be + gdp_pcap_be + libdem_be + female_wi:pctfemleg_wi + 
    female_wi:pctfemleg_be + female_wi:gdp_pcap_wi + female_wi:gdp_pcap_be"
)

## Return each combination of lhs and rhs as a list
alt_contemp_forms <- formula_builder(lhs = base_form,  rhs = terms_forms)

# Prepare the brms formula for each model
alt_contemp_forms_bf <- map(
  .x = contemp_forms,
  ~ bf(.x, decomp = "QR", center = FALSE)
)

#------------------------------------------------------------------------------#
#--------------------------Contemporary Model Priors----------------------------
#------------------------------------------------------------------------------#

# Here I consider alternative prior distributions to ensure the substantive
# conclusions from the main analysis are not sensitive to prior specifications.

# Specify Priors for the model M0
alt_priors_contemp_sgc_cond_linvar_M0 <-
  prior(student_t(5, 0, 1.5), class = "b", coef = "Intercept") +
  prior(student_t(10, 0, 1), class = "b") + 
  prior(student_t(15, 0, 1.5), class = "sd", group = "survey_tt") +
  prior(student_t(15, 0, 1), class = "sd", group = "survey_tt", coef = "female_wi") +
  prior(student_t(10, 0, 1.25), class = "sd", group = "cohort_5y_kk") +
  prior(student_t(10, 0, 1.5), class = "sd", group = "country_jj") +
  prior(student_t(15, 0, 1.5), class = "sd", group = "country_jj", coef = "female_wi") +
  prior(lkj(3), class = "cor", group = "country_jj") +
  prior(lkj(3), class = "cor", group = "survey_tt")

# Specify Priors for the model M1 and M3
alt_priors_contemp_sgc_cond_linvar_M1_M3 <- alt_priors_contemp_sgc_cond_linvar_M0 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:libdem_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:libdem_be")

# Specify Priors for the model M4
alt_priors_contemp_sgc_cond_linvar_M4 <- alt_priors_contemp_sgc_cond_linvar_M0 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:pctfemleg_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:pctfemleg_be")

# Specify Priors for the model M5
alt_priors_contemp_sgc_cond_linvar_M5 <- alt_priors_contemp_sgc_cond_linvar_M1_M3 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:pctfemleg_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:pctfemleg_be")

# Specify Priors for the model M10
alt_priors_contemp_sgc_cond_linvar_M10 <- alt_priors_contemp_sgc_cond_linvar_M4 +
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:gdp_pcap_wi") + 
  prior(student_t(10, 0, 0.5), class = "b", coef = "female_wi:gdp_pcap_be")

# Add all the priors to a list object
alt_contemp_priors <- list(
  alt_priors_contemp_sgc_cond_linvar_M0,
  alt_priors_contemp_sgc_cond_linvar_M1_M3,
  alt_priors_contemp_sgc_cond_linvar_M0,
  alt_priors_contemp_sgc_cond_linvar_M1_M3,
  alt_priors_contemp_sgc_cond_linvar_M4,
  alt_priors_contemp_sgc_cond_linvar_M5,
  alt_priors_contemp_sgc_cond_linvar_M0,
  alt_priors_contemp_sgc_cond_linvar_M0,
  alt_priors_contemp_sgc_cond_linvar_M4,
  alt_priors_contemp_sgc_cond_linvar_M5,
  alt_priors_contemp_sgc_cond_linvar_M10
)

#------------------------------------------------------------------------------#
#----------------------------Estimating the Models------------------------------
#------------------------------------------------------------------------------#

# Create the paths to save the model objects to
alt_contemp_model_paths <- str_c(
  contemp_models_dir,
  "SGC_HLogit_Full_M",
  0:10,
  "_Contemporary_AltPrior"
)

# Fit each of the models (6 chains, 8k iterations), takes about a week to run
alt_sgc_cond_contemp_linvar_fits <- map(
  .x = seq_along(alt_contemp_forms),
  .f = ~ brm(
    formula = alt_contemp_forms_bf[[.x]],
    family = brmsfamily("bernoulli", link = "logit"),
    prior = alt_contemp_priors[[.x]],
    data = contemp_model_df,
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
    file = alt_contemp_model_paths[.x]
  )
) # Each model is run with 6 chains in parallel for 8,000 iterations each;
# total average wall time per model is between 16 and 27 hours. Total run time
# for all models is approximately 240.5 hours.

# Add LOO-CV to each Model
alt_sgc_cond_contemp_linvar_fits <- map(
  .x = alt_sgc_cond_contemp_linvar_fits,
  .f = ~ add_criterion(
    .x,
    criterion = "loo",
    cores = 1,
    seed = 12345,
    ndraws = 3000
  )
)

# Add Marginal and Conditional Bayes R2 to each Model
alt_sgc_cond_contemp_linvar_fits <- map(
  .x = contemp_models_alt,
  .f = ~ Bayesian_R2(
    .x,
    conditional = TRUE,
    marginal = TRUE,
    save = TRUE,
    seed = 12345,
    ndraws = 3000
  )
)

# Add Log Marginal Likelihood based on 100 Bridge Sampling Repetitions
alt_sgc_cond_contemp_linvar_fits <- map(
  .x = alt_sgc_cond_contemp_linvar_fits,
  .f = ~ add_criterion(
    .x,
    criterion = "marglik",
    cores = 16,
    seed = 12345,
    maxiter = 5e4,
    repetitions = 100,
    silent = TRUE
  )
) # Run time is approximately 8-10 hours per model; bridge sampling 
# is performed under Linux Mint since multi-core computation is not
# supported on Windows.


