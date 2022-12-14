# Set Project Options
options(
  digits = 6, # Significant figures output
  mc.cores = 12L, # Multicore processing
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  brms.backend = "cmdstanr",
  knitr.kable.NA = ''
)

# Load the helper functions on project start
.helpers <- lapply(
  list.files(
    path = "functions/", 
    pattern = ".*.R", 
    full.names = TRUE
    ), source
  )

# Base Directory for the contemporary model objects
contemp_models_dir <- "output/fits/contemporary/"

# Base Directory for the socialization model objects
soc_models_dir <- "output/fits/socialization/"

## Base Directory for the socialization predictions
main_preds_dir <- "output/predictions/main-models/"

## Base Directory for the socialization (alternative prior) predictions
alt_preds_dir <- "output/predictions/alternative-prior/"

# Base Directory for the stan files
stan_dir <- "output/stan-code/"

# Base Directory for the model diagnostics
diags_dir <- "output/diagnostics/"
