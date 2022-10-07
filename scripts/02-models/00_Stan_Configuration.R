#-------------Setup: Getting Started with Stan, cmdstanr, and brms--------------
#-Author: A. Jordan Nafa------------------------------Created: January 7, 2022-#
#-R Version: 4.2.1------------------------------------Revised: October 6, 2022-#

## Step 1. System Preliminaries----

# Check if any existing Stan packages are installed
{
  ## Check for existing installations
  stan_packages <- installed.packages()[
    grepl("cmdstanr|rstan$|StanHeaders|brms$", 
          installed.packages()[, 1]), 1]
  
  ## Remove any existing Stan packages
  if (length(stan_packages) > 0) {
    remove.packages(c("StanHeaders", "rstan", "brms"))
  }
  
  ## Delete any pre-existing RData file
  if (file.exists(".RData")) {
    file.remove(".RData")
  }
}

# Check if required packages are installed
required_pkgs()

## Stan Installation and Configuration----

## Step 2: Installing rstan and brms----

# Install the development versions of rstan and StanHeaders
install.packages(
  pkgs = "rstan",
  repos = c(
    "https://mc-stan.org/r-packages/", 
    getOption("repos")
  ))

# This will fit a simple example model to check that the Stan compiler is working
example(stan_model, package = "rstan", run.dontrun = TRUE)

# You can either manually restart your R session via RStudio's GUI or run this code
rstudioapi::restartSession()

# Install the latest development version of brms from github
remotes::install_github("paul-buerkner/brms")

## Step 3: Installing cmdstanr and cmdstan----

# Install cmdstanr from github
remotes::install_github("stan-dev/cmdstanr")

# Check that the C++ Toolchain is Configured
cmdstanr::check_cmdstan_toolchain(fix = TRUE)

# Set the makeflags to use multiple cores for faster compilation
Sys.setenv(
  MAKEFLAGS = paste0(
    "-j", 
    parallel::detectCores(logical = FALSE)
  ))

# Install cmdstan version 2.30.1
cmdstanr::install_cmdstan(
  dir = paste(Sys.getenv("HOME"), "/.cmdstan/", sep = ""),
  cores = parallel::detectCores(logical = FALSE),
  overwrite = TRUE,
  version = "2.30.1",
  cpp_options = list("STAN_THREADS" = TRUE),
  check_toolchain = TRUE
)

# You can either manually restart your R session via RStudio's GUI or run this code
rstudioapi::restartSession()

# Verify that cmdstan installed successfully
(cmdstan.version <- cmdstanr::cmdstan_version())

# Ensure cmdstan path is set properly
cmdstanr::set_cmdstan_path(
  path = paste(
    Sys.getenv("HOME"), 
    "/.cmdstan/cmdstan-", 
    cmdstan.version,
    sep = ""
  ))

## Step 4: WINDOWS USERS ONLY----

# Execute `mingw32-make install-tbb` in the terminal
rstudioapi::terminalExecute(
  command = "mingw32-make install-tbb",
  workingDir = cmdstanr::cmdstan_path()
)

# Reset the terminal
rstudioapi::terminalKill(id = rstudioapi::terminalList())

# Note: You may need to close R out completely to fully reset
# the terminal so the tbb path is recognized
