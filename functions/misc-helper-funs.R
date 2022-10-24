#' Miscellaneous Project Helper Functions
#' 

# A function to check if required packages are installed during Stan installation
required_pkgs <- function(...) {
  ## Retrieve installed packages
  pkgs <- installed.packages()[, 1]
  
  ## Check if rstudioapi is installed
  if (isTRUE(all.equal(grep("rstudioapi", pkgs), integer(0)))) {
    print("Installing the {rstudioapi} package")
    install.packages("rstudioapi")
  }
  
  ## Check if remotes is installed
  if (isTRUE(all.equal(grep("remotes", pkgs), integer(0)))) {
    print("Installing the {remotes} package")
    install.packages("remotes")
  }
  
  ## Else print a message
  else {
    print("{remotes} and {rstudioapi} packages are already installed")
  }
}

## Function from the brms package to ensure weighted draws sum to n
round_largest_remainder <- function (x) {
  x <- as.numeric(x)
  total <- round(sum(x))
  out <- floor(x)
  diff <- x - out
  J <- order(diff, decreasing = TRUE)
  I <- seq_len(total - floor(sum(out)))
  out[J[I]] <- out[J[I]] + 1
  return(out)
}

## A function for getting the models to pass to to post_prob and bf
get_models_marglik <- function(models_object, criteria_pos) {
  
  # Check that model object is a charachter
  stopifnot("Argument models_object must be a string" = is.character(models_object))
  
  out <- stringr::str_c(
    models_object,
    "[[", 
    criteria_pos, 
    "]]$criteria$marglik"
  ) %>% 
    ## Use glue here to construct the object calls
    glue::glue_collapse(., sep = ", \n")
  
  # Return the models to pass to bridgesampling functions
  return(out)
}
