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