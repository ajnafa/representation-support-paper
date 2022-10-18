#' Estimating Stacked Marginal Effects and Contrasts for Bayesian Models
#' 
#' This function facilitates the estimation of stacked averaged marginal 
#' effects for Bayesian models and is designed to account for uncertainty in 
#' the process of model selection when estimating average effects and adjusted 
#' probabilities.
#'
#' @aliases stacked_ame
#' 
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dbl map2
#' @importFrom dplyr slice_sample mutate
#'
#' @param x A list object returned by `brmsmargins::brmsmargins`. The object
#' must contain at a minimum two matrices, one called the `Posterior` and the 
#' other called `Contrasts`. See ?brmsmargins::brmsmargins for additional 
#' details.
#'
#' @param weights A vector of stacking weights such as that returned by the
#' `stacking_weights` function. `weights` must be the same length as `x`.
#' 
#' @param seed An integer value to use for the random number seed to ensure the 
#' index values for the random draws are locally reproducible.
#
#' @param ... Additional arguments for future development, currently unused.
#'
#' @return A tibble containing the stacked posterior draws for the average
#' marginal effects
#' 
#' @export stacked_ame
#' 
stacked_ame <- function(x, weights, seed, ...) {
  
  ## Check that weights is a numeric vector and x is a list
  stopifnot(exprs = {
    is.vector(weights) && is.numeric(weights)
    is.list(x)
  })
  
  ## Get the number of draws used in the marginal effects estimation
  ndraws <- purrr::map_dbl(
    .x = x, 
    .f = ~ nrow(.x$Posterior)
  )
  
  ## Check that all of the matrices have the same number of draws
  for (i in seq_along(ndraws)) {
    x_rows <- all.equal(as.numeric(ndraws[1]), as.numeric(ndraws[i]))
    stopifnot("All matrices must have the same number of draws" = x_rows, x_rows)
  }
  
  ## Construct a vector of draws
  weighted_draws <- round_largest_remainder(weights * ndraws[1])
  names(weighted_draws) <- names(weights)
  
  ## For models with only main effects
  if (dim(x[[1]]$Contrasts)[2] == 1) {
    
    ## Build the tibble for each model
    marginal_draws_df <- purrr::map2(
      .x = x,
      .y = names(x),
      .f = ~ tibble::tibble(
        .ame = .x$Contrasts[, 1],
        .draw = 1:nrow(.x$Posterior),
        .model = .y
      )
    )
    
  }
  
  ## For models with interactions effects
  else if (dim(x[[1]]$Contrasts)[2] >= 2) {
    
    ## Build the tibble for each model
    marginal_draws_df <- purrr::map2(
      .x = x,
      .y = names(x),
      .f = ~ tibble::as_tibble(.x$Contrasts) |>
        dplyr::mutate(
          .draw = 1:nrow(.x$Posterior),
          .model = .y,
          .before = 1
        )
    )
  }
  
  ## Update the rng seed to ensure reproducibility
  set.seed(seed)
  
  # Initialize a list to store the result in
  out <- list()
  
  for (i in seq_along(weighted_draws)) {
    
    if (weighted_draws[i] >= 1) {
      
      out[[i]] <- dplyr::slice_sample(marginal_draws_df[[i]], n = weighted_draws[i])
      
    }
  }
  
  ## Append the draws into a data frame
  out <- dplyr::bind_rows(out, .id = ".loo_id")
  
  ## Add Attributes to the matrix
  attr(out, which = "ndraws") <- weighted_draws
  attr(out, which = "weights") <- weights
  
  ## Return just the final object
  return(out)
}
