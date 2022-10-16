#' Estimating Model Averaged Marginal Effects and Contrasts for Bayesian Models
#' 
#' This function facilitates the estimation of model averaged marginal effects
#' for Bayesian models and is designed to account for uncertainty in the 
#' process of model selection when estimating average effects and adjusted 
#' probabilities.
#'
#' @aliases model_averaged_ame
#' 
#' @importFrom tibble tibble
#' @importFrom purrr map_dbl map2
#' @importFrom dplyr slice_sample
#'
#' @param x A list object returned by `brmsmargins::brmsmargins`. The object
#' must contain at a minimum two matrices, one called the `Posterior` and the 
#' other called `Contrasts`. See ?brmsmargins::brmsmargins for additional 
#' details.
#'
#' @param weights An \eqn{n \times m} matrix of posterior probability weights 
#' such as that returned by `bridgesampling::post_prob`.
#' 
#' @param seed An integer value to use for the random number seed to ensure the 
#' index values for the random draws are locally reproducible.
#
#' @param ... Additional arguments for future development, currently unused.
#'
#' @return A tibble containing the model averaged posterior draws for the 
#' average marginal effects
#' 
#' @export model_averaged_ame
#' 
model_averaged_ame <- function(x, weights, seed, ...) {
  
  ## Check that weights is a matrix and x is a list
  stopifnot(exprs = {
    is.matrix(weights)
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
  
  ## Construct a matrix of draws
  weighted_draws <- apply(
    weights * ndraws[1], 
    MARGIN = 1, 
    round_largest_remainder
  )
  
  ## Build the tibble for each model
  marginal_draws_df <- purrr::map2(
    .x = x,
    .y = names(x),
    .f = ~ tibble::tibble(
      .pred_low = .x$Posterior[, 1],
      .pred_hi = .x$Posterior[, 2],
      .ame = .x$Contrasts[, 1],
      .draw = 1:nrow(.x$Posterior),
      .model = .y
    )
  )
  
  ## Update the rng seed to ensure reproducibility
  set.seed(seed)
  
  # Initialize a list to store the result in
  out <- list()
  
  for (j in 1:ncol(weighted_draws)) {
    
    out[[j]] <- list()
    
    for (i in 1:nrow(weighted_draws)) {
      
      if (weighted_draws[i, j] >= 1) {
        
        .ndraws <- weighted_draws[i, j]
        
        out[[j]][[i]] <- dplyr::slice_sample(marginal_draws_df[[i]], n = .ndraws)
        
      }
    }
    
    ## Append everything into a single list
    out[[j]] <- do.call(rbind, out[[j]])
  }
  
  ## Append the models into a data frame
  out <- dplyr::bind_rows(out, .id = ".postprob")
  
  ## Return just the final object
  return(out)
  
}


