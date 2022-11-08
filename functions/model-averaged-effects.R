#' Estimating Model Averaged Marginal Effects and Contrasts for Bayesian Models
#' 
#' This function facilitates the estimation of model averaged marginal effects
#' for Bayesian models and is designed to account for uncertainty in the 
#' process of model specification when estimating average marginal effects and 
#' probability contrasts.
#'
#' @aliases model_averaged_ame
#' 
#' @import data.table
#' @importFrom purrr map_dbl
#'
#' @param x A list object returned by `brmsmargins::brmsmargins`. The object
#' must contain at a minimum two matrices, one called the `Posterior` and the 
#' other called `Contrasts`. See ?brmsmargins::brmsmargins for additional 
#' details.
#'
#' @param weights An \eqn{n \times m} matrix of posterior probability weights 
#' such as that returned by `bridgesampling::post_prob`.
#' 
#' @param summary A logical argument indicating whether to return the full 
#' draws for each row in `weights` or return the average for each model-draw 
#' pair. Defaults to `FALSE`
#
#' @param ... Additional arguments for future development, currently unused.
#'
#' @return Either a datatable list of datatables containing the model averaged 
#' posterior draws for the average marginal effects depending on the input.
#' 
#' @export model_averaged_ame
#' 
model_averaged_ame <- function(x, weights, summary = FALSE, ...) {
  
  # Validate weights argument and get draws
  ndraws <- validate_weight_args(x, weights)
  
  # Get the number of models in ther set
  models <- ncol(weights)
  
  ## For models with only main effects
  if (ncol(x[[1]]$Contrasts) == 1) {
    # Initialize a list to store each matrix of draws in
    draws <- list()
    
    for (i in 1:nrow(weights)) {
      # Build a matrix of draws by model for each set of weights
      draws[[i]] <- matrix(NA_real_, nrow = ndraws, ncol = (models + 2))
      
      # Looping over models to calculate p(y | M) * AME
      for (m in 1:models) {
        # Fill in the elements in the matrix
        draws[[i]][, m] <- as.numeric(x[[m]]$Contrasts * weights[i, m])
      }
      
      # Add draw and repetion identifiers
      draws[[i]][, (models + 1)] <- seq(1, ndraws, 1)
      draws[[i]][, (models + 2)] <- rep(i, length.out = ndraws)
    }
    
    # Bind everything into one data frame
    draws <- do.call("rbind", draws)
    
    # Build the final output data frame
    draws <- data.table::data.table(
      weighted_draws = rowSums(draws[, 1:models]),
      .draw = draws[, (models + 1)],
      .bridgerep = draws[, (models + 2)]
    )
    
    if (isTRUE(summary)) {
      draws = draws[, .(bmame = mean(weighted_draws),
                        bmame_lower = quantile(weighted_draws, probs = 0.025),
                        bmame_upper = quantile(weighted_draws, probs = 0.975)), 
                    by = .draw]
    }
  }
  
  ## For models with interaction effects
  if (ncol(x[[1]]$Contrasts) >= 2) {
    # Initialize a list to store each matrix of draws in
    draws <- list()
    
    for (l in 1:ncol(x[[1]]$Contrasts)) {
      # Nested list for each contrast
      draws[[l]] <- list()
      
      for (i in 1:nrow(weights)) {
        # Build a matrix of draws by model for each set of weights
        draws[[l]][[i]] <- matrix(NA_real_, nrow = ndraws, ncol = (models + 2))
        
        # Looping over models to calculate p(y | M) * AME
        for (m in 1:models) {
          # Fill in the elements in the matrix
          draws[[l]][[i]][, m] <- as.numeric(x[[m]]$Contrasts[, l] * weights[i, m])
        }
        
        # Add draw and repetion identifiers
        draws[[l]][[i]][, (models + 1)] <- seq(1, ndraws, 1)
        draws[[l]][[i]][, (models + 2)] <- rep(i, length.out = ndraws)
      }
      
      # Bind everything into one data frame
      draws[[l]] <- do.call("rbind", draws[[l]])
      
      # Build the final output data frame
      draws[[l]] <- data.table::data.table(
        weighted_draws = rowSums(draws[[l]][, 1:models]),
        .draw = draws[[l]][, (models + 1)],
        .bridgerep = draws[[l]][, (models + 2)]
      )
      
      if (isTRUE(summary)) {
        draws[[l]] = draws[[l]][, .(bmame = mean(weighted_draws),
                                    bmame_lower = quantile(weighted_draws, probs = 0.025),
                                    bmame_upper = quantile(weighted_draws, probs = 0.975)), 
                                by = .draw]
      }
    }
    # Apply the name of the contrast
    names(draws) <- colnames(x[[1]]$Contrasts)
  }
  
  # Return the output
  return(draws)
}

# Helper function for validating the weights
validate_weight_args <- function(x, weights, ...) {
  ## Check that weights is a matrix and x is a list
  stopifnot(exprs = {
    is.matrix(weights)
    is.list(x)
    length(x) == ncol(weights)
  })
  
  ## Get the number of draws used in the marginal effects estimation
  ndraws <- map_dbl(
    .x = x, 
    .f = ~ nrow(.x$Posterior)
  )
  
  ## Check that all of the matrices have the same number of draws
  for (i in seq_along(ndraws)) {
    x_rows <- all.equal(as.numeric(ndraws[1]), as.numeric(ndraws[i]))
    stopifnot("All matrices must have the same number of draws" = x_rows, x_rows)
  }
  
  ## We only need a single number here
  ndraws <- as.integer(ndraws[1])
  
  ## Return the total draws
  return(ndraws)
}
