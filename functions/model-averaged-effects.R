#' Estimating Model Averaged Marginal Effects and Contrasts for Bayesian Models
#' 
#' This function facilitates the estimation of model averaged marginal effects
#' for Bayesian models and is designed to account for uncertainty in the 
#' process of model selection when estimating average effects and adjusted 
#' probabilities.
#'
#' @aliases model_averaged_ame
#' 
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dbl map2
#' @importFrom dplyr slice_sample mutate group_by summarize ungroup across
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
#' 
#' @param summary An option logical argument indicating whether to return the
#' full draws for each row in `weights` or return the average for each 
#' model-draw pair. Defaults to `FALSE`
#
#' @param ... Additional arguments for future development, currently unused.
#'
#' @return A tibble containing the model averaged posterior draws for the 
#' average marginal effects
#' 
#' @export model_averaged_ame
#' 
model_averaged_ame <- function(x, weights, seed = NULL, summary = FALSE, ...) {
  
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
  
  ## For models with interaction effects
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
  
  # If seed is not null, update the rng seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
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
  
  # Append everything into a single data frame
  out <- bind_rows(out, .id = "postprob_id")
  
  # If summary is true, average over the draws
  if (isTRUE(summary)) {
    
    # Get the number of columns
    nvars <- 2:(ncol(out) - 2)
    
    # Aggregate by model-draw pairs
    out <- out |>
      group_by(.draw, .model) |>
      summarize(across(all_of(nvars), ~ mean(.x))) |>
      ungroup()
    
  }
  
  ## Return just the final object
  return(out)
  
}


