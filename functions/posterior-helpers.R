#' Post-Estimation Thinning for brms Models
#'
#' @param model A required argument supplying the `brmsfit`
#' model object from which tro obtain the posterior draws
#' 
#' @param thin An integer value specifying the order by
#' which the draws should be thinned. For example, `thin = 10`
#' keeps every 10th draw for each chain.
#'
#' @return A `draws_df` object containing the thinned samples
#' 
#' @export thin_samples
#'
#' @examples
#' 
thin_samples <- function(model, thin) {
  
  ## Check that mode lis an object of class brmsfit
  stopifnot("model argument must be an object of class brmsfit" = brms::is.brmsfit(model))
  
  ## Get the total number of draws
  iters <- brms::niterations(model)
  
  ## Get the total number of chains
  chains <- brms::nchains(model)
  
  ## Build the sequence for thinning the chains
  draws_index <- seq(from = 1, to = iters, by = thin)
  
  ## Extract the posterior draws
  posterior <- posterior::as_draws_df(model) |>
    dplyr::filter(.iteration %in% draws_index)
  
  ## Return the thinned samples
  return(posterior)
}


#' MCMC Summary Diagnostics Wrapper Function
#'
#' @param A `draws` object returned by `posterior::as_draws` or an object
#' coercable to one.
#' 
#' @param ... Additional arguments passed to `posterior::summarise_draws` such
#' as `.cores`.
#' 
#' @export mcmc_diagnostics_summary
#'
#' @examples
#' 
mcmc_diagnostics_summary <- function(draws, ...) {
  
  out <- posterior::summarise_draws(
    draws, 
    posterior::default_summary_measures(), 
    posterior::default_convergence_measures(), 
    posterior::default_mcse_measures(),
    ...
    )
  
  return(out)
}
