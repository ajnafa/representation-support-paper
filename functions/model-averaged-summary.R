# Data frame method for model averaged posteriors
model_averaged_summary <- function(x,
                                   model_info,
                                   conf_level = 0.95,
                                   parameters = "^b_|^sd_|^cor_|^sigma|^rescor|^phi|^ar",
                                   ...) {
  
  # Extract the parameters from the pattern
  draws <- posterior::as_draws_df(x) |>
    dplyr::select(dplyr::matches(parameters), '.chain', '.iteration', '.draw')
  
  # Check the prob intervals to use
  prob_lower <- (1-conf_level)/2
  prob_upper <- 1-prob_lower
  
  # Probability of Direction
  prob_dir <- bayestestR::pd(draws)
  
  # Summarize the draws
  draws <- posterior::summarize_draws(
    draws, 
    ~posterior::quantile2(.x, probs = c(prob_lower, prob_upper)), 
    posterior::default_summary_measures()[1:4],
    posterior::default_convergence_measures()
  )
  
  ## Build the data frame for modelsummary
  out_tidy <- data.frame(
    term = draws$variable,
    group = ifelse(grepl("^b_", draws$variable), "fixed", "random"),
    estimate = draws$median,
    conf.level = rep(conf_level, length.out = nrow(draws)),
    conf.low = unlist(draws[, 2], use.names = FALSE),
    conf.high = unlist(draws[, 3], use.names = FALSE),
    std.error = draws$sd,
    rhat = draws$rhat,
    ess_bulk = draws$ess_bulk,
    ess_tail = draws$ess_tail,
    pd = prob_dir$pd
  )
  
  # Create a list with the two items
  out <- list(tidy = out_tidy, glance = model_info)
  class(out) <- "modelsummary_list"
  
  # Return the data frame object
  return(out)
}

