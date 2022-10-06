#' Marginal and Conditional Bayes R-Square for Bayesian Models
#'
#' This function returns estimates for both the marginal and conditional Bayes
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} for multilevel models fit with
#' \pkg{brms} and stores them in the original model object.
#'
#' @aliases Bayesian_R2
#'
#' @param model A required argument supplying the `brmsfit`
#' model object for which to calculate Bayes 
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}}.
#'
#' @param marginal Logical argument indicating whether to calculate
#' the unconditional Bayes \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}}.
#' Defaults to `TRUE`.
#'
#' @param conditional Logical argument indicating whether to calculate
#' Bayes \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} conditional on the
#' random effects. Defaults to `FALSE`.
#'
#' @param effects A one-sided formula specifying the structure of
#' the random effects for the conditional Bayes
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}}. Defaults to `NULL`,
#' which conditions on the full random effects structure.
#'
#' @param save Logical argument indicating whether the results should be
#' saved to the local model file. Defaults to `FALSE` and is
#' currently only supported for `brmsfit` objects.
#'
#' @param overwrite Logical argument indicating whether to overwrite any
#' existing `Bayesian_R2` result saved in `model$criteria`. Defaults to
#' `FALSE`, which for `brmsfit` objects will simply return
#' the original model object supplied.
#'
#' @param ... Additional arguments to be passed to either
#' `brms::bayes_R2` or `rstanarm::bayes_R2` depending on
#' the model class.
#'
#' @return The function returns the original model object with the
#' results stored in the `model$criteria$Bayesian_R2`.
#'
#' @details For additional details on the underlying calls see
#' `?brms::bayes_R2`. A technical discussion of the Bayes 
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} statistic
#' can be found in Gelman et al. (2019).
#'
#'
#' @importFrom brms bayes_R2
#' @export Bayesian_R2
#' @export
Bayesian_R2 <- function(model,
                        marginal = TRUE,
                        conditional = FALSE,
                        effects = NULL,
                        save = FALSE,
                        overwrite = FALSE,
                        ...) {

  ## Initiate a list to store the results in
  out <- list()

  ## Check the class of the model object
  .mod_type <- class(model)

  ## Check that at least one of the arguments is specified
  .check_args <- ifelse(isTRUE(marginal) || isTRUE(conditional), 1, 0)

  ## Otherwise, return an error
  if (.check_args < 1) {
    stop("At least one of the 'conditional' or 'marginal' arguments must be TRUE")
  }

  ## If model is a brmsfit object call `brms::bayes_R2`
  else if (!is.na(match(.mod_type, "brmsfit"))) {

    ### Check if bayes_R2 is already stored to avoid redundant computation
    R2 <- .check_criteria(model, criterion = "bayes_R2", overwrite = overwrite)

    ### If bayes_R2 is already stored and overwrite is false, just return the existing object
    if (!is.null(R2)) {
      model$criteria$bayes_R2 <- R2
      return(model)
    }

    ## Otherwise, evaluate the arguments to the function
    else {

      ### Get the file path
      model.file <- model$file

      ### Marginal Bayes R2 for brmsfit objects
      if (isTRUE(marginal)) {

        ### Return the Marginal Bayes R2 to the temporary list object
        out$marginal_R2 <- brms::bayes_R2(model, re_formula = NA, ...)

        ### Assign a names attribute
        attr(out$marginal_R2, "names") <- "Marginal Bayes R2"

      }

      ### Conditional Bayes R2 for brmsfit objects
      if (isTRUE(conditional)) {

        ### Add Conditional Bayes R2 to the temporary list object
        out$conditional_R2 <- brms::bayes_R2(model, re_formula = effects, ...)

        ### Assign a names attribute
        attr(out$conditional_R2, "names") <- "Conditional Bayes R2"

      }

      ## Add the result to the original model object
      model$criteria$bayes_R2 <- out

      ## If save is true, save the model with the added criteria to the original file
      if (isTRUE(save)) {

        # Write using high file compression
        saveRDS(
          model,
          file = model.file,
          compress = TRUE
        )

        # Print the location the model was saved to
        print(paste("Saving the updated model object to ", model.file, sep = ""))
      }

      ## Return the updated model
      return(model)
    }
  }

  ## Return and error if model is not of class brmsfit
  else {
    stop("This function only supports model objects of class brmsfit")
  }
}

# Function to check whether model criterion is already stored
.check_criteria <- function(x, criterion, overwrite) {

  ## Check the class of the model object
  stopifnot(class(x) == "brmsfit")

  ## Retrieve the names of the already stored criteria
  criteria <- names(x$criteria)

  ## Position of the stored criteria
  criteria_pos <- match(criterion, criteria)

  ## If overwrite is FALSE and criteria is found, return existing criteria
  if (!is.na(criteria_pos) & overwrite == FALSE) {
    return(x$criteria[[criteria_pos]])
  }
}
