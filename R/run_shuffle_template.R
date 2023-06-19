#' Shuffles a post-processed forecast to restore dependence
#'
#' @param forecast is a matrix where the columns correspond to multivariate forecasts.
#' @param template is a matrix where the columns correspond to the template for
#' reshuffling
#'
#' @return a matrix where the post-processed forecast with dependence based on
#' the template
#'
#' @details
#' For a forecast that has been sampled from a post-processed forecast distribution
#' the members need to be reshuffled to restore a meaningful dependence structure
#' in space, time or both.
#'
#' This function uses a template to reshuffle the sample ensemble members. The
#' template can be one of climatology, as in the Schaake Shuffle, or can be from
#' the raw ensemble forecast, as in Empirical Copula Coupling.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#'
#' Clark, Martyn, et al. "The Schaake shuffle: A method for reconstructing
#' space–time variability in forecasted precipitation and temperature fields."
#' Journal of Hydrometeorology 5.1 (2004): 243-262.
#'
#' Schefzik, Roman, Thordis L. Thorarinsdottir, and Tilmann Gneiting.
#' "Uncertainty quantification in complex simulation models using ensemble
#' copula coupling." Statistical science 28.4 (2013): 616-640.
#'
#' Hu, Yiming, et al. "A stratified sampling approach for improved sampling
#' from a calibrated ensemble forecast distribution." Journal of Hydrometeorology
#' 17.9 (2016): 2405-2417.
#'
#' @examples
#'
#' d <- 3  # number of dimensions (space x time)
#' m <- 5  # number of members (sampled from the post-process forecast distribution)
#'
#' set.seed(1)
#' eg_forecast = matrix(rnorm(d*m), nrow = 1)
#'
#' eg_template = matrix(rnorm(d*m), nrow = 1)
#'
#' forecast_with_dependence = run_shuffle_template(eg_forecast, eg_template)
#'
#' # Checking the eg_forecast inherited the order from the dependence eg_template
#' order(eg_forecast)
#' order(eg_template)
#' order(forecast_with_dependence)
#'
#' # Checking the eg_forecast still contains the same members after shuffling
#' sort(eg_forecast)
#' sort(forecast_with_dependence)
#'
#' Add in the example with the data here
#'
#'@export
run_shuffle_template <- function(forecast, template){

  if(any(is.na(forecast))) stop("forecast should not contain missing values")
  if(any(is.na(template))) stop("template should not contain missing values")
  if(!all(dim(forecast) == dim(template))) stop("the dimensions of forecast and template should match")


  sorted_forecast = sort_members(forecast)
  ranked_template = rank_members(template)

  forecast_with_dependence = reorder_members(sorted_forecast, ranked_template)

    return(forecast_with_dependence)

}
