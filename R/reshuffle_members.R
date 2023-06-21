#' Reshuffle ensemble members using template
#'
#' This function reshuffles the members of a post-processed ensemble forecast
#' using a template to restore a meaningful dependence structure.
#'
#' @param forecast is a matrix where the columns correspond to members with the
#' unrealistic dependence structure
#' @param template is a matrix where the columns correspond to the template for
#' dependence structure
#'
#' @return a matrix where the members have been reshuffled according to the
#' dependence template
#'
#' @details
#' For ensemble members that have been sampled from a post-processed forecast
#' distribution the members need to be reshuffled to restore a meaningful
#' dependence structure in space, time or both.
#'
#' This function uses a template to reshuffle the sampled ensemble members. There
#' are many option for template selection. In the Schaake shuffle and its variants
#' the template uses past observations. In Empirical Coupla Coupling, the
#' dependence template is inherited from the raw ensemble forecast.
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
#' eg_forecast = matrix(rnorm(d*m), nrow = d)
#'
#' eg_template = matrix(rnorm(d*m), nrow = d)
#'
#' forecast_with_dependence = reshuffle_members(eg_forecast, eg_template)
#'
#' # Checking the forecast was shuffled correctly using the template
#' order(eg_forecast[1,])
#' order(eg_template[1,])
#' order(forecast_with_dependence[1,])
#'
#' # Checking the forecast still contains the same members after shuffling
#' sort(eg_forecast[1,])
#' sort(forecast_with_dependence[1,])
#'
#'@export
#'
reshuffle_members <- function(forecast, template){

  # error handling
  {
  if(any(is.na(forecast)))
    stop("forecast should not contain missing values")

  if(any(is.na(template)))
    stop("template should not contain missing values")

  if(!all(dim(forecast) == dim(template)))
    stop("the dimensions of forecast and template should match")

  }

  sorted_forecast = sort_members(forecast)
  ranked_template = rank_members(template)

  forecast_with_dependence = reorder_members(sorted_forecast, ranked_template)

  return(forecast_with_dependence)

}
